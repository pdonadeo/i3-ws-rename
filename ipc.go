package main

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"strings"
	"sync"
)

const magicString = "i3-ipc"
const eventBit = uint32(1 << 31)

const (
	msgCommand   = uint32(0)
	msgSubscribe = uint32(2)
	msgTree      = uint32(4)
)

// NodeID is stored as a string but arrives as an integer in i3/Sway JSON.
type NodeID string

func (n *NodeID) UnmarshalJSON(data []byte) error {
	var i int64
	if err := json.Unmarshal(data, &i); err == nil {
		*n = NodeID(fmt.Sprintf("%d", i))
		return nil
	}
	var s string
	if err := json.Unmarshal(data, &s); err != nil {
		return err
	}
	*n = NodeID(s)
	return nil
}

type Rect struct {
	X      int `json:"x"`
	Y      int `json:"y"`
	Width  int `json:"width"`
	Height int `json:"height"`
}

type WindowProperties struct {
	Class_       *string `json:"class"`
	Instance     *string `json:"instance"`
	Title        *string `json:"title"`
	TransientFor *int    `json:"transient_for"`
	WindowRole   *string `json:"window_role"`
}

type Node struct {
	Nodes          []Node            `json:"nodes"`
	FloatingNodes  []Node            `json:"floating_nodes"`
	ID             NodeID            `json:"id"`
	Name           *string           `json:"name"`
	Num            *int              `json:"num"`
	NodeType       string            `json:"type"`
	Border         string            `json:"border"`
	CurrBorderW    int               `json:"current_border_width"`
	Layout         string            `json:"layout"`
	Percent        *float64          `json:"percent"`
	Rect           Rect              `json:"rect"`
	WindowRect     Rect              `json:"window_rect"`
	DecoRect       Rect              `json:"deco_rect"`
	Geometry       Rect              `json:"geometry"`
	Window         *int              `json:"window"`
	WindowProps    *WindowProperties `json:"window_properties"`
	Urgent         bool              `json:"urgent"`
	Focused        bool              `json:"focused"`
	Focus          []NodeID          `json:"focus"`
	FullscreenMode int               `json:"fullscreen_mode"`
	AppID          *string           `json:"app_id"`
}

type CommandOutcome struct {
	Success bool    `json:"success"`
	Error   *string `json:"error"`
}

// Event types

type WindowChange int

const (
	WinNew WindowChange = iota
	WinClose
	WinFocus
	WinTitle
	WinFullscreenMode
	WinMove
	WinFloating
	WinUrgent
	WinMark
	WinUnknown
)

type WindowEventInfo struct {
	Change    WindowChange
	Container Node
}

type ShutdownReason int

const (
	ShutdownNone    ShutdownReason = iota // no shutdown event received yet
	ShutdownRestart                       // i3/Sway is restarting
	ShutdownExit                          // i3/Sway is exiting
)

type Event interface{ isEvent() }

type WindowEvent struct{ Info WindowEventInfo }
type ShutdownEvent struct{ Reason ShutdownReason }

func (WindowEvent) isEvent()   {}
func (ShutdownEvent) isEvent() {}

type rawMsg struct {
	ty      uint32
	payload []byte
}

// Conn is a connection to i3/Sway.
type Conn struct {
	sock    net.Conn
	writeMu sync.Mutex
	eventCh chan Event
	replyCh chan rawMsg
}

func getSocketPath() (string, error) {
	if s := os.Getenv("I3SOCK"); s != "" {
		return s, nil
	}
	if s := os.Getenv("SWAYSOCK"); s != "" {
		return s, nil
	}
	if out, err := exec.Command("i3", "--get-socketpath").Output(); err == nil {
		return strings.TrimSpace(string(out)), nil
	}
	if out, err := exec.Command("sway", "--get-socketpath").Output(); err == nil {
		return strings.TrimSpace(string(out)), nil
	}
	return "", fmt.Errorf("no i3/Sway socket found (I3SOCK/SWAYSOCK not set)")
}

func Connect() (*Conn, error) {
	path, err := getSocketPath()
	if err != nil {
		return nil, err
	}
	sock, err := net.Dial("unix", path)
	if err != nil {
		return nil, fmt.Errorf("connect to %s: %w", path, err)
	}
	c := &Conn{
		sock:    sock,
		eventCh: make(chan Event, 64),
		replyCh: make(chan rawMsg, 8),
	}
	go c.readLoop()
	return c, nil
}

func (c *Conn) Close() { _ = c.sock.Close() }

func (c *Conn) readLoop() {
	defer close(c.eventCh)
	for {
		ty, payload, err := c.readRawMsg()
		if err != nil {
			return
		}
		if ty&eventBit != 0 {
			ev, err := parseEvent(ty, payload)
			if err != nil {
				continue
			}
			c.eventCh <- ev
		} else {
			select {
			case c.replyCh <- rawMsg{ty, payload}:
			default:
			}
		}
	}
}

func (c *Conn) readRawMsg() (uint32, []byte, error) {
	header := make([]byte, 14)
	if _, err := io.ReadFull(c.sock, header); err != nil {
		return 0, nil, err
	}
	if string(header[:6]) != magicString {
		return 0, nil, fmt.Errorf("bad magic string: %q", header[:6])
	}
	length := binary.LittleEndian.Uint32(header[6:10])
	ty := binary.LittleEndian.Uint32(header[10:14])
	payload := make([]byte, length)
	if _, err := io.ReadFull(c.sock, payload); err != nil {
		return 0, nil, err
	}
	return ty, payload, nil
}

func (c *Conn) writeRawMsg(ty uint32, payload string) error {
	buf := make([]byte, 14+len(payload))
	copy(buf[:6], magicString)
	binary.LittleEndian.PutUint32(buf[6:10], uint32(len(payload)))
	binary.LittleEndian.PutUint32(buf[10:14], ty)
	copy(buf[14:], payload)
	c.writeMu.Lock()
	defer c.writeMu.Unlock()
	_, err := c.sock.Write(buf)
	return err
}

// sendCmd sends a message and waits for the reply with the matching type.
// Only safe when a single command is outstanding at a time.
func (c *Conn) sendCmd(ty uint32, payload string) ([]byte, error) {
	if err := c.writeRawMsg(ty, payload); err != nil {
		return nil, err
	}
	for msg := range c.replyCh {
		if msg.ty == ty {
			return msg.payload, nil
		}
		// discard replies of the wrong type (shouldn't happen in practice)
	}
	return nil, fmt.Errorf("connection closed while waiting for reply")
}

func (c *Conn) Subscribe(events []string) error {
	payload, _ := json.Marshal(events)
	data, err := c.sendCmd(msgSubscribe, string(payload))
	if err != nil {
		return err
	}
	var outcome CommandOutcome
	if err := json.Unmarshal(data, &outcome); err != nil {
		return err
	}
	if !outcome.Success {
		if outcome.Error != nil {
			return fmt.Errorf("subscribe failed: %s", *outcome.Error)
		}
		return fmt.Errorf("subscribe failed")
	}
	return nil
}

func (c *Conn) GetTree() (Node, error) {
	data, err := c.sendCmd(msgTree, "")
	if err != nil {
		return Node{}, err
	}
	var node Node
	if err := json.Unmarshal(data, &node); err != nil {
		return Node{}, err
	}
	return node, nil
}

func (c *Conn) Command(cmd string) ([]CommandOutcome, error) {
	data, err := c.sendCmd(msgCommand, cmd)
	if err != nil {
		return nil, err
	}
	var outcomes []CommandOutcome
	if err := json.Unmarshal(data, &outcomes); err != nil {
		return nil, err
	}
	return outcomes, nil
}

func parseEvent(ty uint32, payload []byte) (Event, error) {
	switch ty & ^eventBit {
	case 3: // window
		var raw struct {
			Change    string `json:"change"`
			Container Node   `json:"container"`
		}
		if err := json.Unmarshal(payload, &raw); err != nil {
			return nil, err
		}
		changeMap := map[string]WindowChange{
			"new": WinNew, "close": WinClose, "focus": WinFocus,
			"title": WinTitle, "fullscreen_mode": WinFullscreenMode,
			"move": WinMove, "floating": WinFloating,
			"urgent": WinUrgent, "mark": WinMark,
		}
		change, ok := changeMap[raw.Change]
		if !ok {
			change = WinUnknown
		}
		return WindowEvent{Info: WindowEventInfo{Change: change, Container: raw.Container}}, nil

	case 6: // shutdown
		var raw struct {
			Change string `json:"change"`
		}
		if err := json.Unmarshal(payload, &raw); err != nil {
			return nil, err
		}
		switch raw.Change {
		case "restart":
			return ShutdownEvent{Reason: ShutdownRestart}, nil
		case "exit":
			return ShutdownEvent{Reason: ShutdownExit}, nil
		default:
			return nil, fmt.Errorf("unknown shutdown reason: %s", raw.Change)
		}

	default:
		return nil, fmt.Errorf("unhandled event type %d", ty&^eventBit)
	}
}
