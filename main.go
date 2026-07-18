package main

import (
	"context"
	"flag"
	"fmt"
	"log/slog"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"
)

// fallbackIcon is shown when no config entry matches a window.
const fallbackIcon = "▨"

// version is set at build time via -ldflags "-X main.version=...".
var version = "dev"

// daemonEnvVar is set in the environment of the re-exec'd daemon child so it
// knows not to daemonize again.
const daemonEnvVar = "_WS_RENAME_DAEMON"

// ── Logging ──────────────────────────────────────────────────────────────────

type fileHandler struct {
	mu    sync.Mutex
	file  *os.File
	level slog.Level
}

func newFileHandler(path string, level slog.Level) (*fileHandler, error) {
	f, err := os.OpenFile(path, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0o644)
	if err != nil {
		return nil, err
	}
	return &fileHandler{file: f, level: level}, nil
}

func (h *fileHandler) Enabled(_ context.Context, l slog.Level) bool { return l >= h.level }
func (h *fileHandler) WithAttrs(_ []slog.Attr) slog.Handler         { return h }
func (h *fileHandler) WithGroup(_ string) slog.Handler              { return h }

func (h *fileHandler) Handle(_ context.Context, r slog.Record) error {
	t := r.Time
	ts := fmt.Sprintf("%04d-%02d-%02d %02d:%02d:%02d.%03d",
		t.Year(), t.Month(), t.Day(), t.Hour(), t.Minute(), t.Second(),
		t.Nanosecond()/1e6)

	levelStr := "[INFO]"
	switch {
	case r.Level >= slog.LevelError:
		levelStr = "[ERROR]"
	case r.Level >= slog.LevelWarn:
		levelStr = "[WARNING]"
	case r.Level < slog.LevelInfo:
		levelStr = "[DEBUG]"
	}

	var attrs []string
	r.Attrs(func(a slog.Attr) bool {
		attrs = append(attrs, fmt.Sprintf("%s=%v", a.Key, a.Value.Any()))
		return true
	})

	line := fmt.Sprintf("%s %s %s", ts, levelStr, r.Message)
	if len(attrs) > 0 {
		line += " " + strings.Join(attrs, " ")
	}
	line += "\n"

	h.mu.Lock()
	defer h.mu.Unlock()
	_, err := h.file.WriteString(line)
	return err
}

// ── Daemonization ─────────────────────────────────────────────────────────────

func daemonize(wd string) {
	exe, err := os.Executable()
	if err != nil {
		fmt.Fprintf(os.Stderr, "daemonize: %v\n", err)
		os.Exit(1)
	}
	devNull, err := os.OpenFile("/dev/null", os.O_RDWR, 0)
	if err != nil {
		fmt.Fprintf(os.Stderr, "daemonize: open /dev/null: %v\n", err)
		os.Exit(1)
	}
	null := devNull.Fd()

	env := append(os.Environ(), daemonEnvVar+"=1")
	pid, _, errno := syscall.RawSyscall(syscall.SYS_FORK, 0, 0, 0)
	if errno != 0 {
		fmt.Fprintf(os.Stderr, "daemonize: fork: %v\n", errno)
		os.Exit(1)
	}
	if pid != 0 {
		os.Exit(0) // parent exits
	}
	// child: become session leader (ignore error, mirrors OCaml's `ignore (Unix.setsid ())`)
	_, _ = syscall.Setsid()

	_, err = syscall.ForkExec(exe, os.Args, &syscall.ProcAttr{
		Dir:   wd,
		Env:   env,
		Files: []uintptr{null, null, null},
		Sys:   &syscall.SysProcAttr{Setsid: true},
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "daemonize: forkexec: %v\n", err)
		os.Exit(1)
	}
	os.Exit(0)
}

// ── Tree traversal ────────────────────────────────────────────────────────────

func traverseDeepFirst(f func(n *Node, parent *Node, depth int), n *Node, parent *Node, depth int) {
	f(n, parent, depth)
	for i := range n.Nodes {
		traverseDeepFirst(f, &n.Nodes[i], n, depth+1)
	}
	for i := range n.FloatingNodes {
		traverseDeepFirst(f, &n.FloatingNodes[i], n, depth+1)
	}
}

func getWorkspaceNodes(root *Node) []*Node {
	var ws []*Node
	traverseDeepFirst(func(n *Node, _ *Node, _ int) {
		if n.NodeType == "workspace" && n.Num != nil && *n.Num > 0 {
			ws = append(ws, n)
		}
	}, root, nil, 0)
	return ws
}

func extractLeaves(root *Node) []*Node {
	var leaves []*Node
	traverseDeepFirst(func(n *Node, _ *Node, _ int) {
		if len(n.Nodes) == 0 && len(n.FloatingNodes) == 0 && n.NodeType != "workspace" {
			leaves = append(leaves, n)
		}
	}, root, nil, 0)
	return leaves
}

// ── Icon resolution ───────────────────────────────────────────────────────────

func stringOfNode(conf []IconConf, n *Node) string {
	if n.WindowProps == nil {
		// Wayland / Sway path: match by app_id + optional name regex
		if n.AppID == nil {
			return fallbackIcon
		}
		appID := strings.ToLower(*n.AppID)
		var byAppID []IconConf
		for _, r := range conf {
			if r.AppID != nil && *r.AppID == appID {
				byAppID = append(byAppID, r)
			}
		}
		if n.Name != nil {
			nodeName := strings.ToLower(*n.Name)
			// Take first record where name is nil (matches any) or regex matches.
			for _, r := range byAppID {
				if r.Name == nil {
					return r.Icon
				}
				re, err := regexp.Compile(*r.Name)
				if err == nil && re.MatchString(nodeName) {
					return r.Icon
				}
			}
			return fallbackIcon
		}
		for _, r := range byAppID {
			if r.Name == nil {
				return r.Icon
			}
		}
		return fallbackIcon
	}

	// X11 / i3 path: match by window class and/or instance
	wp := n.WindowProps
	switch {
	case wp.Class_ == nil && wp.Instance == nil:
		return fallbackIcon
	case wp.Class_ == nil:
		if icon := searchInstance(conf, *wp.Instance); icon != nil {
			return *icon
		}
		return strings.ToLower(*wp.Instance)
	case wp.Instance == nil:
		if icon := searchClass(conf, *wp.Class_); icon != nil {
			return *icon
		}
		return strings.ToLower(*wp.Class_)
	default:
		if icon := searchClassInstance(conf, *wp.Class_, *wp.Instance); icon != nil {
			return *icon
		}
		if icon := searchClass(conf, *wp.Class_); icon != nil {
			return *icon
		}
		return strings.ToLower(*wp.Class_)
	}
}

// removeDups removes consecutive duplicates (mirrors OCaml remove_dups).
func removeDups(xs []string) []string {
	if len(xs) == 0 {
		return xs
	}
	out := []string{xs[0]}
	for _, x := range xs[1:] {
		if x != out[len(out)-1] {
			out = append(out, x)
		}
	}
	return out
}

// ── Workspace renaming ────────────────────────────────────────────────────────

func renameWorkspace(conf []IconConf, conn *Conn, ws *Node, log *slog.Logger) error {
	wsName := "N/A"
	if ws.Name != nil {
		wsName = *ws.Name
	}
	wsNum := 0
	if ws.Num != nil {
		wsNum = *ws.Num
	}

	leaves := extractLeaves(ws)
	icons := make([]string, len(leaves))
	for i, leaf := range leaves {
		icons[i] = stringOfNode(conf, leaf)
	}
	icons = removeDups(icons)

	newName := strings.Join(icons, "|")
	if newName == "" {
		newName = strconv.Itoa(wsNum)
	} else {
		newName = fmt.Sprintf("%d:%s", wsNum, newName)
	}

	if wsName == newName {
		return nil
	}

	cmd := fmt.Sprintf(`rename workspace "%s" to "%s"`, wsName, newName)
	log.Debug("SENDING COMMAND", "cmd", cmd)
	outcomes, err := conn.Command(cmd)
	if err != nil {
		return err
	}
	for i, o := range outcomes {
		errStr := ""
		if o.Error != nil {
			errStr = *o.Error
		}
		log.Debug("RESULT", "index", i+1, "success", o.Success, "error", errStr)
	}
	return nil
}

func handleWindowEvent(conf []IconConf, conn *Conn, info WindowEventInfo, log *slog.Logger) {
	switch info.Change {
	case WinNew, WinClose, WinTitle, WinMove:
		tree, err := conn.GetTree()
		if err != nil {
			log.Error("get_tree failed", "error", err)
			return
		}
		for _, ws := range getWorkspaceNodes(&tree) {
			if err := renameWorkspace(conf, conn, ws, log); err != nil {
				log.Error("rename_workspace failed", "error", err)
			}
		}
	}
}

// ── Event loop ────────────────────────────────────────────────────────────────

// eventLoop returns true if the caller should reconnect (Sway restart or
// unexpected disconnect), false if it should exit cleanly.
func eventLoop(conf []IconConf, conn *Conn, sigCh <-chan os.Signal, log *slog.Logger) bool {
	shutdownReason := ShutdownNone
	for {
		select {
		case ev, ok := <-conn.eventCh:
			if !ok {
				// Connection closed. Reconnect unless we got a clean exit.
				return shutdownReason != ShutdownExit
			}
			switch e := ev.(type) {
			case WindowEvent:
				log.Debug("EVENT", "change", e.Info.Change)
				handleWindowEvent(conf, conn, e.Info, log)
			case ShutdownEvent:
				shutdownReason = e.Reason
				if e.Reason == ShutdownExit {
					log.Info("Sway shutdown, exiting")
					return false
				}
				// ShutdownRestart: keep looping until the socket closes.
			}
		case sig := <-sigCh:
			log.Info("Signal received, shutting down", "signal", sig)
			return false
		}
	}
}

func protectedLoop(conf []IconConf, sigCh <-chan os.Signal, log *slog.Logger) error {
	for {
		conn, err := Connect()
		if err != nil {
			return fmt.Errorf("connect: %w", err)
		}
		if err := conn.Subscribe([]string{"window", "shutdown"}); err != nil {
			conn.Close()
			return fmt.Errorf("subscribe: %w", err)
		}
		log.Debug("connected to i3/Sway")

		reconnect := eventLoop(conf, conn, sigCh, log)
		conn.Close()

		if !reconnect {
			return nil
		}
		log.Info("Sway is restarting, wait a second...")
		time.Sleep(1 * time.Second)
		log.Debug("reconnecting to Sway")
	}
}

// ── CLI & main ────────────────────────────────────────────────────────────────

func absPath(path, wd string) string {
	if filepath.IsAbs(path) {
		return path
	}
	return filepath.Join(wd, path)
}

// longFlagNames are the multi-letter flag names that must be given with a
// "--" prefix (GNU/Go convention), never a single "-". Single-letter
// shorthands (e.g. -c, -d) may keep a single dash.
var longFlagNames = map[string]bool{
	"daemon":  true,
	"verbose": true,
	"log":     true,
	"conf":    true,
	"uniq":    true,
	"version": true,
}

// rejectSingleDashLongFlags exits with an error if a multi-letter flag was
// given with a single dash (e.g. -daemon instead of --daemon).
func rejectSingleDashLongFlags(args []string) {
	for _, a := range args {
		if !strings.HasPrefix(a, "-") || strings.HasPrefix(a, "--") {
			continue
		}
		name := strings.TrimPrefix(a, "-")
		if eq := strings.IndexByte(name, '='); eq >= 0 {
			name = name[:eq]
		}
		if longFlagNames[name] {
			fmt.Fprintf(os.Stderr, "ws-rename: unknown flag -%s (did you mean --%s?)\n", name, name)
			os.Exit(2)
		}
	}
}

func main() {
	home, _ := os.LookupEnv("HOME")
	defaultLog := filepath.Join(home, ".cache", "ws-rename", "log.txt")
	defaultConf := getDefaultConfFname("app-icons.json")

	var (
		uniq        bool
		daemon      bool
		verbose     bool
		showVersion bool
		logFile     string
		confFile    string
	)

	flag.BoolVar(&uniq, "u", false, "Remove duplicate icons in case the same application")
	flag.BoolVar(&uniq, "uniq", false, "Remove duplicate icons in case the same application")
	flag.BoolVar(&daemon, "d", false, "Daemon mode: send the application to background")
	flag.BoolVar(&daemon, "daemon", false, "Daemon mode: send the application to background")
	flag.BoolVar(&verbose, "v", false, "Print debug information")
	flag.BoolVar(&verbose, "verbose", false, "Print debug information")
	flag.StringVar(&logFile, "l", defaultLog, "Log file path")
	flag.StringVar(&logFile, "log", defaultLog, "Log file path")
	flag.StringVar(&confFile, "c", defaultConf, "Configuration file path")
	flag.StringVar(&confFile, "conf", defaultConf, "Configuration file path")
	flag.BoolVar(&showVersion, "version", false, "Print version information and exit")

	rejectSingleDashLongFlags(os.Args[1:])
	flag.Parse()

	if showVersion {
		fmt.Println("ws-rename " + version)
		os.Exit(0)
	}

	_ = uniq // accepted but unused, like the OCaml version

	wd, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "getwd: %v\n", err)
		os.Exit(1)
	}

	logFile = absPath(logFile, wd)

	if confFile != "" {
		confFile = absPath(confFile, wd)
		if info, err := os.Stat(confFile); err != nil || info.IsDir() {
			fmt.Fprintf(os.Stderr, "ERROR: file %q does not exist or not readable.\n", confFile)
			os.Exit(1)
		}
	}

	if err := os.MkdirAll(filepath.Dir(logFile), 0o755); err != nil {
		fmt.Fprintf(os.Stderr, "create log dir: %v\n", err)
		os.Exit(1)
	}

	if daemon && os.Getenv(daemonEnvVar) == "" {
		daemonize(wd)
		// parent exits inside daemonize(); never reached
	}

	level := slog.LevelInfo
	if verbose {
		level = slog.LevelDebug
	}
	handler, err := newFileHandler(logFile, level)
	if err != nil {
		fmt.Fprintf(os.Stderr, "open log file: %v\n", err)
		os.Exit(1)
	}
	log := slog.New(handler)

	conf, err := readConf(confFile)
	if err != nil {
		log.Error("failed to read configuration", "file", confFile, "error", err)
		os.Exit(1)
	}
	if confFile != "" {
		log.Debug("configuration loaded", "file", confFile)
	}

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, syscall.SIGTERM, syscall.SIGINT)

	if err := protectedLoop(conf, sigCh, log); err != nil {
		log.Error("fatal error", "error", err)
		os.Exit(1)
	}
}
