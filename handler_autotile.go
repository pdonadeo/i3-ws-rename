package main

import (
	"context"
	"fmt"
	"log/slog"
)

// autotileHandler picks the split direction of the focused container to match
// its shape: wider than tall means the next window goes beside it, taller than
// wide means it goes below. Sway tiles manually — the split direction is
// whatever you last set with `split h`/`split v` — so without this, opening
// windows one after another keeps slicing the same axis and leaves you with
// ever-narrower columns. This is the behaviour of nwg-piotr/autotiling.
type autotileHandler struct {
	// splitRatio is the share of the container a newly opened window gets,
	// as a percentage. Sway splits evenly on its own, so 50 means "send no
	// resize command at all".
	splitRatio int
	// lastSplit remembers the direction last applied to a container, to avoid
	// re-sending the same command on every focus change. It also means a split
	// you set by hand survives until the container's shape actually changes.
	lastSplit map[NodeID]string
}

func newAutotileHandler(splitRatio int) *autotileHandler {
	return &autotileHandler{splitRatio: splitRatio, lastSplit: make(map[NodeID]string)}
}

func (h *autotileHandler) Name() string { return "autotiling" }

func (h *autotileHandler) HandleWindow(ctx *WindowContext) error {
	switch ctx.Info.Change {
	case WinClose:
		delete(h.lastSplit, ctx.Info.Container.ID)
		return nil
	case WinNew, WinFocus, WinMove, WinFloating:
	default:
		return nil
	}

	isNew := ctx.Info.Change == WinNew
	if isNew {
		// Must come first: the resize changes the window's shape, and the
		// split direction below has to be decided on the shape it ends up
		// with, not on the even split Sway just gave it.
		if err := h.resizeNewWindow(ctx); err != nil {
			return err
		}
	}

	tree, err := ctx.Tree()
	if err != nil {
		return fmt.Errorf("get_tree: %w", err)
	}
	node, parent := findFocusedLeaf(tree)
	if node == nil || parent == nil {
		return nil
	}

	// Floating and fullscreen windows have no meaningful split direction, and
	// in a tabbed or stacked container the children are drawn on top of each
	// other: setting a split there would silently reshape the layout.
	if node.NodeType == "floating_con" || parent.NodeType == "floating_con" {
		return nil
	}
	if node.FullscreenMode != 0 {
		return nil
	}
	if parent.Layout == "tabbed" || parent.Layout == "stacked" {
		return nil
	}

	split := "splith"
	if node.Rect.Height > node.Rect.Width {
		split = "splitv"
	}
	if h.lastSplit[node.ID] == split {
		return nil
	}
	h.lastSplit[node.ID] = split

	// A new window is worth reporting; the adjustments that merely follow the
	// focus around are frequent and would drown the journal, so they stay at
	// debug level.
	level := slog.LevelDebug
	if isNew {
		level = slog.LevelInfo
	}
	ctx.Log.Log(context.Background(), level, "setting split direction",
		"con_id", node.ID, "split", split,
		"width", node.Rect.Width, "height", node.Rect.Height)
	return ctx.Command(fmt.Sprintf("[con_id=%s] %s", node.ID, split))
}

// resizeNewWindow gives a freshly opened window its configured share of the
// container, leaving the rest to the window that was already there. Sway
// splits evenly, so without this every new window would halve the space.
func (h *autotileHandler) resizeNewWindow(ctx *WindowContext) error {
	if h.splitRatio == 50 {
		return nil // already what Sway does: no point sending a command
	}

	tree, err := ctx.Tree()
	if err != nil {
		return fmt.Errorf("get_tree: %w", err)
	}
	node, parent := findNodeByID(tree, ctx.Info.Container.ID)
	if node == nil || parent == nil {
		return nil
	}
	if node.NodeType == "floating_con" || node.FullscreenMode != 0 {
		return nil
	}
	// Exactly two children means this window has just been paired with the one
	// that was there before, which is the case this ratio is about. Anything
	// else (a tabbed or stacked parent, a container built by hand, the very
	// first window on a workspace) is left alone.
	if len(parent.Nodes) != 2 {
		return nil
	}

	var axis string
	switch parent.Layout {
	case "splith":
		axis = "width"
	case "splitv":
		axis = "height"
	default:
		return nil
	}

	ctx.Log.Info("resizing new window",
		"con_id", node.ID, "axis", axis, "ratio", h.splitRatio)
	return ctx.Command(fmt.Sprintf("[con_id=%s] resize set %s %d ppt",
		node.ID, axis, h.splitRatio))
}

// findFocusedLeaf returns the focused window along with its parent container.
// Only leaves are considered: a focused split container is not a window.
func findFocusedLeaf(root *Node) (node, parent *Node) {
	traverseDeepFirst(func(n *Node, p *Node, _ int) {
		if n.Focused && len(n.Nodes) == 0 && len(n.FloatingNodes) == 0 {
			node, parent = n, p
		}
	}, root, nil, 0)
	return node, parent
}

// findNodeByID returns a container by id along with its parent. Used for the
// window an event is about, which is not necessarily the focused one.
func findNodeByID(root *Node, id NodeID) (node, parent *Node) {
	traverseDeepFirst(func(n *Node, p *Node, _ int) {
		if n.ID == id {
			node, parent = n, p
		}
	}, root, nil, 0)
	return node, parent
}
