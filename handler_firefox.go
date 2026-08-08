package main

import (
	"bufio"
	"fmt"
	"log/slog"
	"os"
	"regexp"
	"strings"
)

// firefoxAppID is the only application these rules apply to. Sway sets the
// real title of a Firefox window through a separate "title" event that arrives
// *after* the window is mapped, so `for_window [app_id=... title=...]` — whose
// criteria are evaluated once, at map time — often never fires. Matching the
// title as it actually arrives is the whole point of this handler.
const firefoxAppID = "org.mozilla.firefox"

// firefoxTitleRulesFile is looked up in the usual config directories; the
// handler is enabled by the mere presence of the file.
const firefoxTitleRulesFile = "firefox-title-rules.conf"

type firefoxRule struct {
	re      *regexp.Regexp
	cmds    string
	pattern string // as written in the config file, for log records
}

// firefoxHandler applies title-based rules to Firefox windows as their title
// arrives, replacing the for_window rules that no longer fire reliably.
type firefoxHandler struct {
	rules []firefoxRule
	// applied tracks which rules already ran for a given container, so that
	// the flood of "title" events a single window produces doesn't re-run the
	// same command over and over. Entries are dropped when the window closes:
	// in a daemon that runs for weeks, keeping them forever would be a slow
	// but real leak.
	applied map[NodeID]map[int]bool
}

func newFirefoxHandler(rules []firefoxRule) *firefoxHandler {
	return &firefoxHandler{rules: rules, applied: make(map[NodeID]map[int]bool)}
}

func (h *firefoxHandler) Name() string { return "firefox-title-rules" }

func (h *firefoxHandler) HandleWindow(ctx *WindowContext) error {
	c := &ctx.Info.Container

	switch ctx.Info.Change {
	case WinClose:
		delete(h.applied, c.ID)
		return nil
	case WinNew, WinTitle:
	default:
		return nil
	}

	if c.AppID == nil || *c.AppID != firefoxAppID {
		return nil
	}
	if c.Name == nil || *c.Name == "" {
		return nil // title not set yet: the "title" event will bring it
	}
	title := *c.Name

	for i := range h.rules {
		if h.applied[c.ID][i] {
			continue
		}
		if !h.rules[i].re.MatchString(title) {
			continue
		}
		if h.applied[c.ID] == nil {
			h.applied[c.ID] = make(map[int]bool)
		}
		h.applied[c.ID][i] = true

		ctx.Log.Info("title rule matched",
			"con_id", c.ID, "title", title, "pattern", h.rules[i].pattern)
		cmd := fmt.Sprintf("[con_id=%s] %s", c.ID, h.rules[i].cmds)
		if err := ctx.Command(cmd); err != nil {
			ctx.Log.Error("rule command failed", "cmd", cmd, "error", err)
		}
	}
	return nil
}

// ── Rules file ────────────────────────────────────────────────────────────────

// loadFirefoxRules reads the "pattern=commands" rules file: one rule per line,
// split on the first "=" (so both halves may contain further "="), blank lines
// and lines starting with "#" ignored, whitespace around the separator
// trimmed. A line that fails to compile is reported and skipped rather than
// taking the whole handler down with it.
func loadFirefoxRules(path string, log *slog.Logger) ([]firefoxRule, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer func() { _ = f.Close() }() // read-only: nothing useful to do on failure

	var rules []firefoxRule
	scanner := bufio.NewScanner(f)
	for lineNo := 1; scanner.Scan(); lineNo++ {
		line := scanner.Text()
		if strings.TrimSpace(line) == "" || strings.HasPrefix(strings.TrimSpace(line), "#") {
			continue
		}
		rawPattern, rawCmds, found := strings.Cut(line, "=")
		if !found {
			log.Warn("ignoring rule without '=' separator", "file", path, "line", lineNo)
			continue
		}
		pattern := strings.TrimRight(rawPattern, " \t")
		cmds := strings.TrimLeft(rawCmds, " \t")

		re, err := regexp.Compile(pattern)
		if err != nil {
			log.Error("ignoring rule with invalid regexp",
				"file", path, "line", lineNo, "pattern", pattern, "error", err)
			continue
		}
		rules = append(rules, firefoxRule{re: re, cmds: cmds, pattern: pattern})
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return rules, nil
}
