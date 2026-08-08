# ws-rename

`ws-rename` is a small background helper for the [Sway](https://swaywm.org/)
window manager. It watches your workspaces and automatically renames each one
to show an icon for every application you have open in it, so instead of a
bare workspace number your bar shows something like:

```
1:🦊|📝   2:🎵   3:💻|💬
```

No more guessing which workspace has your browser, your editor, or your chat
app — you can see it at a glance in the bar.

## How it works

Every time you open, close, move, or rename a window, `ws-rename` looks at
which applications are present on each workspace and renames the workspace to
a string made of the workspace number followed by one icon (or short label)
per open application, separated by `|`. If it doesn't recognize an
application, the fallback depends on the window: native Wayland windows fall
back to a generic icon (▨); windows running through XWayland (apps that
haven't been ported to native Wayland) fall back to their own class (or
instance, if class isn't set) in lowercase — see below for why the two
differ.

The program runs quietly in the background and does not require any
interaction once it's set up.

## Requirements

- Sway, with a status bar that displays workspace names (the default bar
  does).
- A font that can render the icons or emoji you choose to use (most modern
  system fonts and emoji fonts work fine).

## Installing

The simplest way is to grab a prebuilt binary from the
[Releases](../../releases) page — `ws-rename-linux-amd64` or
`ws-rename-linux-arm64`, depending on your machine — and copy it somewhere on
your `PATH`, for example `~/.local/bin/ws-rename` (remember to `chmod +x` it).

`ws-rename` is written in Go and has no external dependencies, so if you'd
rather build it yourself all you need is a Go toolchain (Go 1.26 or newer):

```sh
go build -o ws-rename .
```

Copy the resulting `ws-rename` binary somewhere on your `PATH`, for example
`~/.local/bin/`.

## Setting up your icons

You tell `ws-rename` which icon or label to show for each application through
a configuration file named `app-icons.json`. By default the program looks for
it in:

1. `$XDG_CONFIG_HOME/sway/app-icons.json` (if `XDG_CONFIG_HOME` is set)
2. `~/.config/sway/app-icons.json`

The file is a JSON list, one entry per application. Native Wayland windows are
matched by *app ID* and, optionally, by a pattern on the window title; windows
running through XWayland (apps that haven't been ported to native Wayland)
are matched by *window class* and/or *window instance* instead, the same
properties X11 windows have always exposed. A minimal example:

```json
[
  { "window_class": "firefox", "icon": "🦊" },
  { "window_class": "code", "window_instance": "code", "icon": "📝" },
  { "app_id": "firefox", "icon": "🦊" },
  { "app_id": "foot", "icon": "💻" },
  { "app_id": "foot", "name": "^weechat", "icon": "💬" }
]
```

- `window_class` / `window_instance` — used for XWayland windows, these
  correspond to the values `xprop` shows for a window (`WM_CLASS`).
- `app_id` — the Wayland application identifier, used for native Wayland
  windows.
- `name` — optional; matches against the window title, letting you show a
  different icon for the same application depending on what's open in it
  (e.g. a terminal running a chat client vs. a plain shell).
- `icon` — the text shown on the workspace: typically a single emoji or a
  glyph from an icon font, but any short string works.

When more than one entry could match a window, the most specific one wins:
for XWayland windows, class+instance beats class alone, which beats instance
alone; for native Wayland windows, app_id+name beats app_id alone. If nothing
matches, XWayland windows fall back to their own class (or instance) in
lowercase, since X11 windows essentially always have one; native Wayland
windows are only guaranteed an app_id, so there's nothing meaningful to fall
back to and you get a generic icon (▨) instead.

You don't need to restart `ws-rename` after editing this file — just restart
the program (see below) to pick up your changes.

## Extra behaviours

Because the program is already listening to every window event the compositor
emits, it can host other event-driven behaviours at no extra cost — a single
IPC subscription instead of one background script per feature. Both of the
following are off unless you turn them on.

### Firefox title rules

Sway evaluates `for_window` criteria once, when a window is mapped. Firefox on
Wayland sets its real title through a separate `title` event that arrives
*later*, so `for_window [app_id="org.mozilla.firefox" title="..."]` often never
fires. This feature applies the same rules when the title actually shows up.

It turns itself on if a file named `firefox-title-rules.conf` exists in the
same directories searched for `app-icons.json`. One rule per line, split on the
first `=`; blank lines and lines starting with `#` are ignored:

```
.*\| Brilliant — Mozilla Firefox=move container to workspace number 5, workspace number 5
```

The left-hand side is a regular expression matched against the window title
(unanchored, like sway's own criteria); the right-hand side is the list of
commands, applied exactly as they would be inside a `for_window` block. Each
rule runs at most once per window.

### Autotiling

With `--autotiling`, the split direction of the focused window is kept in sync
with its shape: wider than tall means the next window opens beside it, taller
than wide means below. Without it, Sway keeps splitting the same axis until
you are left with unusably narrow columns. Floating and fullscreen
windows, and containers in a tabbed or stacked parent, are left alone.

A new window also gets only part of the space instead of half of it: by
default 30%, leaving 70% to the window that was already there, on the theory
that what you were working on shouldn't be cut in half by what you just
opened. Use `--split-ratio` to change the share, or `--split-ratio 50` to go
back to even splits. Note that this applies to newly opened windows only —
a window *moved* into an existing container still lands on an even split.

## Running it

Add a line like this to your Sway configuration file so the program starts
automatically with your session:

```
exec ws-rename --daemon
```

Available command-line options:

| Flag              | Description                                                          |
| ----------------- | --------------------------------------------------------------------- |
| `-d`, `--daemon`  | Run in the background instead of staying attached to your terminal.  |
| `-v`, `--verbose` | Log extra diagnostic detail, useful when troubleshooting.            |
| `-c`, `--conf`    | Path to a specific `app-icons.json` file, if you don't want to rely on the default locations. |
| `--autotiling`    | Keep the split direction of the focused window matched to its shape. |
| `--split-ratio`   | Percentage of the container given to a newly opened window (default: 30; 50 splits evenly). Requires `--autotiling`. |
| `--firefox-rules` | Path to a specific Firefox title rules file; pass an empty value to disable the feature even when the file exists. |
| `--stderr`        | Log to stderr instead of the system log, handy when running in the foreground. |
| `--version`       | Print the program's version and exit.                                |
| `-u`, `--uniq`    | Accepted for backwards compatibility, but currently has no effect (consecutive duplicate icons are always collapsed). |

The program writes to the system log, so if something doesn't look like it
should — an icon not showing up, a workspace not renaming, a rule not firing —
that's the first place to check:

```sh
journalctl -t ws-rename -f
```

Running with `--verbose` adds a detailed trace of everything the program saw
and did, including the layout decisions it takes on every focus change.

## Restarting after a Sway reload

If Sway restarts (for example after a config reload), `ws-rename`
automatically reconnects and keeps working without needing to be started
again by hand.

## License

MIT — see [LICENSE](LICENSE).
