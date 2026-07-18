# i3-ws-rename

`i3-ws-rename` is a small background helper for the [i3](https://i3wm.org/) and
[Sway](https://swaywm.org/) window managers. It watches your workspaces and
automatically renames each one to show an icon for every application you have
open in it, so instead of a bare workspace number your bar shows something
like:

```
1:🦊|📝   2:🎵   3:💻|💬
```

No more guessing which workspace has your browser, your editor, or your chat
app — you can see it at a glance in the bar.

## How it works

Every time you open, close, move, or rename a window, `i3-ws-rename` looks at
which applications are present on each workspace and renames the workspace to
a string made of the workspace number followed by one icon (or short label)
per open application, separated by `|`. If it doesn't recognize an
application, it falls back to a generic icon (▨) or to the application's own
name in lowercase.

The program runs quietly in the background and does not require any
interaction once it's set up.

## Requirements

- i3 or Sway, with a status bar that displays workspace names (the default
  bars in both window managers do).
- A font that can render the icons or emoji you choose to use (most modern
  system fonts and emoji fonts work fine).

## Installing

`i3-ws-rename` is written in Go and has no external dependencies, so building
it only requires a Go toolchain (Go 1.26 or newer):

```sh
go build -o ws-rename .
```

Copy the resulting `ws-rename` binary somewhere on your `PATH`, for example
`~/.local/bin/`.

## Setting up your icons

You tell `i3-ws-rename` which icon or label to show for each application
through a configuration file named `app-icons.json`. By default the program
looks for it in:

1. `$XDG_CONFIG_HOME/sway/app-icons.json` (if `XDG_CONFIG_HOME` is set)
2. `~/.config/sway/app-icons.json`
3. `~/.i3/app-icons.json`

The file is a JSON list, one entry per application. On i3 (X11), applications
are matched by *window class* and/or *window instance*; on Sway (Wayland),
they are matched by *app ID* and, optionally, by a pattern on the window
title. A minimal example:

```json
[
  { "window_class": "firefox", "icon": "🦊" },
  { "window_class": "code", "window_instance": "code", "icon": "📝" },
  { "app_id": "firefox", "icon": "🦊" },
  { "app_id": "foot", "icon": "💻" },
  { "app_id": "foot", "name": "^weechat", "icon": "💬" }
]
```

- `window_class` / `window_instance` — used on i3, these correspond to the
  values `xprop` shows for a window (`WM_CLASS`).
- `app_id` — used on Sway, this is the Wayland application identifier.
- `name` — optional; matches against the window title, letting you show a
  different icon for the same application depending on what's open in it
  (e.g. a terminal running a chat client vs. a plain shell).
- `icon` — the text shown on the workspace: typically a single emoji or a
  glyph from an icon font, but any short string works.

You don't need to restart `i3-ws-rename` after editing this file — just
restart the program (see below) to pick up your changes.

## Running it

Add a line like this to your i3 or Sway configuration file so the program
starts automatically with your session:

```
exec ws-rename --daemon
```

Available command-line options:

| Flag              | Description                                                          |
| ----------------- | --------------------------------------------------------------------- |
| `-d`, `--daemon`  | Run in the background instead of staying attached to your terminal.  |
| `-v`, `--verbose` | Write extra diagnostic detail to the log file, useful when troubleshooting. |
| `-l`, `--log`     | Path to the log file (default: `~/.cache/ws-rename/log.txt`).        |
| `-c`, `--conf`    | Path to a specific `app-icons.json` file, if you don't want to rely on the default locations. |
| `--version`       | Print the program's version and exit.                                |

If something doesn't look right — an icon not showing up, or a workspace not
renaming — the log file is the first place to check; running with `--verbose`
gives a detailed trace of what the program saw and did.

## Restarting after a Sway reload

If Sway restarts (for example after a config reload), `i3-ws-rename`
automatically reconnects and keeps working without needing to be started
again by hand.

## License

MIT — see [LICENSE](LICENSE).
