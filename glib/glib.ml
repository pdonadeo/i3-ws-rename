open Ctypes
open Foreign

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#GMainContext
 *)
type g_main_context
let g_main_context : g_main_context structure typ = structure "GMainContext"

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#GMainLoop
 *)
type g_main_loop
let g_main_loop : g_main_loop structure typ = structure "GMainLoop"

(*
 * https://developer.gnome.org/glib/stable/glib-Basic-Types.html#gpointer
 *)
let gpointer = ptr void

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#GSourceFunc
 *)
let g_source_func = gpointer @-> returning bool

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-main-context-default
 * GMainContext* g_main_context_default (void)
 *)
let g_main_context_default = foreign "g_main_context_default" (void @-> returning (ptr g_main_context))

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-main-loop-new
 * GMainLoop * g_main_loop_new (GMainContext *context, gboolean is_running)
 *)
let g_main_loop_new = foreign "g_main_loop_new" ((ptr g_main_context) @-> bool @-> returning (ptr g_main_loop))

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-main-loop-run
 * void g_main_loop_run (GMainLoop *loop)
 *)
let g_main_loop_run = foreign "g_main_loop_run" ((ptr g_main_loop) @-> returning void)

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-main-loop-quit
 * void g_main_loop_quit (GMainLoop *loop)
 *)
let g_main_loop_quit = foreign "g_main_loop_quit" ((ptr g_main_loop) @-> returning void)

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-main-loop-unref
 * void g_main_loop_unref (GMainLoop *loop)
 *)
let g_main_loop_unref = foreign "g_main_loop_unref" ((ptr g_main_loop) @-> returning void)

(*
 * https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html#g-timeout-add-seconds
 * guint g_timeout_add_seconds (guint interval, GSourceFunc function, gpointer data)
 *)
let g_timeout_add_seconds =
  foreign "g_timeout_add_seconds"
    (uint @-> funptr g_source_func @-> gpointer @-> returning uint)
