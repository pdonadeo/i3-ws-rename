open Utils


let must_shutdown = ref false

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let redirect_to_dev_null ~mode fd =
  let dev_null = Unix.openfile "/dev/null" [mode] 0o777 in
  Unix.dup2 dev_null fd;
  Unix.close dev_null

let daemonize ?(cd = "/") ?umask () =
  let fork_no_parent () =
    match Unix.handle_unix_error Unix.fork () with
    | 0 -> () (* in the child *)
    | _pid -> exit 0 (* the parent exits *)
  in
  (* Fork into the background, parent exits, child continues. *)
  fork_no_parent ();
  (* Become session leader. *)
  ignore (Unix.setsid ());
  (* Fork again to ensure that we will never regain a controlling terminal. *)
  fork_no_parent ();
  (* Release old working directory. *)
  Unix.chdir cd;
  (* Ensure sensible umask.  Adjust as needed. *)
  let () =
    match umask with
    | None -> ()
    | Some umask -> Unix.umask umask |> ignore in
  redirect_to_dev_null ~mode:Unix.O_RDONLY Unix.stdin;
  redirect_to_dev_null ~mode:Unix.O_WRONLY Unix.stdout;
  redirect_to_dev_null ~mode:Unix.O_WRONLY Unix.stderr

let handle_protocol_error e =
  let open I3ipc in
  let msg = match e with
  | No_IPC_socket -> Printf.sprintf "No_IPC_socket"
  | Bad_magic_string mn -> Printf.sprintf "Bad_magic_string: %s" mn
  | Unexpected_eof -> Printf.sprintf "Unexpected_eof"
  | Unknown_type _std_int_uint32 -> Printf.sprintf "Unknown_type"
  | Bad_reply reply -> Printf.sprintf "Bad_reply: %s" reply in
  Logs.err (fun m -> m "handle_protocol_error: %s" msg)

let handle_win_event conf conn (event_info : I3ipc.Event.window_event_info) =
  let open I3ipc in

  match event_info.Event.change with
  | Event.New | Close | Title | Move -> begin
    let%lwt tree = get_tree conn in
    let ws_nodes = get_workspaces_nodes tree in
    Lwt_list.iter_p (rename_workspace conf conn) ws_nodes
  end
  | _ -> Lwt.return ()

let rec loop conf conn =
  let open I3ipc in
  let b = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer b in

  let%lwt ev = next_event conn in
  let%lwt () = match ev with
    | Window info -> begin
      Event.pp_window_event_info fmt info;
      handle_win_event conf conn info
    end
    | Shutdown reason -> begin
      Event.pp_shutdown_reason fmt reason;
      Format.pp_print_newline fmt ();
      (match reason with
      | Restart -> must_shutdown := false
      | Exit -> must_shutdown := true);
      Lwt.return ()
    end
    | _ -> Lwt.return () in
  Format.pp_print_flush fmt ();
  Logs.debug (fun m -> m "EVENT: %s" (Buffer.contents b));
  loop conf conn

let rec protected_loop conf conn =
  try%lwt loop conf conn
  with I3ipc.Protocol_error e -> begin
    handle_protocol_error e;
    if !must_shutdown then begin
      Logs.info (fun m -> m "i3 shutdown, exiting");
      Lwt.return ()
    end else begin
      Logs.info (fun m -> m "i3 is restarting, wait a second...");
      let%lwt () = Lwt_unix.sleep 1.0 in
      must_shutdown := false;
      let%lwt conn = connect_and_subscribe () in
      Logs.debug (fun m -> m "...reconnected to i3");
      protected_loop conf conn
    end
  end

let main unique verbose =
  Logs.set_reporter (lwt_reporter ());
  if verbose
    then Logs.set_level (Some Logs.Debug)
    else Logs.set_level (Some Logs.Info);

  let%lwt conf = Conf.read_configuration "app-icons.json" in
  Logs.debug (fun m -> m "Configuration successfully loaded");

  ignore (unique);

  let%lwt conn = connect_and_subscribe () in
  protected_loop conf conn

open Cmdliner

let unique =
  let doc = "Remove duplicate icons in case the same application." in
  Arg.(value & flag & info ["u"; "uniq"] ~doc)

let daemon =
  let doc = "Daemon mode: send the application to background." in
  Arg.(value & flag & info ["d"; "daemon"] ~doc)

let verbose =
  let doc = "Print debug informations." in
  Arg.(value & flag & info ["v"; "verbose"] ~doc)

let info =
  let doc = "Dynamically update i3wm workspace names based on running applications in each and optionally define an icon to show instead." in
  let man = [
    `S Manpage.s_bugs;
    `P "Bug reports on GitHub: https://github.com/pdonadeo/i3-ws-rename/issues" ]
  in
  Term.info "i3-ws-rename" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let main' u daemon verbose =
  if daemon then daemonize ();
  Lwt_main.run (main u verbose)

let main_t = Term.(const main' $ unique $ daemon $ verbose)

let () = Term.exit @@ Term.eval (main_t, info)
