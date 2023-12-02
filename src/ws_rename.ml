open Utils

let spf = Printf.sprintf
let shutdown, do_shutdown = Lwt.wait ()

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
    | Some umask -> Unix.umask umask |> ignore
  in
  redirect_to_dev_null ~mode:Unix.O_RDONLY Unix.stdin;
  redirect_to_dev_null ~mode:Unix.O_WRONLY Unix.stdout;
  redirect_to_dev_null ~mode:Unix.O_WRONLY Unix.stderr

let handle_protocol_error e =
  let open I3ipc in
  let msg =
    match e with
    | No_IPC_socket -> Printf.sprintf "No_IPC_socket"
    | Bad_magic_string mn -> Printf.sprintf "Bad_magic_string: %s" mn
    | Unexpected_eof -> Printf.sprintf "Unexpected_eof"
    | Unknown_type _std_int_uint32 -> Printf.sprintf "Unknown_type"
    | Bad_reply reply -> Printf.sprintf "Bad_reply: %s" reply
  in
  Logs.err (fun m -> m "handle_protocol_error: %s" msg)

let get_workspaces_nodes root =
  traverse_deep_first
    (fun acc _depth n _parent ->
      let open I3ipc in
      let open Reply in
      if n.nodetype = Workspace && opt_def ~def:(-1) n.num > 0 then n :: acc else acc)
    []
    root
  |> List.rev

let string_of_node conf (node : I3ipc.Reply.node) =
  let open I3ipc in
  let open Reply in
  match node.window_properties with
  | None -> begin
    match node.app_id with
    | Some app_id -> begin
      let app_id = String.lowercase_ascii app_id in
      let app_id_records = List.filter (fun r -> r.Conf.app_id = Some app_id) conf in
      match node.name with
      | Some node_name -> begin
        let node_name = String.lowercase_ascii node_name in
        List.filter
          (fun r ->
            match r.Conf.name with
            | None -> true
            | Some name -> Re2.matches (Re2.create_exn name) node_name)
          app_id_records
        |> Conf.hd_opt
        |> opt_map ~f:(fun r -> r.Conf.icon)
        |> opt_def ~def:"�"
      end
      | None -> begin
        List.filter (fun r -> r.Conf.name = None) app_id_records
        |> Conf.hd_opt
        |> opt_map ~f:(fun r -> r.Conf.icon)
        |> opt_def ~def:"�"
      end
    end
    | None -> "�"
  end
  | Some wp -> begin
    match (wp.class_, wp.instance) with
    | None, None -> "�"
    | None, Some i -> begin
      match Conf.search_instance conf i with
      | Some icon -> icon
      | None -> String.lowercase_ascii i
    end
    | Some c, None -> begin
      match Conf.search_class conf c with
      | Some icon -> icon
      | None -> String.lowercase_ascii c
    end
    | Some c, Some i -> begin
      match Conf.search_class_instance conf c i with
      | Some icon -> icon
      | None -> begin
        match Conf.search_class conf c with
        | Some icon -> icon
        | None -> String.lowercase_ascii c
      end
    end
  end

let remove_dups xs =
  List.fold_left
    (fun (last, acc) s ->
      match last with
      | None -> (Some s, s :: acc)
      | Some l -> begin if s = l then (Some s, acc) else (Some s, s :: acc) end)
    (None, [])
    xs
  |> snd
  |> List.rev

let rename_workspace conf conn ws =
  let open I3ipc in
  let open Reply in
  let ws_name = opt_def ~def:"N/A" ws.name in
  let ws_num = opt_def ~def:0 ws.num in

  Format.fprintf Format.str_formatter "%a" Reply.pp_node ws;
  let ws_str = Format.flush_str_formatter () in
  Logs.err (fun m -> m "NODE: %s" ws_str);

  Logs.debug (fun m -> m "====================================================");
  Logs.debug (fun m -> m "WORKSPACE NUMBER %d (%s)" ws_num ws_name);
  let leaves = extract_leaves ws in
  List.iter (fun (leaf : node) -> Logs.debug (fun m -> m "    %s; %s" leaf.id (opt_def ~def:"N/A" leaf.name))) leaves;
  let new_name = leaves |> List.map (string_of_node conf) |> remove_dups |> String.concat "|" in
  let new_name = if new_name = "" then string_of_int ws_num else Printf.sprintf "%d:%s" ws_num new_name in
  if ws_name <> new_name
  then begin
    let cmd = spf "rename workspace \"%s\" to \"%s\"" ws_name new_name in
    Logs.debug (fun m -> m "SENDING COMMAND: %s" cmd);
    let%lwt outcomes = command conn cmd in
    List.iteri
      (fun i outcome ->
        Logs.debug (fun m -> m "    RESULT[%d].success = %s" (i + 1) (string_of_bool outcome.success));
        Logs.debug (fun m -> m "    RESULT[%d].error   = %s" (i + 1) (opt_def ~def:"" outcome.error)))
      outcomes;

    Logs.debug (fun m -> m "----------------------------------------------------");
    Lwt.return ()
  end
  else Lwt.return ()

let handle_win_event conf conn (event_info : I3ipc.Event.window_event_info) =
  let open I3ipc in
  let%lwt tree = get_tree conn in
  match event_info.Event.change with
  | Event.New | Close | Title | Move -> begin
    let ws_nodes = get_workspaces_nodes tree in
    let%lwt () = Lwt_list.iter_p (rename_workspace conf conn) ws_nodes in
    Lwt.return_unit
  end
  | _ -> Lwt.return ()

let rec loop conf conn =
  let open I3ipc in
  let b = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer b in

  let ev = Lwt.choose [Lwt.map (fun e -> `I3_event e) (next_event conn); Lwt.map (fun _ -> `Shutdown) shutdown] in

  match%lwt ev with
  | `I3_event ev -> begin
    let%lwt () =
      match ev with
      | Window info -> begin
        Event.pp_window_event_info fmt info;
        Format.pp_print_flush fmt ();
        Logs.debug (fun m -> m "EVENT: %s" (Buffer.contents b));
        handle_win_event conf conn info
      end
      | Shutdown reason -> begin
        Event.pp_shutdown_reason fmt reason;
        Format.pp_print_newline fmt ();
        Format.pp_print_flush fmt ();
        Logs.debug (fun m -> m "EVENT: %s" (Buffer.contents b));
        match reason with
        | Restart -> Lwt.return_unit
        | Exit -> Lwt.wakeup do_shutdown 0 |> Lwt.return
      end
      | _ -> Lwt.return ()
    in
    Gc.compact ();
    Malloc.malloc_trim 0;
    loop conf conn
  end
  | `Shutdown -> begin
    let%lwt signal = shutdown in
    Logs.info (fun m -> m "Signal %s received, shutting down..." (string_of_signal signal));
    Lwt.return_unit
  end

let rec protected_loop conf conn =
  try%lwt loop conf conn
  with I3ipc.Protocol_error e ->
    handle_protocol_error e;
    if not (Lwt.is_sleeping shutdown)
    then begin
      Logs.info (fun m -> m "Sway shutdown, exiting");
      Lwt.return ()
    end
    else begin
      Logs.info (fun m -> m "Sway is restarting, wait a second...");
      let%lwt () = Lwt_unix.sleep 1.0 in
      let%lwt conn = connect_and_subscribe () in
      Logs.debug (fun m -> m "...reconnected to Swat");
      protected_loop conf conn
    end

let rec gc_loop () =
  let open Gc in
  let%lwt () = Lwt_unix.sleep 60.0 in
  Logs.debug (fun m -> m "GARBAGE COLLECTION LOOP");
  compact ();
  gc_loop ()

let _ = Lwt_unix.on_signal Sys.sigterm (fun s -> Lwt.wakeup do_shutdown s)

let main _unique verbose log_fname conf_fname =
  Logs.set_reporter (Reporter.lwt_file_reporter (Some log_fname));
  if verbose then Logs.set_level (Some Logs.Debug) else Logs.set_level (Some Logs.Info);

  let%lwt conf = Conf.read_icons_configuration conf_fname in

  detach_promise gc_loop "gc_loop";
  let%lwt conn = connect_and_subscribe () in
  let%lwt _ = Lwt.all [protected_loop conf conn] in
  let%lwt () = Lwt_unix.sleep 1.0 in
  Lwt.return_unit

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

let log_fname_def = Unix.getenv "HOME" / ".cache/ws-rename/log.txt"

let log_fname =
  let doc = "Position of the log file." in
  Arg.(value & opt string log_fname_def & info ["l"; "log"] ~docv:"LOG" ~doc)

let conf_fname_def = get_default_conf_fname "app-icons.json"

let conf_fname =
  let doc = "Specifies an alternate configuration file path." in
  Arg.(value & opt string conf_fname_def & info ["c"; "conf"] ~docv:"CONFIGURATION" ~doc)

let main' u daemon verbose log_fname conf_fname =
  let cd = Unix.getcwd () in
  let log_fname = if Filename.is_relative log_fname then cd / log_fname else log_fname in

  let conf_fname =
    if conf_fname = ""
    then None
    else begin
      if file_exists_and_is_readable conf_fname
      then Some conf_fname
      else begin
        Printf.eprintf "ERROR: file \"%s\" does not exist or not readable.\n%!" conf_fname;
        exit 1
      end
    end
  in

  mkdir_p (Filename.dirname log_fname) 0o755;

  if daemon then daemonize ~cd ();
  Lwt_main.run (main u verbose log_fname conf_fname)

let () = Printexc.record_backtrace true
let () = Gc.set { (Gc.get ()) with Gc.allocation_policy = 2; Gc.space_overhead = 85 }

let cmd =
  let info =
    let doc =
      "Dynamically update Sway workspace names based on running applications in each and optionally define an icon to show instead."
    in
    let man = [`S Manpage.s_bugs; `P "Bug reports on GitHub: https://github.com/pdonadeo/i3-ws-rename/issues"] in
    Cmd.info "%%NAME%%" ~version:"%%VERSION%%" ~doc ~exits:Cmd.Exit.defaults ~man
  in
  Cmd.v info Term.(const main' $ unique $ daemon $ verbose $ log_fname $ conf_fname)

let () = exit (Cmd.eval cmd)