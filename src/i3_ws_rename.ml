open Constants
open Utils


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

let get_workspaces_nodes root =
  traverse_deep_first (fun acc _depth n _parent ->
    let open I3ipc in
    let open Reply in
    if n.nodetype = Workspace && (opt_def ~def:(-1) n.num) > 0
    then n::acc else acc
  ) [] root |> List.rev

let get_windows_nodes root =
  traverse_deep_first (fun acc _depth n _parent ->
    let open I3ipc in
    let open Reply in
    if n.nodetype = Con || n.nodetype = Floating_con
    then n::acc else acc
  ) [] root |> List.rev

let string_of_node conf (node : I3ipc.Reply.node) =
  let open I3ipc in
  let open Reply in

  match node.window_properties with
  | None -> "N/A"
  | Some wp -> begin
    match wp.class_, wp.instance with
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
  List.fold_left (fun (last, acc) s ->
    match last with
    | None -> Some s, s::acc
    | Some l -> begin
      if s = l then Some s, acc else Some s, s::acc
    end
  ) (None, []) xs |> snd |> List.rev

let rename_workspace conf conn ws =
  let open I3ipc in
  let open Reply in

  let ws_name = opt_def ~def:"N/A" ws.name in
  let ws_num = opt_def ~def:0 ws.num in

  Logs.debug (fun m -> m "====================================================");
  Logs.debug (fun m -> m "WORKSPACE NUMBER %d (%s)" ws_num ws_name);
  let leaves = extract_leaves ws in
  List.iter (fun (leaf : node) ->
    Logs.debug (fun m -> m "    %s; %s" leaf.id (opt_def ~def:"N/A" leaf.name));
  ) leaves;
  let new_name =
    leaves
    |> List.map (string_of_node conf)
    |> remove_dups
    |> String.concat "|" in
  let new_name =
    if new_name = ""
    then string_of_int ws_num
    else Printf.sprintf "%d: %s" ws_num new_name in
  if ws_name <> new_name then begin
    let cmd = spf "rename workspace \"%s\" to \"%s\"" ws_name new_name in
    Logs.debug (fun m -> m "SENDING COMMAND: %s" cmd);
    let%lwt outcomes = command conn cmd in
    List.iteri (fun i outcome ->
      Logs.debug (fun m -> m "    RESULT[%d].success = %s" (i+1) (string_of_bool outcome.success));
      Logs.debug (fun m -> m "    RESULT[%d].error   = %s" (i+1) (opt_def ~def:"" outcome.error))
    ) outcomes;

    Logs.debug (fun m -> m "----------------------------------------------------");
    Lwt.return ()
  end else Lwt.return ()

let manage_fullscreen n =
  if n = 0
  then begin
    match%lwt find_picom_pid () with
    | Some _pid -> Lwt.return_unit
    | None -> begin
      let%lwt status = Lwt_process.exec ("picom", [|"picom"; "-b"; "-f"; "-D"; "3"; "-C"; "-G"|]) in
      (match status with
      | Unix.WEXITED 0 -> Logs.info (fun m -> m "picom successfully started")
      | _ -> Logs.err (fun m -> m "Error while starting picom"));
      let%lwt status = Lwt_process.exec ("xset", [|"xset"; "s"; "300"; "300";|]) in
      (match status with
      | Unix.WEXITED 0 -> Logs.info (fun m -> m "xset screen saver set to 5 minutes")
      | _ -> Logs.err (fun m -> m "Error while setting screen saver"));
      let%lwt status = Lwt_process.exec ("xset", [|"xset"; "dpms"; "600"; "600"; "600"|]) in
      (match status with
      | Unix.WEXITED 0 -> Logs.info (fun m -> m "xset screen OFF in 10 minutes")
      | _ -> Logs.err (fun m -> m "Error while setting DPMS"));
      Lwt.return_unit
    end
  end
  else begin
    match%lwt find_picom_pid () with
    | Some pid -> begin
      Logs.info (fun m -> m "Some fullscreen windows: killing picom");
      Unix.kill pid 15;

      let%lwt status = Lwt_process.exec ("xset", [|"xset"; "s"; "off"; "-dpms"|]) in
      (match status with
      | Unix.WEXITED 0 -> Logs.info (fun m -> m "Screen saver and DPMS disabled")
      | _ -> Logs.err (fun m -> m "Error while turning blank screen off"));
      Lwt.return_unit
    end
    | None -> Lwt.return_unit
  end

let handle_win_event conf conn (event_info : I3ipc.Event.window_event_info) =
  let open I3ipc in

  let%lwt tree = get_tree conn in
  match event_info.Event.change with
  | Event.New | Close | Title | Move | FullscreenMode -> begin
    let ws_nodes = get_workspaces_nodes tree in
    let%lwt () = Lwt_list.iter_p (rename_workspace conf conn) ws_nodes in

    let open Reply in
    let windows_nodes = get_windows_nodes tree in
    let windows_in_fullscreen =
      ListLabels.fold_left windows_nodes ~init:0 ~f:(fun w_in_full w ->
        if w.fullscreen_mode <> No_fullscreen
        then (w_in_full + 1)
        else w_in_full
      ) in
    manage_fullscreen windows_in_fullscreen
  end
  | _ -> Lwt.return ()

let rec loop conf conn =
  let open I3ipc in
  let b = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer b in

  let ev = Lwt.choose [
    Lwt.map (fun e -> `I3_event e) (next_event conn);
    Lwt.map (fun _ -> `Shutdown) shutdown;
  ] in

  match%lwt ev with
  | `I3_event ev -> begin
    let%lwt () = match ev with
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
      | _ -> Lwt.return () in
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
  with I3ipc.Protocol_error e -> begin
    handle_protocol_error e;
    if not (Lwt.is_sleeping shutdown) then begin
      Logs.info (fun m -> m "i3 shutdown, exiting");
      Lwt.return ()
    end else begin
      Logs.info (fun m -> m "i3 is restarting, wait a second...");
      let%lwt () = Lwt_unix.sleep 1.0 in
      let%lwt conn = connect_and_subscribe () in
      Logs.debug (fun m -> m "...reconnected to i3");
      protected_loop conf conn
    end
  end

let rec gc_loop () =
  let open Gc in
  let path = "/proc/" ^ (Unix.getpid () |> string_of_int) ^ "/status" in
  let%lwt () = Lwt_unix.sleep 60.0 in
  Logs.debug (fun m -> m "GARBAGE COLLECTION LOOP");
  let stat' = stat () in
  let%lwt memstats = Utils.get_meminfo ~path () in
  Logs.debug (fun m -> m "BEFORE GC: heap_words = %d; live_words = %d" stat'.heap_words stat'.live_words);
  Logs.debug (fun m -> m "           VmRSS = %d" (BatMap.String.find "VmRSS" memstats));
  compact ();
  let stat' = stat () in
  let%lwt memstats = Utils.get_meminfo ~path () in
  Logs.debug (fun m -> m "AFTER  GC: heap_words = %d; live_words = %d" stat'.heap_words stat'.live_words);
  Logs.debug (fun m -> m "           VmRSS = %d" (BatMap.String.find "VmRSS" memstats));
  gc_loop ()

let rec clear_color_profile () =
  let%lwt res = Lwt_process.exec ("/usr/bin/xprop", [|"xprop"; "-root"; "-remove"; "_ICC_PROFILE" |]) in
  let () =
    match res with
    | Unix.WEXITED 0 -> Logs.debug (fun m -> m "Color profile cleared")
    | _ -> Logs.err (fun m -> m "Error while clearing color profile") in
  let%lwt () = Lwt_unix.sleep 3.0 in
  clear_color_profile ()

let _ = Lwt_unix.on_signal Sys.sigterm (fun s -> Lwt.wakeup do_shutdown s)

let main _unique verbose log_fname conf_fname otp_conf_fname state_fname =
  Logs.set_reporter (Reporter.lwt_file_reporter (Some log_fname));
  if verbose
  then Logs.set_level (Some Logs.Debug)
  else Logs.set_level (Some Logs.Info);

  let%lwt conf = Conf.read_icons_configuration conf_fname in
  let%lwt otp_conf = Conf.read_otp_configuration otp_conf_fname in
  Conf.otp_global_configuration := otp_conf;

  Lwt.async gc_loop;
  Lwt.async clear_color_profile;
  let status_completed = I3_status.entry_point state_fname shutdown in
  let%lwt conn = connect_and_subscribe () in
  let%lwt _ = Lwt.all [protected_loop conf conn; status_completed ()] in
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

let log_fname_def = (Unix.getenv "HOME")/".cache/i3-ws-rename/log.txt"
let log_fname =
  let doc = "Position of the log file." in
  Arg.(value & opt string log_fname_def & info ["l"; "log"] ~docv:"LOG" ~doc)

let conf_fname_def = get_default_conf_fname "app-icons.json"
let conf_fname =
  let doc = "Specifies an alternate configuration file path." in
  Arg.(value & opt string conf_fname_def & info ["c"; "conf"] ~docv:"CONFIGURATION" ~doc)

let otp_conf_fname_def = get_default_conf_fname "otp.json"
let otp_conf_fname =
  let doc = "Specifies an alternate configuration file for the OTP secrets." in
  Arg.(value & opt string otp_conf_fname_def & info ["o"; "otp"] ~docv:"OTP" ~doc)


let state_fname_def = get_default_conf_fname "state.json"
let state_fname =
  let doc = "Specifies an alternate state file path." in
  Arg.(value & opt string state_fname_def & info ["s"; "state"] ~docv:"STATE" ~doc)

let info =
  let doc = "Dynamically update i3wm workspace names based on running applications in each and optionally define an icon to show instead." in
  let man = [
    `S Manpage.s_bugs;
    `P "Bug reports on GitHub: https://github.com/pdonadeo/i3-ws-rename/issues" ]
  in
  Term.info "%%NAME%%" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let main' u daemon verbose log_fname conf_fname otp_conf_fname state_fname =
  let cd = Unix.getcwd () in
  let log_fname =
    if Filename.is_relative log_fname
    then cd/log_fname
    else log_fname in

  let conf_fname =
    if conf_fname = ""
    then None
    else begin
      if file_exists_and_is_readable conf_fname
      then Some conf_fname
      else begin
        Printf.eprintf "ERROR: file \"%s\" does not exist or not readable.\n%!" conf_fname;
        exit 1;
      end
    end in

  let otp_conf_fname =
    if otp_conf_fname = ""
    then None
    else begin
      if file_exists_and_is_readable otp_conf_fname
      then Some otp_conf_fname
      else begin
        Printf.eprintf "ERROR: file \"%s\" does not exist or not readable.\n%!" otp_conf_fname;
        exit 1;
      end
    end in

  mkdir_p (Filename.dirname log_fname) 0o755;

  if daemon then daemonize ~cd ();
  Lwt_glib.install ();
  Lwt_main.run (main u verbose log_fname conf_fname otp_conf_fname state_fname)

let main_t = Term.(const main' $ unique $ daemon $ verbose $ log_fname $ conf_fname $ otp_conf_fname $ state_fname)

let () = Printexc.record_backtrace true

let () = Gc.set { (Gc.get ()) with
    Gc.allocation_policy = 2;
    Gc.space_overhead = 85;
  }

let () = Term.exit @@ Term.eval (main_t, info)
