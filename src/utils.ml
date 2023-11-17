module StringTuple2 = struct
  type t = string * string

  let compare (x1, y1) (x2, y2) = if compare x1 x2 = 0 then compare y1 y2 else compare x1 x2
end

module StringTuple2Map = BatMap.Make (StringTuple2)

let connect_and_subscribe () =
  let open I3ipc in
  let%lwt conn = connect () in
  let events = [Window; Shutdown] in
  let%lwt _ = subscribe conn events in
  Lwt.return conn

let opt_def ~def o =
  match o with
  | Some v -> v
  | None -> def

let opt_map o ~f =
  match o with
  | None -> None
  | Some v -> Some (f v)

let first_n n xs =
  if n <= 0
  then []
  else
    let rec loop n t accum =
      if n = 0
      then (List.rev accum, t)
      else
        match t with
        | [] -> (xs, [] (* in this case, xs = rev accum *))
        | hd :: tl -> loop (n - 1) tl (hd :: accum)
    in
    loop n xs [] |> fst

let avg xs =
  let sum = ListLabels.fold_left xs ~init:0.0 ~f:(fun s v -> s +. v) in
  sum /. (List.length xs |> Stdlib.float_of_int)

let rec traverse_deep_first_aux (f : 'a -> int -> I3ipc.Reply.node -> I3ipc.Reply.node option -> 'a) (acc : 'a)
    ?(parent = None) ?(depth = 0) (n : I3ipc.Reply.node) : 'a =
  let acc' = f acc depth n parent in
  let children = n.nodes @ n.floating_nodes in
  List.fold_left (fun acc -> traverse_deep_first_aux f acc ~parent:(Some n) ~depth:(depth + 1)) acc' children

let traverse_deep_first f acc n = traverse_deep_first_aux f acc n
let all_nodes n = traverse_deep_first (fun acc depth node _ -> (node, depth) :: acc) [] n |> List.rev

let extract_leaves root =
  traverse_deep_first
    (fun acc _depth n _parent ->
      if List.length (List.rev_append n.nodes n.floating_nodes) = 0 && n.nodetype <> Workspace then n :: acc else acc)
    []
    root
  |> List.rev

let rec mkdir dirname perm = try Unix.mkdir dirname perm with Unix.Unix_error (EINTR, _, _) -> mkdir dirname perm

let rec mkdir_p dir perm =
  let open Unix in
  let mkdir_idempotent dir perm =
    match mkdir dir perm with
    | () -> ()
    | exception Unix_error ((EEXIST | EISDIR), _, _) -> ()
  in

  match mkdir_idempotent dir perm with
  | () -> ()
  | exception (Unix_error (ENOENT, _, _) as exn) -> begin
    let parent = Filename.dirname dir in
    if parent = dir
    then raise exn
    else (
      mkdir_p parent perm;
      mkdir_idempotent dir perm)
  end

let ( / ) a b = Filename.concat a b

let file_readable f =
  try
    open_in f |> close_in;
    true
  with Sys_error _ -> false

let file_exists_and_is_readable f = if Sys.file_exists f && not (Sys.is_directory f) then file_readable f else false

let get_default_conf_fname fname =
  let getenv s = try Some (Unix.getenv s) with Not_found -> None in

  let skip_xdg_config_home () =
    let home = Unix.getenv "HOME" in
    let c = home / ".config" / "i3" / fname in
    if file_exists_and_is_readable c
    then c
    else begin
      let c = home / ".i3" / fname in
      if file_exists_and_is_readable c then c else ""
    end
  in

  match getenv "XDG_CONFIG_HOME" with
  | Some xdg_config_home -> begin
    let c = xdg_config_home / "i3" / fname in
    if file_exists_and_is_readable c then c else skip_xdg_config_home ()
  end
  | None -> skip_xdg_config_home ()

module StringTuple2Map_Json = struct
  type el = {
    key : string * string;
    data : string;
  }
  [@@deriving yojson, show]

  type t = el list [@@deriving yojson, show]

  let map_to_json (m : string StringTuple2Map.t) =
    let list = StringTuple2Map.fold (fun key data acc -> { key; data } :: acc) m [] in
    to_yojson list

  let json_to_map (j : Yojson.Safe.t) : string StringTuple2Map.t =
    let list_or_error = of_yojson j in
    match list_or_error with
    | Ok list -> begin
      ListLabels.fold_left list ~init:StringTuple2Map.empty ~f:(fun acc el -> StringTuple2Map.add el.key el.data acc)
    end
    | Error _ -> begin
      Logs.err (fun m -> m "Error parsing message serialized state");
      failwith "Error parsing message serialized state"
    end

  let map_from_fname fname =
    try%lwt
      Lwt_io.with_file fname ~mode:Lwt_io.input (fun f ->
          let%lwt state_str = Lwt_io.read f in
          Yojson.Safe.from_string state_str |> json_to_map |> Result.ok |> Lwt.return)
    with exn ->
      Logs.err (fun m -> m "Error loading state");
      let open Printexc in
      let exn_str = to_string exn in
      let backtrace = get_backtrace () in
      Logs.info (fun m -> m "Exception while loading state\n\n%s\n%s" exn_str backtrace);
      Result.error () |> Lwt.return

  let map_to_file state_fname state =
    try%lwt
      Lwt_io.with_file state_fname ~mode:Lwt_io.output (fun f ->
          let state_str = to_yojson state |> Yojson.Safe.pretty_to_string in
          Lwt_io.write f state_str)
    with exn ->
      Logs.err (fun m -> m "Error saving state");
      let open Printexc in
      let exn_str = to_string exn in
      let backtrace = get_backtrace () in
      Logs.info (fun m -> m "Exception while saving state\n\n%s\n%s" exn_str backtrace);
      Lwt.return_unit
end

let string_of_signal s =
  match s with
  | -1 -> "SIGABRT"
  | -2 -> "SIGALRM"
  | -3 -> "SIGFPE"
  | -4 -> "SIGHUP"
  | -5 -> "SIGILL"
  | -6 -> "SIGINT"
  | -7 -> "SIGKILL"
  | -8 -> "SIGPIPE"
  | -9 -> "SIGQUIT"
  | -10 -> "SIGSEGV"
  | -11 -> "SIGTERM"
  | -12 -> "SIGUSR1"
  | -13 -> "SIGUSR2"
  | -14 -> "SIGCHLD"
  | -15 -> "SIGCONT"
  | -16 -> "SIGSTOP"
  | -17 -> "SIGTSTP"
  | -18 -> "SIGTTIN"
  | -19 -> "SIGTTOU"
  | -20 -> "SIGVTALRM"
  | -21 -> "SIGPROF"
  | -22 -> "SIGBUS"
  | -23 -> "SIGPOLL"
  | -24 -> "SIGSYS"
  | -25 -> "SIGTRAP"
  | -26 -> "SIGURG"
  | -27 -> "SIGXCPU"
  | -28 -> "SIGXFSZ"
  | _ -> "UNKNOWN SIGNAL"

let read_json_configuration decoder fname =
  match fname with
  | Some fname ->
    Lwt_io.with_file fname ~mode:Lwt_io.input (fun f ->
        let%lwt conf_str = Lwt_io.read f in
        let conf_j = Yojson.Safe.from_string conf_str in
        let conf_or_error = decoder conf_j in
        match conf_or_error with
        | Ok conf -> begin
          Logs.debug (fun m -> m "Configuration successfully loaded from %s" fname);
          Lwt.return conf
        end
        | Error e -> failwith (Printf.sprintf "Parse error: %s" e)
        (* TODO *))
  | None -> Lwt.return []

let detach_promise ?(restart = false) f (handler_label : string) =
  let rec handler exn =
    Logs.err (fun p -> p "Exception in detached promise %s" handler_label);
    Logs.err (fun p -> p "    %s" (Printexc.to_string exn));
    Printexc.get_backtrace ()
    |> String.split_on_char '\n'
    |> List.filter (fun l -> if String.trim l = "" then false else true)
    |> List.iter (fun l -> Logs.err (fun p -> p "    %s" l));
    if restart
    then
      Lwt.dont_wait
        (fun () ->
          Logs.err (fun p -> p "Waiting 5 seconds and restarting %s" handler_label);
          let%lwt () = Lwt_unix.sleep 5.0 in
          f ())
        handler
  in
  Lwt.dont_wait f handler
