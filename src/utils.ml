module StringTuple2 = struct
  type t = string * string
  let compare (x1, y1) (x2, y2) =
    if compare x1 x2 = 0
    then compare y1 y2
    else compare x1 x2
end

module StringTuple2Map = BatMap.Make(StringTuple2)

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
  else (
    let rec loop n t accum =
      if n = 0
      then List.rev accum, t
      else (
        match t with
        | [] -> xs, [] (* in this case, xs = rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum))
    in
    loop n xs [] |> fst
  )

let avg xs =
  let sum = ListLabels.fold_left xs ~init:0.0 ~f:(fun s v -> s +. v) in
  sum /. (List.length xs |> Stdlib.float_of_int)

let rec traverse_deep_first_aux (f : 'a -> int -> I3ipc.Reply.node -> I3ipc.Reply.node option -> 'a) (acc : 'a) ?(parent=None) ?(depth=0) (n : I3ipc.Reply.node) : 'a =
  let acc' = f acc depth n parent in
  let children = n.nodes @ n.floating_nodes in
  List.fold_left (fun acc -> traverse_deep_first_aux f acc ~parent:(Some n) ~depth:(depth + 1)) acc' children

let traverse_deep_first f acc n =
  traverse_deep_first_aux f acc n

let all_nodes n =
  traverse_deep_first (fun acc depth node _ ->
    (node, depth)::acc
  ) [] n |> List.rev

let extract_leaves root =
  traverse_deep_first (fun acc _depth n _parent ->
    if List.length (List.rev_append n.nodes n.floating_nodes) = 0 && n.nodetype <> Workspace
    then n::acc else acc
  ) [] root |> List.rev

let rec mkdir dirname perm =
  try Unix.mkdir dirname perm
  with Unix.Unix_error (EINTR, _, _) -> mkdir dirname perm

let rec mkdir_p dir perm =
  let open Unix in
  let mkdir_idempotent dir perm =
    match mkdir dir perm with
    | () -> ()
    | exception Unix_error ((EEXIST | EISDIR), _, _) -> () in

  match mkdir_idempotent dir perm with
  | () -> ()
  | exception ((Unix_error (ENOENT, _, _)) as exn) -> begin
    let parent = Filename.dirname dir in
    if parent = dir then raise exn
    else
      (mkdir_p parent perm;
       mkdir_idempotent dir perm)
  end

let (/) a b = Filename.concat a b

let file_readable f =
  try open_in f |> close_in; true
  with Sys_error _ -> false

let file_exists_and_is_readable f =
  if (Sys.file_exists f && not (Sys.is_directory f))
  then file_readable f
  else false

let get_default_conf_fname fname =
  let getenv s = try (Some (Unix.getenv s)) with Not_found -> None in

  let skip_xdg_config_home () =
    let home = Unix.getenv "HOME" in
    let c = home/".config"/"i3"/fname in
    if file_exists_and_is_readable c
    then c
    else begin
      let c = home/".i3"/fname in
      if file_exists_and_is_readable c
      then c else ""
    end
  in

  match getenv "XDG_CONFIG_HOME" with
  | Some xdg_config_home -> begin
    let c = xdg_config_home/"i3"/fname in
    if file_exists_and_is_readable c
    then c
    else skip_xdg_config_home ()
  end
  | None -> skip_xdg_config_home ()

module StringTuple2Map_Json = struct
  type el = {
    key : string * string;
    data : string
  } [@@deriving yojson, show]

  type t = el list [@@deriving yojson, show]

  let map_to_json (m : string StringTuple2Map.t) =
    let list = StringTuple2Map.fold (fun key data acc -> { key; data }::acc) m [] in
    to_yojson list

  let json_to_map (j : Yojson.Safe.t) : string StringTuple2Map.t =
    let list_or_error = of_yojson j in
    match list_or_error with
    | Ok list -> begin
      ListLabels.fold_left list ~init:StringTuple2Map.empty ~f:(fun acc el ->
        StringTuple2Map.add el.key el.data acc
      )
    end
    | Error _ -> begin
      Logs.err (fun m -> m "Error parsing message serialized state");
      failwith "Error parsing message serialized state"
    end

  let map_from_fname fname =
    try%lwt
      Lwt_io.with_file fname ~mode:Lwt_io.input (fun f ->
        let%lwt state_str = Lwt_io.read f in
        Yojson.Safe.from_string state_str |> json_to_map |> Result.ok |> Lwt.return
      )
    with
    | exn -> begin
      Logs.err (fun m -> m "Error loading state");
      let open Printexc in
      let exn_str = to_string exn in
      let backtrace = get_backtrace () in
      Logs.info (fun m -> m "Exception while loading state\n\n%s\n%s" exn_str backtrace);
      Result.error () |> Lwt.return
    end

    let map_to_file state_fname state =
      try%lwt
        Lwt_io.with_file state_fname ~mode:Lwt_io.output (fun f ->
          let state_str = to_yojson state |> Yojson.Safe.pretty_to_string in
          Lwt_io.write f state_str
        )
      with
      | exn -> begin
        Logs.err (fun m -> m "Error saving state");
        let open Printexc in
        let exn_str = to_string exn in
        let backtrace = get_backtrace () in
        Logs.info (fun m -> m "Exception while saving state\n\n%s\n%s" exn_str backtrace);
        Lwt.return_unit
      end
end

let get_meminfo ?(path="/proc/meminfo") () =
  let open Lwt_io in
  with_file ~flags:[O_RDONLY] ~mode:Input path (fun ic ->
    let%lwt lines = read_lines ic |> Lwt_stream.to_list in

    let stats = ListLabels.fold_left ~init:BatMap.String.empty ~f:(fun stats line ->
      let tokens =
        BatString.replace_chars (function | '\t' -> " " | x -> String.make 1 x) line
        |> BatString.split_on_char ' '
        |> ListLabels.filter ~f:(fun t -> if String.equal t "" then false else true)
        |> Array.of_list
      in
      let name = tokens.(0) |> BatString.strip ~chars:":" in
      try
        let val_kb = tokens.(1) |> int_of_string in
        BatMap.String.add name val_kb stats
      with _ -> stats
    ) lines in
    Lwt.return stats
  )

type temp_range =
  | Low of float
  | Normal of float
  | High of float
  | Hot of float

let int_of_temp_range r =
  match r with
  | Low _ -> 0
  | Normal _ -> 1
  | High _ -> 2
  | Hot _ -> 3

let temp_range_of_int t idx =
  match idx with
  | 0 -> Low t
  | 1 -> Normal t
  | 2 -> High t
  | 3 -> Hot t
  | _ -> raise (Invalid_argument "temp_range_of_int")

let get_temp = function
  | Low v -> v
  | Normal v -> v
  | High v -> v
  | Hot v -> v

let get_range thresholds v =
  if v <= thresholds.(1)
  then (Low v)
  else if thresholds.(1) < v && v <= thresholds.(3)
  then (Normal v)
  else if thresholds.(3) < v && v <= thresholds.(4)
  then (High v)
  else (Hot v)

let not_same_range t1 t2 = (int_of_temp_range t1) <> (int_of_temp_range t2)

let get_hwmon_base_dir name =
  let open Lwt_io in
  let open Lwt_unix in
  let (/) x y = x ^ "/" ^ y in
  let base = "/sys/class/hwmon" in
  let%lwt dir = opendir base in
  let%lwt entries = Lwt_unix.readdir_n dir 100 in
  let%lwt () = closedir dir in
  Array.sort String.compare entries;
  let entries = Array.to_list entries |> ListLabels.filter ~f:(fun el -> el.[0] <> '.') in
  let%lwt res =
    Lwt_list.fold_left_s (fun acc e ->
      with_file ~flags:[O_RDONLY] ~mode:Input (base/e/"name") (fun ic ->
        let%lwt e_name = read_line ic in
        if e_name = name then Lwt.return (Some (base/e)) else Lwt.return acc
      )
    ) None entries in

  Lwt.return res

let get_hwmon_file_map name =
  try%lwt
    let open Lwt_io in
    let open Lwt_unix in

    let%lwt base = get_hwmon_base_dir name in
    let base = Option.get base in

    let%lwt dir = opendir base in
    let%lwt entries = Lwt_unix.readdir_n dir 100 in
    let%lwt () = closedir dir in

    Array.sort String.compare entries;
    let entries = Array.to_list entries |> ListLabels.filter ~f:(fun el -> el.[0] <> '.') in

    let%lwt ret_map =
      Lwt_list.fold_left_s (fun map fname ->
        if (BatString.starts_with fname "temp") && (BatString.ends_with fname "_label")
        then begin
          let b = BatString.split_on_string ~by:"_" fname |> List.hd in
          let%lwt label = with_file ~flags:[O_RDONLY] ~mode:Input (base/fname) read_line in
          BatMap.String.add label (base/(b^"_input")) map |> Lwt.return
        end
        else Lwt.return map
      ) BatMap.String.empty entries in
    Lwt.return (`Ok ret_map)
  with _ -> Lwt.return `Device_not_found

let read_temperatures file_map =
  let open Lwt_io in
  let open Lwt_unix in

  let lab_fname_l = BatMap.String.fold (fun k v a -> (k, v)::a) file_map [] |> List.rev in
  Lwt_list.fold_left_s (fun map (label, fname) ->
    let%lwt value = with_file ~flags:[O_RDONLY] ~mode:Input fname read_line in
    let value = (float_of_string value) /. 1000. in
    BatMap.String.add label value map |> Lwt.return
  ) BatMap.String.empty lab_fname_l

let get_user_process_list () =
  let open Lwt_process in
  let%lwt ps_out = pread ("ps", [|"/usr/bin/ps"; "x"; "-o"; "pid,cmd"|]) in
  let lines = String.split_on_char '\n' ps_out
    |> ListLabels.map ~f:String.trim
    |> ListLabels.filter ~f:(fun l -> if l = "" then false else true)
    |> List.tl in
  let processes =
    ListLabels.map lines ~f:(fun l ->
      let tokens = String.split_on_char ' ' l in
      let pid = List.hd tokens |> int_of_string in
      let cmd = String.concat " " (List.tl tokens) in
      pid, cmd
    ) in

  Lwt.return processes

let picom_regex = Str.regexp "^picom"

let find_picom_pid () =
  let%lwt processes = get_user_process_list () in
  ListLabels.find_map processes ~f:(fun (pid, cmd) ->
    if Str.string_match picom_regex cmd 0
    then Some pid
    else None
  ) |> Lwt.return

let read_hdd_temp () =
  let open Unix in
  let open Lwt_unix in

  let s = socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let buf = Bytes.make 256 '\000' in

  let rec loop acc =
    let%lwt received = recv s buf 0 256 [] in
    if received = 0
    then Lwt.return (String.concat "" (List.rev acc))
    else begin
      let data = Bytes.sub buf 0 received in
      let data_s = Bytes.to_string data in
      loop (data_s::acc)
    end in

  try%lwt
    let%lwt () = connect s (ADDR_INET ((inet_addr_of_string "127.0.0.1"), 7634)) in

    let%lwt data = loop [] in
    let temp = List.nth (String.split_on_char '|' data) 3 |> float_of_string in
    let%lwt () = close s in
    Lwt.return (Ok temp)
  with
  | exn -> begin
    let%lwt () = close s in
    Lwt.return (Error (Printexc.to_string exn))
  end

let get_or_timeout ?(timeout=1.0) uri =
  let open Lwt.Infix in
  let uri = Uri.of_string uri in
  let timeout =
    Lwt_unix.sleep timeout >>= fun () ->
    Lwt.return `Timeout in
  let get =
    try%lwt (
      Cohttp_lwt_unix.Client.get uri >>= fun response ->
      Lwt.return (`Response response)
    ) with exn -> Lwt.return (`Exception exn) in
  Lwt.pick [ timeout; get ]

let string_of_signal s =
  match s with
  | -1 -> "SIGABRT" | -2 -> "SIGALRM" | -3 -> "SIGFPE" | -4 -> "SIGHUP"
  | -5 -> "SIGILL" | -6 -> "SIGINT" | -7 -> "SIGKILL" | -8 -> "SIGPIPE"
  | -9 -> "SIGQUIT" | -10 -> "SIGSEGV" | -11 -> "SIGTERM" | -12 -> "SIGUSR1"
  | -13 -> "SIGUSR2" | -14 -> "SIGCHLD" | -15 -> "SIGCONT" | -16 -> "SIGSTOP"
  | -17 -> "SIGTSTP" | -18 -> "SIGTTIN" | -19 -> "SIGTTOU" | -20 -> "SIGVTALRM"
  | -21 -> "SIGPROF" | -22 -> "SIGBUS" | -23 -> "SIGPOLL" | -24 -> "SIGSYS"
  | -25 -> "SIGTRAP" | -26 -> "SIGURG" | -27 -> "SIGXCPU" | -28 -> "SIGXFSZ"
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
      | Error e -> failwith (Printf.sprintf "Parse error: %s" e) (* TODO *)
    )
  | None -> Lwt.return []
