module StringMap = Map.Make(String)

module StringTuple2 = struct
  open Core_kernel

  include Tuple.Make       (String) (String)
  include Tuple.Comparable (String) (String)
end

module StringTuple2Map = Map.Make(StringTuple2)

let spf = Printf.sprintf

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

let get_default_conf_fname () =
  let fname = "app-icons.json" in
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

