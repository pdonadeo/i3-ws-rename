module StringMap = Map.Make(String)

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
    if List.length (List.rev_append n.nodes n.floating_nodes) > 0
    then acc else n::acc
  ) [] root |> List.rev

let get_workspaces_nodes root =
  traverse_deep_first (fun acc _depth n _parent ->
    let open I3ipc in
    let open Reply in
    if n.nodetype = Workspace && (opt_def ~def:(-1) n.num) > 0
    then n::acc else acc
  ) [] root |> List.rev

let string_of_node conf (node : I3ipc.Reply.node) =
  let open I3ipc in
  let open Reply in

  match node.window_properties with
  | None -> None
  | Some wp -> begin
    match wp.class_, wp.instance with
    | None, None -> None
    | None, Some i -> Some i
    | Some c, None -> begin
      Conf.search_class conf (String.lowercase_ascii c)
    end
    | Some c, Some _i -> begin
      Conf.search_class conf (String.lowercase_ascii c)
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

let rename_workspace conf conn w =
  let open I3ipc in
  let open Reply in

  let w_name = opt_def ~def:"N/A" w.name in
  let w_num = opt_def ~def:0 w.num in

  Logs.debug (fun m -> m "====================================================");
  Logs.debug (fun m -> m "WORKSPACE NUMBER %d (%s)" w_num w_name);
  let leaves = extract_leaves w in
  List.iter (fun (leaf : node) ->
    Logs.debug (fun m -> m "    %s; %s" leaf.id (opt_def ~def:"N/A" leaf.name));
  ) leaves;
  let new_name =
    List.map (string_of_node conf) leaves
    |> List.map (opt_def ~def:"N/A")
    |> remove_dups
    |> String.concat "|"
    |> Printf.sprintf "%d: %s" w_num in
  if w_name <> new_name then begin
    let cmd = spf "rename workspace \"%s\" to \"%s\"" w_name new_name in
    Logs.debug (fun m -> m "SENDING COMMAND: %s" cmd);
    let%lwt outcomes = command conn cmd in
    List.iteri (fun i outcome ->
      Logs.debug (fun m -> m "    RESULT[%d].success = %s" (i+1) (string_of_bool outcome.success));
      Logs.debug (fun m -> m "    RESULT[%d].error   = %s" (i+1) (opt_def ~def:"" outcome.error))
    ) outcomes;

    Logs.debug (fun m -> m "----------------------------------------------------");
    Lwt.return ()
  end else Lwt.return ()
