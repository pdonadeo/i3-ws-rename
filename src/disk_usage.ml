let delay = 5.0

let df () =
  let open Base in
  let open String in

  let init = Map.empty (module String) in
  try%lwt
    let%lwt res = Lwt_process.pread ("/usr/bin/df", [|"df"; "-k"|]) in
    let lines = split_lines res |> List.tl_exn in
    let init = Map.empty (module String) in
    let map = List.fold lines ~init ~f:(fun m line ->
      let usage_mount = List.split_n (split ~on:' ' line |> List.filter ~f:((<>) "")) 4 |> snd in
      let usage = List.nth_exn usage_mount 0 |> strip ~drop:(Char.equal '%') |> Int.of_string in
      let mount = List.nth_exn usage_mount 1 in
      Map.add_exn m ~key:mount ~data:usage
    ) in
    Lwt.return map
  with _ -> Lwt.return init

class ['a] modulo instance_name status_pipe : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "disk"
    val mutable state = None

    method! private loop () =
      let%lwt res = df () in
      let usage =
        match Base.Map.find res instance_name with
        | Some u -> u
        | None -> 0 in

      let%lwt result =
        if state <> (Some usage) then begin
          state <- Some usage;
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end else Lwt.return true in

      let%lwt () = Lwt_unix.sleep delay in
      if result = true
      then self#loop ()
      else Lwt.return ()

    method! json () =
      match state with
      | None -> "{}"
      | Some s -> Printf.sprintf "{ %d }" s
end
