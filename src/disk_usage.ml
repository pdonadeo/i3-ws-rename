open Modules_inventory


type t = module_instance

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
      let usage = List.nth_exn usage_mount 0 |> strip ~drop:(Char.equal '%') in
      let mount = List.nth_exn usage_mount 1 in
      Map.add_exn m ~key:mount ~data:usage
    ) in
    Lwt.return map
  with _ -> Lwt.return init

let name = "disk"

let state = ref None

let rec loop p instance () =
  let%lwt res = df () in
  let usage =
    match Base.Map.find res instance with
    | Some u -> u
    | None -> "N/A" in
  state := Some usage;
  let%lwt () = Lwt_unix.sleep delay in
  let%lwt result = Lwt_pipe.write p (`Status_change (name, instance)) in
  if result = true
  then loop p instance ()
  else Lwt.return ()

let json () =
  match !state with
  | None -> "{}"
  | Some s -> Printf.sprintf "{ %s }" s

let create p instance =
  let this = {
    name = name;
    instance;
    json
  } in
  let ready = Lwt.wait () in
  Lwt.async (loop p instance);
  Lwt.wakeup (snd ready) this;
  fst ready