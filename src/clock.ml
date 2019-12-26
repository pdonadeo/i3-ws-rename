open Modules_inventory


type t = module_instance

let delay = 0.5

let now () =
  let open Unix in
  let { tm_min; tm_hour; tm_sec; _ } = gettimeofday () |> localtime in
  (* ignore tm_sec; *)
  Lwt.return (Printf.sprintf "%02d:%02d:%02d" tm_hour tm_min tm_sec)

let name = "clock"

let state = ref None

let rec loop p instance () =
  let%lwt n = now () in

  let%lwt result =
    match !state with
    | None -> begin
      state := Some n;
      Lwt_pipe.write p (`Status_change (name, instance))
    end
    | Some old_n -> begin
      if n <> old_n then begin
        state := Some n;
        Lwt_pipe.write p (`Status_change (name, instance))
      end
      else Lwt.return true
    end in

  if result then begin
    let%lwt () = Lwt_unix.sleep delay in
    loop p instance ()
  end
  else Lwt.return ()

let name = "clock"

let json () =
  match !state with
  | None -> "{}"
  | Some t -> Printf.sprintf "{ %s }" t

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