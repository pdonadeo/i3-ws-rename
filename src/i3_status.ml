open Base
open Utils
open Modules_inventory


let header_block = "{ \"version\": 1, \"stop_signal\": 20, \"cont_signal\": 18, \"click_events\": true }"

let pipe : (message_to_status, [< `r | `w ]) Lwt_pipe.t = Lwt_pipe.create ~max_size:10 ()

let modules_configuration  = [
  (
    (new Disk_usage.modulo "/" pipe : [ `r | `w] Lwt_module.modulo),
    "/",
    true
  );
  (
    (new Disk_usage.modulo "/home" pipe : [ `r | `w] Lwt_module.modulo),
    "/home",
    true
  );
  (
    (new Clock.modulo "0" pipe : [ `r | `w] Lwt_module.modulo),
    "0",
    true
  )
]

let entry_point () =
  let%lwt () = Lwt_io.printf "I3_statuss.entry_point\n%!" in

  let%lwt running_instances = Lwt_list.fold_left_s (fun map m ->
    let modulo, instance, separator = m in
    ignore separator; (* TODO *)

    let%lwt () = modulo#run () in
    Lwt.return (StringTuple2Map.add (modulo#name, instance) modulo map)
  ) StringTuple2Map.empty modules_configuration in

  let%lwt () = Lwt_io.write_line Lwt_io.stdout header_block in
  let%lwt () = Lwt_io.write_line Lwt_io.stdout "[" in

  let rec loop () =
    match%lwt Lwt_pipe.read_with_timeout pipe ~timeout:(Some 1.0) with
    | Timeout -> begin
      loop ()
    end
    | Data_available t -> begin
      match t with
      | `Status_change (name, instance_name) -> begin
        let%lwt () = Lwt_io.printf "Update from %s/%s\n%!" name instance_name in
        let mod_ = StringTuple2Map.find (name, instance_name) running_instances in
        let%lwt () = Lwt_io.printf "    JSON = %s\n%!" (mod_#json ()) in
        loop ()
      end
    end
    | Pipe_closed
    | Nothing_available -> Lwt.return ()
  in

  loop ()