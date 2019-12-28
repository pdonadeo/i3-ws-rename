open Base
open Utils
open Modules_inventory


let header_block = "{ \"version\": 1, \"stop_signal\": 20, \"cont_signal\": 18, \"click_events\": true }"

let pipe : (message_to_status, [< `r | `w ]) Lwt_pipe.t = Lwt_pipe.create ~max_size:10 ()

let color = "#c5c8c6"
let color_good = "#39B258"
let color_degraded = "#CC9246"
let color_bad = "#9B51FF"
let color_separator = "#C5C8C6"

let modules : [ `r | `w] Lwt_module.modulo list = [
  new Disk_usage.modulo "/" pipe color color_degraded color_bad false;
  new Disk_usage.modulo "/home" pipe color color_degraded color_bad true;
  new Load_avg.modulo "0" pipe color_good color_degraded color_bad false;
  new Clock.modulo "0" pipe color_degraded true;
]

let entry_point () =
  let%lwt running_instances = Lwt_list.fold_left_s (fun map (modulo : [ `r | `w] Lwt_module.modulo) ->
    let%lwt () = modulo#run () in
    Lwt.return (StringTuple2Map.add (modulo#name, modulo#instance) modulo map)
  ) StringTuple2Map.empty modules in

  ignore running_instances; (* TODO *)

  let%lwt () = Lwt_io.write_line Lwt_io.stdout header_block in
  let%lwt () = Lwt_io.write_line Lwt_io.stdout "[" in

  let rec loop () =
    match%lwt Lwt_pipe.read_with_timeout pipe ~timeout:(Some 1.0) with
    | Timeout -> begin
      loop ()
    end
    | Data_available t -> begin
      match t with
      | `Status_change (_name, _instance_name) -> begin
        let%lwt () = Lwt_io.printf "[" in
        let%lwt blocks = Lwt_list.map_p (fun mod_ -> mod_#json ()) modules in
        let%lwt () = Lwt_io.printf "%s" (String.concat ~sep:"," blocks) in
        let%lwt () = Lwt_io.printf "]," in
        let%lwt () = Lwt_io.(flush stdout) in
        loop ()
      end
    end
    | Pipe_closed
    | Nothing_available -> Lwt.return ()
  in

  loop ()