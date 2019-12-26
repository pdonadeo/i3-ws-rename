open Base
open Utils
open Modules_inventory


let header_block = "{ \"version\": 1, \"stop_signal\": 20, \"cont_signal\": 18, \"click_events\": true }"

type module_instance = {
  module_ : (module STATUS_MODULE);
  instance : string;
  separator : bool;
}

let modules_configuration  = [
  {
    module_ = (module Disk_usage : STATUS_MODULE);
    instance = "/";
    separator = true;
  };
  {
    module_ = (module Disk_usage : STATUS_MODULE);
    instance = "/home";
    separator = true;
  };
  {
    module_ = (module Clock : STATUS_MODULE);
    instance = "0";
    separator = true;
  };
]

let entry_point () =
  let%lwt () = Lwt_io.printf "I3_statuss.entry_point\n%!" in
  let pipe = Lwt_pipe.create ~max_size:10 () in

  let%lwt running_instances = Lwt_list.fold_left_s (fun map m ->
    let {
      module_;
      instance;
      separator;
    } = m in
    ignore separator; (* TODO *)
    let module M = (val module_ : STATUS_MODULE) in
    let%lwt inst = M.create pipe instance in
    Lwt.return (StringTuple2Map.add (M.name, instance) inst map)
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
      | `Status_change (name, instance) -> begin
        let%lwt () = Lwt_io.printf "Update from %s/%s\n%!" name instance in
        let { json; _ } = StringTuple2Map.find (name, instance) running_instances in
        let%lwt () = Lwt_io.printf "    STATUS = %s\n%!" (json ()) in
        loop ()
      end
    end
    | Pipe_closed
    | Nothing_available -> Lwt.return ()
  in

  loop ()