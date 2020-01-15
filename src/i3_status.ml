open Utils


let header_block = "{ \"version\": 1, \"stop_signal\": 20, \"cont_signal\": 18, \"click_events\": true }"

let pipe : (Lwt_module.message_to_status, [< `r | `w ]) Lwt_pipe.t = Lwt_pipe.create ~max_size:10 ()

let color = "#c5c8c6"
let color_good = "#39B258"
let color_degraded = "#CC9246"
let color_bad = "#9B51FF"
let color_separator = "#C5C8C6"

let modules : [ `r | `w] Lwt_module.modulo list = [
  new Disk_usage.modulo "/" pipe color color_degraded color_bad true;
  new Disk_usage.modulo "/home" pipe color color_degraded color_bad true;
  new Whatsmyip.modulo "0" pipe color_good color_bad true;
  new Path_exists.modulo "PostgreSQL" pipe "/run/postgresql/.s.PGSQL.5432.lock" "" color_good true;
  new Path_exists.modulo "Docker" pipe "/run/docker.pid" "" color_good true;
  new Load_avg.modulo "0" pipe color_good color_degraded color_bad false;
  new Cpu.modulo "0" pipe color_good color_degraded color_bad false;
  new Memory.modulo "0" pipe color_good color_degraded color_bad true;
  new Clock.modulo "0" pipe color_degraded true;
  new Volume.modulo "0" pipe color color_degraded false;
]

let rec read_click_event () =
  let open Lwt_io in
  let%lwt l = read_line stdin in
  if l = "["
  then read_click_event ()
  else begin
    let msg =
      if BatString.starts_with l ","
      then BatString.strip ~chars:"," l
      else l in
    let res = Yojson.Safe.from_string msg |> I3bar_protocol.Click_event.of_yojson in
    match res with
    | Ok event -> Lwt.return event
    | Error _ -> begin
      Logs.err (fun m -> m "Error parsing message: %s" msg);
      read_click_event ()
    end
  end

let rec read_clicks_loop (running_instances : [ `r | `w ] Lwt_module.modulo StringTuple2Map.t) () =
  let%lwt msg = read_click_event () in
  let name = msg.name in
  let instance = msg.instance in
  let maybe_mod =
    try Some (StringTuple2Map.find (name, instance) running_instances)
    with Not_found -> None in
  match maybe_mod with
  | Some mod_ -> begin
    let pipe = mod_#pipe in
    let%lwt _ = Lwt_pipe.write pipe msg in
    read_clicks_loop running_instances ()
  end
  | None -> read_clicks_loop running_instances ()


let entry_point () =
  let%lwt running_instances = Lwt_list.fold_left_s (fun map (modulo : [ `r | `w] Lwt_module.modulo) ->
    let%lwt () = modulo#run () in
    Lwt.return (StringTuple2Map.add (modulo#name, modulo#instance) modulo map)
  ) StringTuple2Map.empty modules in

  Lwt.async (read_clicks_loop running_instances);

  let%lwt () = Lwt_io.write_line Lwt_io.stdout header_block in
  let%lwt () = Lwt_io.write_line Lwt_io.stdout "[" in

  let rec loop () =
    match%lwt Lwt_pipe.read_with_timeout pipe ~timeout:(Some 1.0) with
    | Timeout -> loop ()
    | Data_available t -> begin
      match t with
      | `Status_change (name, instance_name) -> begin
        Logs.debug (fun m -> m "(%s,%s) state update" name instance_name);
        let%lwt () = Lwt_io.printf "[" in
        let%lwt blocks = Lwt_list.map_p (fun mod_ -> mod_#json ()) modules in
        let%lwt () = Lwt_io.printf "%s" (BatString.concat "," blocks) in
        let%lwt () = Lwt_io.printf "]," in
        let%lwt () = Lwt_io.(flush stdout) in
        loop ()
      end
    end
    | Pipe_closed
    | Nothing_available -> Lwt.return ()
  in

  loop ()
