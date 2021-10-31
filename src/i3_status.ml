open Utils


(* In the past I used stop_signal = 10 (SIGUSR1) and cont_signal = 12 (SIGUSR2), like this:
 *     let header_block = "{ \"version\": 1, \"stop_signal\": 10, \"cont_signal\": 12, \"click_events\": true }"
 * I had problems because i3bar send signals to every child process, not only to the main process, randomly
 * killing picom, xset and other processes.
 * I decided to disable  this feature.
 *)
let header_block = "{ \"version\": 1, \"stop_signal\": 0, \"cont_signal\": 0, \"click_events\": true }"

let pipe : (Lwt_module.message_to_status, [< `r | `w ]) Lwt_pipe.t = Lwt_pipe.create ~max_size:10 ()

let color = "#c5c8c6"
let color_good = "#39B258"
let color_degraded = "#CC9246"
let color_bad = "#9B51FF"
let color_separator = "#C5C8C6"

let game_mode_module = new Game_mode.modulo "0" pipe color_good color_degraded true

let modules : [ `r | `w] Lwt_module.modulo list = [
  new Spotify.modulo "0" pipe color_good color_degraded true;
  new Dualsense_battery.modulo "0" pipe color_good color_degraded color_bad true;
  new Disk_usage.modulo "/" pipe color color_degraded color_bad true;
  new Disk_usage.modulo "/home" pipe color color_degraded color_bad true;
  new Whatsmyip.modulo "0" pipe color_good color_bad true;
  game_mode_module;
  new Otp.modulo "0" pipe color_good color_degraded color_bad true;
  new Vpn_status.modulo "0" pipe color_good true;
  new Path_exists.modulo "PostgreSQL" pipe "/run/postgresql/.s.PGSQL.5432.lock" "" color_good true;
  new Path_exists.modulo "Docker" pipe "/run/docker.pid" "" color_good true;
  new Load_avg.modulo "0" pipe color_good color_degraded color_bad false;
  new Cpu.modulo "0" pipe color_good color_degraded color_bad false;
  new Memory.modulo "0" pipe color_good color_degraded color_bad true;
  new K10_temp.modulo "0" pipe color_good color_degraded color_bad false;
  new SSD_temp.modulo "0" pipe color_good color_degraded color_bad false;
  new NVME_temp.modulo "0" pipe color_good color_degraded color_bad true;
  new Case_fan_leds.modulo "0" pipe color_degraded color_good color_bad true;
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

let _ = Lwt_unix.on_signal Sys.sigusr1 (fun _ ->
  Logs.debug (fun m -> m "SIGUSR1 received, sending Stop_output message to the status loop…");
  Lwt.async (fun () ->
    let%lwt _ = Lwt_pipe.write pipe `Stop_output in
    Lwt.return_unit
  )
)
let _ = Lwt_unix.on_signal Sys.sigusr2 (fun _ ->
  Logs.debug (fun m -> m "SIGUSR2 received, sending Continue_output message to the status loop…");
  Lwt.async (fun () ->
    let%lwt _ = Lwt_pipe.write pipe `Continue_output in
    Lwt.return_unit
  )
)

let entry_point state_fname shutdown () =
  let%lwt state = StringTuple2Map_Json.map_from_fname state_fname in
  let%lwt running_instances = Lwt_list.fold_left_s (fun map (modulo : [ `r | `w] Lwt_module.modulo) ->
    let%lwt () =
      match state with
      | Ok state -> begin
        match StringTuple2Map.find_opt (modulo#name, modulo#instance) state with
        | Some mod_state -> modulo#load_state mod_state
        | None -> Lwt.return_unit
      end
      | Error _ -> Lwt.return_unit in
    let%lwt () = modulo#run () in
    Lwt.return (StringTuple2Map.add (modulo#name, modulo#instance) modulo map)
  ) StringTuple2Map.empty modules in

  Lwt.async (read_clicks_loop running_instances);

  let%lwt () = Lwt_io.write_line Lwt_io.stdout header_block in
  let%lwt () = Lwt_io.write_line Lwt_io.stdout "[" in

  let completed, signal_completed = Lwt.wait () in

  let rec loop ?(enable_output=true) () =
    (* Since I decided to disable signals from i3bar, enable_output is always true *)
    if not (Lwt.is_sleeping shutdown) then begin
      let running_instances = StringTuple2Map.fold (fun _ m acc -> m::acc) running_instances [] in
      let%lwt state = Lwt_list.fold_left_s (fun serialized modulo ->
        let%lwt state = modulo#dump_state () in
        let serialized = {
          StringTuple2Map_Json.key = (modulo#name, modulo#instance);
          StringTuple2Map_Json.data = state;
        }::serialized in
        let%lwt () = modulo#stop () in
        Lwt.return serialized
      ) [] running_instances in
      let%lwt () = StringTuple2Map_Json.map_to_file state_fname state in
      Lwt.wakeup signal_completed ();
      Lwt.return_unit
    end else begin
      match%lwt Lwt_pipe.read_with_timeout pipe ~timeout:(Some 1.0) with
      | Timeout -> loop ~enable_output ()
      | Data_available t -> begin
        match t with
        | `Status_change (name, instance_name) -> begin
          if enable_output then begin
            Logs.debug (fun m -> m "(%s,%s) state update" name instance_name);
            let%lwt () = Lwt_io.printf "[" in
            let%lwt blocks = Lwt_list.map_p (fun mod_ -> mod_#json ()) modules in
            let%lwt () = Lwt_io.printf "%s" (BatString.concat "," blocks) in
            let%lwt () = Lwt_io.printf "]," in
            let%lwt () = Lwt_io.(flush stdout) in
            loop ~enable_output ()
          end else loop ~enable_output ()
        end
        | `Stop_output -> begin
          Logs.info (fun m -> m "I'm stopping to send JSON output to i3bar");
          loop ~enable_output:false ()
        end
        | `Continue_output -> begin
          Logs.info (fun m -> m "I'm going to send JSON output to i3bar again");
          loop ~enable_output:true ()
        end
      end
      | Pipe_closed
      | Nothing_available -> Lwt.return_unit
    end in

  Lwt.async loop;
  completed
