open Constants

let delay = 60.0

let find_dir_command = (
  "/usr/bin/find",
  [|"find"; "/sys/class/power_supply/"; "-type"; "l"; "-name"; "*ps-controller-battery*"|]
)

let battery_status () =
  let open BatString in
  try%lwt
    let%lwt res = Lwt_process.pread ~stderr:`Dev_null find_dir_command in
    let lines = String.trim res |> split_on_string ~by:"\n" |> List.map String.trim |> List.filter ((<>) "") in
    match List.length lines with
    | 1 -> begin
      let ( / ) d f = d^"/"^f in
      let dir = List.hd lines in
      let status_file = dir/"status" in
      let capacity_file = dir/"capacity" in
      let%lwt res = Lwt_process.pread ~stderr:`Dev_null ("/usr/bin/cat", [|"cat"; status_file|]) in
      Logs.debug (fun m -> m "%s" (String.trim res));
      let status =
        match String.trim res with
        | "Discharging" -> `Discharging
        | "Charging" -> `Charging
        | "Full" -> `Full
        | _ -> `Unknown in
      let%lwt res = Lwt_process.pread ~stderr:`Dev_null ("/usr/bin/cat", [|"cat"; capacity_file|]) in
      let capacity = String.trim res |> int_of_string in
      Lwt.return (`Battery_status ( status, capacity))
    end
    | _ -> begin
      Lwt.return `Dualsense_not_present
    end

  with _ -> Lwt.return `Dualsense_not_present

class ['a] modulo instance status_pipe color_good color_degraded color_bad separator : ['a] Lwt_module.modulo =
object (self)
  constraint 'a = [ `r | `w]

  inherit ['a] Lwt_module.base_modulo instance status_pipe

  val! name = "dualsense_battery"
  val mutable state = `Dualsense_not_present
  val mutable show_level = false

  method! private loop () =
    let%lwt state' = battery_status () in

    let%lwt result =
      if state <> state' then begin
        state <- state';
        Lwt_pipe.write status_pipe (`Status_change (name, instance))
      end else Lwt.return true in
    Logs.debug (fun m -> m "(%s,%s)" name instance);

    let%lwt () = Lwt_unix.sleep delay in
    if result = true
    then self#loop ()
    else Lwt.return ()

  method! private read_loop () =
    let%lwt maybe_msg = Lwt_pipe.read pipe in
    let%lwt _unused = match maybe_msg with
    | Some _ -> begin
      show_level <- (not show_level);
      Lwt_pipe.write status_pipe (`Status_change (name, instance))
    end
    | None -> Lwt.return true in
    self#read_loop ()

  method! json () =
    let color, level = match state with
    | `Battery_status (`Full, _) -> color_good, 100
    | `Battery_status (`Unknown, _) -> color_bad, -1
    | `Battery_status (_, level) when 0  <= level && level < 25 -> color_bad, level
    | `Battery_status (_, level) when 25 <= level && level < 50 -> color_degraded, level
    | `Battery_status (_, level) when 50 <= level && level < 75 -> color_degraded, level
    | `Battery_status (_, level) when 75 <= level && level < 99 -> color_good, level
    | `Battery_status (_, level) when level = 100 -> color_good, level
    | `Battery_status (_, _) -> color_bad, -1  (* This should never happen... *)
    | `Dualsense_not_present -> "", -1
    in

    let present = match state with
    | `Dualsense_not_present -> false
    | _ -> true in

    let charging = match state with
    | `Battery_status (`Charging, _) -> spf " %s" battery_bolt
    | _ -> "" in

    let level =
      if show_level = true
      then spf " %d%%" level
      else if color = color_bad
        then spf " %d%%" level
        else "" in
    let full_text =
      if present
      then spf "%s%s%s" gamepad_alt level charging
      else "" in
    let short_text = full_text in

    let bl = {I3bar_protocol.Block.default with
      full_text;
      short_text;
      color;
      name;
      instance;
      separator;
    } in
    Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
