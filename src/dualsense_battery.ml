open Constants

let delay = 2.0

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

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
object (self)
  constraint 'a = [ `r | `w]

  inherit ['a] Lwt_module.base_modulo instance_name status_pipe

  val! name = "dualsense_battery"
  val mutable state = `Dualsense_not_present

  method! private loop () =
    let%lwt state' = battery_status () in

    let%lwt result =
      if state <> state' then begin
        state <- state';
        Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
      end else Lwt.return true in
    Logs.debug (fun m -> m "(%s,%s)" name instance_name);

    let%lwt () = Lwt_unix.sleep delay in
    if result = true
    then self#loop ()
    else Lwt.return ()

  method! json () =
    let color, level = match state with
    | `Battery_status (`Full, _) -> color_good, -1
    | `Battery_status (`Unknown, _) -> color_bad, -1
    | `Battery_status (_, level) when 0  <= level && level < 25 -> color_bad, level
    | `Battery_status (_, level) when 25 <= level && level < 50 -> color_degraded, -1
    | `Battery_status (_, level) when 50 <= level && level < 75 -> color_degraded, -1
    | `Battery_status (_, level) when 75 <= level && level < 99 -> color_good, -1
    | `Battery_status (_, level) when level = 100 -> color_good, -1
    | `Battery_status (_, _) -> color_bad, -1  (* This should never happen... *)
    | `Dualsense_not_present -> "", -1
    in

    let charging = match state with
    | `Battery_status (`Charging, _) -> spf " %s" battery_bolt
    | _ -> "" in

    let level =
      if level = -1
      then ""
      else spf " %d%%" level in
    let full_text = spf "%s%s%s" gamepad_alt level charging in
    let short_text = full_text in

    let bl = {I3bar_protocol.Block.default with
      full_text;
      short_text;
      color;
      name;
      instance = instance_name;
      separator = sep;
    } in
    Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
