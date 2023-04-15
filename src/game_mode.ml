open Constants
open Utils


let spf = Printf.sprintf

type status =
  | Game_mode_on
  | Game_mode_off
  | Game_mode_changing
  | Bluetooth_power_cycle
  [@@deriving yojson, show]

let wallpaper_script = (Unix.getenv "HOME")/".config/i3/set_wallpaper.sh"

let turn_on_game_mode () =
  let%lwt _ = Lwt_process.exec ("/usr/bin/sudo", [|"sudo"; "servizi_steam.sh"; "stop" |]) in
  let%lwt () = match%lwt Utils.find_picom_pid () with
    | Some pid -> begin
      Logs.info (fun m -> m "Game mode: killing picom");
      Unix.kill pid 15 |> Lwt.return
    end
    | None -> Lwt.return_unit in
  let%lwt () = match%lwt Utils.find_xwinwrap_pid () with
    | Some pid -> begin
      Logs.info (fun m -> m "Game mode: killing xwinwrap");
      Unix.kill pid 15 |> Lwt.return
    end
    | None -> Lwt.return_unit in
  let%lwt _ = Lwt_process.exec ("/usr/bin/xset", [|"xset"; "s"; "off"; "-dpms";|]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "stop"; "syncthing.service" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "stop"; "dunst.service" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "stop"; "go-cervino.service" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "restart"; "pipewire.service"; "pipewire.socket"; "pipewire-pulse.service"; "pipewire-pulse.socket"; "pipewire-session-manager.service"|]) in
  let%lwt _ = Lwt_process.exec (wallpaper_script, [|"set_wallpaper.sh"; "reset"|]) in
  Lwt.return_unit

let turn_off_game_mode () =
  let%lwt () = match%lwt Utils.find_picom_pid () with
    | Some _pid -> Lwt.return_unit
    | None -> begin
      let%lwt _ = Lwt_process.exec ("/usr/bin/picom", [|"picom"; "-b"; "-f"; "-D"; "3"; "-C"; "-G"|]) in
      Lwt.return_unit
    end in
  let%lwt _ = Lwt_process.exec ("/usr/bin/xset", [|"xset"; "s"; "300"; "300";|]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/xset", [|"xset"; "dpms"; "600"; "600"; "600" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "start"; "syncthing.service" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "start"; "dunst.service" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/systemctl", [| "systemctl"; "--user"; "start"; "go-cervino.service" |]) in
  let%lwt _ = Lwt_process.exec ("/usr/bin/sudo", [|"sudo"; "servizi_steam.sh"; "start" |]) in
  let%lwt _ = Lwt_process.exec (wallpaper_script, [|"set_wallpaper.sh"|]) in
  Lwt.return_unit

let bluetooth_power_cycle () =
  let%lwt _ = Lwt_process.exec ("/usr/bin/sudo", [|"sudo"; "bluetooth_cycle.sh"; "start" |]) in
  Lwt.return_unit

class ['a] modulo instance status_pipe color color_degraded separator : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w ]

    inherit ['a] Lwt_module.base_modulo instance status_pipe

    val! name = "game_mode"
    val mutable state = Game_mode_off

    method! private read_loop () =
      let%lwt maybe_msg = Lwt_pipe.read pipe in
      let%lwt _ = match maybe_msg with
      | Some { button = 1; _ } -> begin
        (* Left click *)
        match state with
        | Game_mode_off -> begin
          Logs.info (fun m -> m "Turning on GAME MODE");
          state <- Game_mode_changing;
          let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance)) in
          let%lwt () = turn_on_game_mode () in
          state <- Game_mode_on;
          Lwt_pipe.write status_pipe (`Status_change (name, instance))
        end
        | Game_mode_on -> begin
          Logs.info (fun m -> m "Turning off GAME MODE");
          state <- Game_mode_changing;
          let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance)) in
          let%lwt () = turn_off_game_mode () in
          state <- Game_mode_off;
          Lwt_pipe.write status_pipe (`Status_change (name, instance))
        end
        | Game_mode_changing
        | Bluetooth_power_cycle -> Lwt.return_true
      end
      | Some { button = 3; _ } -> begin
        (* Right click *)
        Logs.info (fun m -> m "Bluetooth power cycle START");
        let old_state = state in
        state <- Bluetooth_power_cycle;
        let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance)) in
        let%lwt () = bluetooth_power_cycle () in
        state <- old_state;
        Lwt_pipe.write status_pipe (`Status_change (name, instance))
      end
      | _ -> Lwt.return true in
      self#read_loop ()

    method! dump_state () =
      status_to_yojson state |> Yojson.Safe.to_string |> Lwt.return

    method! json () =
      let color =
        match state with
        | Game_mode_off -> ""
        | Game_mode_on -> color
        | Game_mode_changing
        | Bluetooth_power_cycle -> color_degraded in

      let bl = {I3bar_protocol.Block.default with
        full_text = joystick;
        short_text = joystick;
        color;
        name;
        instance;
        separator;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
