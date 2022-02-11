open Constants
open Utils

let delay = 1.0
let moving_average_length = 30

let spf = Printf.sprintf

let hwmon_file_map_k10temp = Lwt_main.run (get_hwmon_file_map "k10temp")

let tctl_thresholds = [|
  30.250; (* MIN k10temp/Tctl *)
  40.825;
  51.400; (* equally spaced k10temp/Tctl *)
  61.975;
  72.550;
  83.125; (* MAX k10temp/Tctl *)
|]

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "k10_temp"
    val mutable state = Low 0.0
    val mutable k10temp_last_data = []

    method! private loop () =
      match hwmon_file_map_k10temp with
      | `Ok hwmon_file_map_k10temp -> begin
        let%lwt k10temp_temps = read_temperatures hwmon_file_map_k10temp in

        let k10temp_last_data' =
          match BatMap.String.find_opt "Tctl" k10temp_temps with
          | None -> k10temp_last_data
          | Some v -> v::k10temp_last_data |> first_n moving_average_length in
        k10temp_last_data <- k10temp_last_data';

        let k10temp_last_data_str = "[" ^ (List.map string_of_float k10temp_last_data |> String.concat ", ") ^ "]" in
        Logs.debug (fun m -> m "(%s,%s) Tctl = %s" name instance_name k10temp_last_data_str);

        let%lwt () =
          if List.length k10temp_last_data >= moving_average_length then begin
            let tctl_avg = avg k10temp_last_data in
            let tctl' = get_range tctl_thresholds tctl_avg in

            let%lwt _ =
              if not_same_range state tctl'
              then Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
              else Lwt.return_true in

            state <- tctl';

            Lwt.return_unit
          end else Lwt.return_unit in

        let%lwt () = Lwt_unix.sleep delay in
        self#loop ()
      end
      | `Device_not_found -> Logs.info (fun m -> m "(%s,%s) Device K10 not found, exiting loop" name instance_name) |> Lwt.return

    method! json () =
      let color =
        match state with
        | Low _ -> color_good
        | Normal _ -> color_good
        | High _ -> color_degraded
        | Hot _ -> color_bad in

      let tctl_t = get_temp state in

      let full_text =
        match state with
        | Low _ -> ""
        | Normal _ -> spf "%s %s " thermometer_half microchip
        | High _ -> spf "%s %s " temperature_high microchip
        | Hot _ -> spf "%s %s %0.2f° " temperature_hot microchip tctl_t
      in

      let short_text = full_text in
      let bl = {I3bar_protocol.Block.default with
        full_text;
        short_text;
        color;
        name;
        instance = instance_name;
        separator = sep;
        separator_block_width = 0;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
  end
