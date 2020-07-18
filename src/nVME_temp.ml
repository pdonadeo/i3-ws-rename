open Constants
open Utils

let delay = 1.0
let moving_average_length = 10

let spf = Printf.sprintf

let hwmon_file_map_nvme = Lwt_main.run (get_hwmon_file_map "nvme")

let sensor2_thresholds = [|
  42.85; (* MIN nvme/Sensor 2 *)
  53.85; (* 25% nvme/Sensor 2 *)
  54.85; (* 50% nvme/Sensor 2 *)
  55.85; (* 75% nvme/Sensor 2 *)
  57.85; (* 90% nvme/Sensor 2 *)
  76.85; (* MAX nvme/Sensor 2 *)
|]

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "nvme"
    val mutable state = Low 0.0
    val mutable nvme_last_data= []

    method! private loop () =
      let%lwt nvme_temps = read_temperatures hwmon_file_map_nvme in

      let nvme_last_data' =
        match BatMap.String.find_opt "Sensor 2" nvme_temps with
        | None -> nvme_last_data
        | Some v -> v::nvme_last_data |> first_n moving_average_length in
      nvme_last_data <- nvme_last_data';

      let nvme_last_data_str = "[" ^ (List.map string_of_float nvme_last_data |> String.concat ", ") ^ "]" in
      Logs.debug (fun m -> m "(%s,%s) Sensor 2 = %s" name instance_name nvme_last_data_str);

      let%lwt () =
        if List.length nvme_last_data >= moving_average_length then begin
          let sensor2_avg = avg nvme_last_data in
          let sensor2' = get_range sensor2_thresholds sensor2_avg in

          let%lwt _ =
            if not_same_range state sensor2'
            then Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
            else Lwt.return_true in

          state <- sensor2';

          Logs.info (fun m -> m "(%s,%s) Sensor 2 AVG = %f" name instance_name sensor2_avg);
          Lwt.return_unit
        end else Lwt.return_unit in

      let%lwt () = Lwt_unix.sleep delay in
      self#loop ()

    method! json () =
      let color =
        match state with
        | Low _ -> color_good
        | Normal _ -> color_good
        | High _ -> color_degraded
        | Hot _ -> color_bad in

      let sensor2_t = get_temp state in

      let full_text =
        match state with
        | Low _ -> ""
        | Normal _ -> spf "%s %s" thermometer_half hdd
        | High _ -> spf "%s %s" temperature_high hdd
        | Hot _ -> spf "%s %s %0.2f°" temperature_hot hdd sensor2_t
      in

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
