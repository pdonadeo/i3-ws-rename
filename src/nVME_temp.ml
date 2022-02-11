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

let composite_thresholds = [|
  45.0; (* MIN nvme/Composite *)
  48.6;
  52.2; (* equally spaced nvme/Composite *)
  55.8;
  59.4;
  63.0; (* MAX nvme/Composite *)
|]

let used_sensor_name = "Composite"          (* or "Sensor 2" *)
let used_thresholds = composite_thresholds  (* or sensor2_thresholds *)

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "nvme"
    val mutable state = Low 0.0
    val mutable nvme_last_data= []

    method! private loop () =
      match hwmon_file_map_nvme with
      | `Ok hwmon_file_map_nvme -> begin
        let%lwt nvme_temps = read_temperatures hwmon_file_map_nvme in

        let nvme_last_data' =
          match BatMap.String.find_opt used_sensor_name nvme_temps with
          | None -> nvme_last_data
          | Some v -> v::nvme_last_data |> first_n moving_average_length in
        nvme_last_data <- nvme_last_data';

        let nvme_last_data_str = "[" ^ (List.map string_of_float nvme_last_data |> String.concat ", ") ^ "]" in
        Logs.debug (fun m -> m "(%s,%s) %s = %s" name instance_name used_sensor_name nvme_last_data_str);

        let%lwt () =
          if List.length nvme_last_data >= moving_average_length then begin
            let sensor_avg = avg nvme_last_data in
            let sensor' = get_range used_thresholds sensor_avg in

            let%lwt _ =
              if not_same_range state sensor'
              then Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
              else Lwt.return_true in

            state <- sensor';

            Lwt.return_unit
          end else Lwt.return_unit in

        let%lwt () = Lwt_unix.sleep delay in
        self#loop ()
      end
      | `Device_not_found -> Logs.info (fun m -> m "(%s,%s) Device NVME not found, exiting loop" name instance_name) |> Lwt.return

    method! json () =
      let color =
        match state with
        | Low _ -> color_good
        | Normal _ -> color_good
        | High _ -> color_degraded
        | Hot _ -> color_bad in

      let sensor_t = get_temp state in

      let full_text =
        match state with
        | Low _ -> ""
        | Normal _ -> spf "%s %s" thermometer_half hdd
        | High _ -> spf "%s %s" temperature_high hdd
        | Hot _ -> spf "%s %s %0.2f°" temperature_hot hdd sensor_t
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
