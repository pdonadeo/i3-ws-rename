open Constants
open Utils

let delay = 1.0
let moving_average_length = 10

let spf = Printf.sprintf

let ssd_thresholds = [|
  27.0; (* MIN CT2000MX500SSD1/temp *)
  34.8;
  42.6; (* equally spaced CT2000MX500SSD1/temp *)
  50.4;
  58.2;
  66.0; (* MAX CT2000MX500SSD1/temp *)
|]

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "ssd"
    val mutable state = Low 0.0
    val mutable ssd_last_data= []

    method! private loop () =
      let%lwt () =
        match%lwt read_hdd_temp () with
        | Ok t -> begin
          ssd_last_data <- t::ssd_last_data |> first_n moving_average_length;

          let ssd_last_data_str = "[" ^ (List.map string_of_float ssd_last_data |> String.concat ", ") ^ "]" in
          Logs.debug (fun m -> m "(%s,%s) SSD = %s" name instance_name ssd_last_data_str);

          let%lwt () =
            if List.length ssd_last_data >= moving_average_length then begin
              let ssd_avg = avg ssd_last_data in
              let ssd' = get_range ssd_thresholds ssd_avg in

              let%lwt _ =
                if not_same_range state ssd'
                then Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
                else Lwt.return_true in

              state <- ssd';

              Lwt.return_unit
            end else Lwt.return_unit in
          Lwt.return_unit
        end
        | Error e -> begin
          Logs.info (fun m -> m "(%s,%s) Cannot read SSD temperature" name instance_name);
          Logs.info (fun m -> m "(%s,%s)     ERROR: %s" name instance_name e);
          Lwt.return_unit
        end in
      let%lwt () = Lwt_unix.sleep delay in
      self#loop ()

    method! json () =
      let color =
        match state with
        | Low _ -> color_good
        | Normal _ -> color_good
        | High _ -> color_degraded
        | Hot _ -> color_bad in

      let ssd_t = get_temp state in

      let full_text =
        match state with
        | Low _ -> ""
        | Normal _ -> spf "%s %s " thermometer_half compact_disc
        | High _ -> spf "%s %s " temperature_high compact_disc
        | Hot _ -> spf "%s %s %0.2f° " temperature_hot compact_disc ssd_t
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
