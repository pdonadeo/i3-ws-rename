let delay = 1.0

let spf = Printf.sprintf

let get_cpu_stats () =
  let open Lwt_io in
  with_file ~flags:[O_RDONLY] ~mode:Input "/proc/stat" (fun ic ->
    let%lwt lines = read_lines ic |> Lwt_stream.to_list in
    let first_line = List.hd lines in
    let open Core in
    let stats =
      String.split ~on:' ' first_line
      |> List.filter ~f:(fun t -> if String.equal t "" then false else true)
      |> List.tl_exn
      |> List.to_array
      |> Array.map ~f:Int.of_string in
    let stats =
      String.Map.add_exn String.Map.empty ~key:"user" ~data:stats.(0)
      |> String.Map.add_exn ~key:"nice"       ~data:stats.(1)
      |> String.Map.add_exn ~key:"system"     ~data:stats.(2)
      |> String.Map.add_exn ~key:"idle"       ~data:stats.(3)
      |> String.Map.add_exn ~key:"iowait"     ~data:stats.(4)
      |> String.Map.add_exn ~key:"irq"        ~data:stats.(5)
      |> String.Map.add_exn ~key:"softirq"    ~data:stats.(6)
      |> String.Map.add_exn ~key:"steal"      ~data:stats.(7)
      |> String.Map.add_exn ~key:"guest"      ~data:stats.(8)
      |> String.Map.add_exn ~key:"guest_nice" ~data:stats.(9) in
    Lwt.return stats
  )

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "cpu"
    val mutable state = None
    val mutable last_cpu_stats = Core.String.Map.empty
    val mutable cpu_stats = Core.String.Map.empty

    method private compute_cpu_used () =
      let open Core in
      let last_sum = String.Map.fold ~init:0 ~f:(fun ~key ~data sum -> ignore key; sum + data) last_cpu_stats |> Float.of_int in
      let sum = String.Map.fold ~init:0 ~f:(fun ~key ~data sum -> ignore key; sum + data) cpu_stats |> Float.of_int in
      let last_idle =
        match String.Map.find last_cpu_stats "idle" with
        | Some i -> i |> Float.of_int
        | None -> 0. in
      let idle = String.Map.find_exn cpu_stats "idle" |> Float.of_int in
      (1. -. (idle -. last_idle) /. (sum -. last_sum)) *. 100.

    method! private loop () =
      let%lwt cpu_stats' = get_cpu_stats () in
      last_cpu_stats <- cpu_stats;
      cpu_stats <- cpu_stats';

      let cpu_used = self#compute_cpu_used () in
      let cpu_used_str = spf "%0.2f" cpu_used in

      let%lwt result =
        match state with
        | None -> begin
          state <- Some (cpu_used, cpu_used_str);
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end
        | Some (cpu, _str) -> begin
          if Float.abs (cpu -. cpu_used) > 5.0 then begin
            state <- Some (cpu_used, cpu_used_str);
            Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
          end else begin
            Lwt.return true
          end
        end in

      let%lwt () = Lwt_unix.sleep delay in
      if result = true
      then self#loop ()
      else Lwt.return ()

    method! json () =
      let icon = "" in
      let color, full_text, short_text =
        match state with
        | None -> color_good, "", ""
        | Some (c, s) when 0. <= c && c <= 50. ->
            color_good, spf "%s %s%% " icon s, ""
        | Some (c, s) when 50. < c && c <= 75. ->
            color_degraded, spf "%s %s%% " icon s, spf "%s %s%% " icon s
        | Some (_, s) ->
            color_bad, spf "%s %s%% " icon s, spf "%s %s%% " icon s in
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