let delay = 2.0

let spf = Printf.sprintf

let get_cpu_stats () =
  let open Lwt_io in
  with_file ~flags:[O_RDONLY] ~mode:Input "/proc/stat" (fun ic ->
    let%lwt lines = read_lines ic |> Lwt_stream.to_list in
    let first_line = List.hd lines in

    let stats =
      BatString.split_on_char ' ' first_line
      |> List.filter (fun t -> if String.equal t "" then false else true)
      |> List.tl
      |> Array.of_list
      |> Array.map int_of_string in
    let stats =
      BatMap.String.add    "user"       stats.(0) BatMap.String.empty
      |> BatMap.String.add "nice"       stats.(1)
      |> BatMap.String.add "system"     stats.(2)
      |> BatMap.String.add "idle"       stats.(3)
      |> BatMap.String.add "iowait"     stats.(4)
      |> BatMap.String.add "irq"        stats.(5)
      |> BatMap.String.add "softirq"    stats.(6)
      |> BatMap.String.add "steal"      stats.(7)
      |> BatMap.String.add "guest"      stats.(8)
      |> BatMap.String.add "guest_nice" stats.(9) in
    Lwt.return stats
  )

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "cpu"
    val mutable state = None
    val mutable last_cpu_stats = BatMap.String.empty
    val mutable cpu_stats = BatMap.String.empty

    method private compute_cpu_used () =
      (* (string -> 'a -> 'b -> 'b) -> 'a BatMap.String.t -> 'b -> 'b *)
      let last_sum = BatMap.String.fold (fun key data sum -> ignore key; sum + data) last_cpu_stats 0 |> Float.of_int in
      let sum = BatMap.String.fold (fun key data sum -> ignore key; sum + data) cpu_stats 0 |> Float.of_int in
      let last_idle =
        try BatMap.String.find "idle" last_cpu_stats |> float_of_int
        with Not_found -> 0. in
      let idle = BatMap.String.find "idle" cpu_stats |> float_of_int in
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
            color_good, spf "%s %s%% " icon s, icon^" "
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