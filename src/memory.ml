let delay = 1.0

let spf = Printf.sprintf

let get_meminfo () =
  let open Lwt_io in
  with_file ~flags:[O_RDONLY] ~mode:Input "/proc/meminfo" (fun ic ->
    let%lwt lines = read_lines ic |> Lwt_stream.to_list in

    let stats = ListLabels.fold_left ~init:BatMap.String.empty ~f:(fun stats line ->
      let tokens =
        BatString.split_on_char ' ' line
        |> ListLabels.filter ~f:(fun t -> if String.equal t "" then false else true)
        |> Array.of_list
      in
      let name = tokens.(0) |> BatString.strip ~chars:":" in
      let val_kb = tokens.(1) |> int_of_string in
      BatMap.String.add name val_kb stats
    ) lines in
    Lwt.return stats
  )

let compute_mem_used meminfo =
  let mem_total = BatMap.String.find "MemTotal" meminfo in
  let mem_free = BatMap.String.find "MemFree" meminfo in
  let buffers = BatMap.String.find "Buffers" meminfo in
  let cached = BatMap.String.find "Cached" meminfo in
  let s_reclaimable = BatMap.String.find "SReclaimable" meminfo in
  let shmem = BatMap.String.find "Shmem" meminfo in

  let used_mem_KiB = (mem_total - mem_free -
    (buffers + cached + (s_reclaimable - shmem))) |> Float.of_int in
  let mem_total_KiB = Float.of_int mem_total in
  let used_mem_MiB = used_mem_KiB /. 1024. in
  let used_mem_GiB = used_mem_MiB /. 1024. in
  let used_mem_perc = used_mem_KiB /. mem_total_KiB *. 100. in
  if used_mem_GiB <= 1.0
  then used_mem_perc, used_mem_KiB, Float.round used_mem_MiB |> Float.to_int |> Int.to_string, "MiB"
  else used_mem_perc, used_mem_KiB, spf "%0.2f" used_mem_GiB, "GiB"

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "memory"
    val mutable state = None

    method! private loop () =
      let%lwt mem_stats = get_meminfo () in
      let used_mem_perc, used_mem_KiB, used_str, unity = compute_mem_used mem_stats in

      let%lwt result =
        match state with
        | None -> begin
          state <- Some (used_mem_perc, used_mem_KiB, used_str, unity);
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end
        | Some (_, _, used_str', _) -> begin
          if used_str' <> used_str then begin
            state <- Some (used_mem_perc, used_mem_KiB, used_str, unity);
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
      let icon = "" in
      let color, full_text, short_text =
        match state with
        | None -> color_good, "", ""
        | Some (p, _, s, u) when 0. <= p && p <= 50. ->
            color_good, spf "%s %s %s" icon s u, ""
        | Some (p, _, s, u) when 50. < p && p <= 75. ->
            color_degraded, spf "%s %s %s" icon s u, spf "%s %s %s" icon s u
        | Some (_, _, s, u) ->
            color_bad, spf "%s %s %s" icon s u, spf "%s %s %s" icon s u in
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