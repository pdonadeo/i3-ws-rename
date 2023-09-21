open Constants

let delay = 5.0

let compute_mem_used meminfo =
  let mem_total = BatMap.String.find "MemTotal" meminfo in
  let mem_free = BatMap.String.find "MemFree" meminfo in
  let buffers = BatMap.String.find "Buffers" meminfo in
  let cached = BatMap.String.find "Cached" meminfo in
  let s_reclaimable = BatMap.String.find "SReclaimable" meminfo in
  let used_diff = mem_free + cached + s_reclaimable + buffers in
  (* Used memory copied from htop:
     https://github.com/htop-dev/htop/blob/e79788c250c23e0c7c9f3e29fdca578ce9e9bca0/linux/LinuxMachine.c#L197-L217

     This is the original comment from htop source code:
      Compute memory partition like procps(free)
      https://gitlab.com/procps-ng/procps/-/blob/master/proc/sysinfo.c

     Adjustments:
      - Shmem in part of Cached (see https://lore.kernel.org/patchwork/patch/648763/),
        do not show twice by subtracting from Cached and do not subtract twice from used.
  *)
  let used_mem_KiB = (if mem_total >= used_diff then mem_total - used_diff else mem_total - mem_free) |> Float.of_int in
  let mem_total_KiB = Float.of_int mem_total in
  let used_mem_MiB = used_mem_KiB /. 1024. in
  let used_mem_GiB = used_mem_MiB /. 1024. in
  let used_mem_perc = used_mem_KiB /. mem_total_KiB *. 100. in
  if used_mem_GiB <= 1.0
  then (used_mem_perc, used_mem_KiB, Float.round used_mem_MiB |> Float.to_int |> Int.to_string, "MiB")
  else (used_mem_perc, used_mem_KiB, spf "%0.2f" used_mem_GiB, "GiB")

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [`r | `w]
    inherit ['a] Lwt_module.base_modulo instance_name status_pipe
    val! name = "memory"
    val mutable state = None

    method! private loop () =
      let%lwt mem_stats = Utils.get_meminfo () in
      let used_mem_perc, used_mem_KiB, used_str, unity = compute_mem_used mem_stats in

      let%lwt result =
        match state with
        | None -> begin
          state <- Some (used_mem_perc, used_mem_KiB, used_str, unity);
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end
        | Some (_, _, used_str', _) -> begin
          if used_str' <> used_str
          then begin
            state <- Some (used_mem_perc, used_mem_KiB, used_str, unity);
            Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
          end
          else begin
            Lwt.return true
          end
        end
      in

      let%lwt () = Lwt_unix.sleep delay in
      if result = true then self#loop () else Lwt.return ()

    method! json () =
      let icon = "" in
      let color, full_text, short_text =
        match state with
        | None -> (color_good, "", "")
        | Some (p, _, s, u) when 0. <= p && p <= 50. -> (color_good, spf "%s %s %s" icon s u, icon)
        | Some (p, _, s, u) when 50. < p && p <= 75. ->
          (color_degraded, spf "%s %s %s" icon s u, spf "%s %s %s" icon s u)
        | Some (_, _, s, u) -> (color_bad, spf "%s %s %s" icon s u, spf "%s %s %s" icon s u)
      in
      let bl =
        {
          I3bar_protocol.Block.default with
          full_text;
          short_text;
          color;
          name;
          instance = instance_name;
          separator = sep;
        }
      in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
  end
