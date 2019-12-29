let delay = 0.25

let spf = Printf.sprintf

let get_cores () =
  let open Lwt_io in
  with_file ~flags:[O_RDONLY] ~mode:Input "/proc/cpuinfo" (fun ic ->
    let%lwt lines = read_lines ic |> Lwt_stream.to_list in
    let count = List.fold_left (fun c l ->
      if Core.String.is_prefix l ~prefix:"processor"
      then c + 1
      else c
    ) 0 lines in
    Lwt.return (Float.of_int count)
  )

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "load_avg"
    val mutable state = None

    method! private loop () =
      let load1, _, _ = Unix_extended.getloadavg () in

      let%lwt result =
        if state <> (Some load1) then begin
          state <- Some load1;
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end else Lwt.return true in

      let%lwt () = Lwt_unix.sleep delay in
      if result = true
      then self#loop ()
      else Lwt.return ()

    method! json () =
      let%lwt cores = get_cores () in
      let load_perc =
        match state with
        | Some l -> l /. cores
        | None -> 0.0 in
      let icon = "" in
      let color, full_text, short_text =
        match state, load_perc with
        | None, _ -> color_good, "", ""
        | Some l, lp when 0. <= lp && lp <= 50. -> color_good, spf "%s %0.2f" icon l, ""
        | Some l, lp when 50. < lp && lp <= 75. -> color_degraded, spf "%s %0.2f" icon l, spf "%s %0.2f" icon l
        | Some l, _ -> color_bad, spf "%s %0.2f" icon l, spf "%s %0.2f" icon l in
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
