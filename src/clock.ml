let delay = 0.33

let now () =
  let open Unix in
  let { tm_min; tm_hour; tm_mday; tm_mon; tm_year; _ } =
    gettimeofday () |> localtime in
  (tm_year + 1900, tm_mon + 1, tm_mday, tm_hour, tm_min)

let format_full state =
  match state with
  | Some (y, mo, d, h, m) -> Printf.sprintf " %02d/%02d/%04d  %02d:%02d" d mo y h m
  | None -> ""

let format_short state =
  match state with
  | Some (_y, _mo, _d, h, m) -> Printf.sprintf "  %02d:%02d" h m
  | None -> ""

class ['message, 'a] modulo instance_name status_pipe color sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "clock"
    val mutable state = None

    method! private loop () =
      let n = now () in

      let%lwt result =
        match state with
        | None -> begin
          state <- Some n;
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end
        | Some old_n -> begin
          if n <> old_n then begin
            state <- Some n;
            Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
          end
          else Lwt.return true
        end in

      if result then begin
        let%lwt () = Lwt_unix.sleep delay in
        self#loop ()
      end
      else Lwt.return ()

    method! json () =
      let bl = {I3bar_protocol.Block.default with
        full_text = format_full state;
        short_text = format_short state;
        color;
        name;
        instance = instance_name;
        separator = sep;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
