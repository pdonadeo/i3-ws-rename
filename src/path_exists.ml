let delay = 1.0

class ['a] modulo instance_name status_pipe path icon color sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "path_exists"
    val mutable state = None

    method! private loop () =
      let%lwt exists = Lwt_unix.file_exists path in

      let%lwt result =
        match state with
        | None -> begin
          state <- Some exists;
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end
        | Some exists' -> begin
          if exists <> exists' then begin
            state <- Some exists;
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
      let full_text =
        match state with
        | None
        | Some false -> ""
        | Some true -> icon in
      let bl = {I3bar_protocol.Block.default with
        full_text;
        color;
        name;
        instance = instance_name;
        separator = sep;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
