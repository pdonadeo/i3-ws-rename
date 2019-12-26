let delay = 0.33

let now () =
  let open Unix in
  let { tm_min; tm_hour; tm_sec; _ } = gettimeofday () |> localtime in
  (* ignore tm_sec; *)
  Lwt.return (Printf.sprintf "%02d:%02d:%02d" tm_hour tm_min tm_sec)

class ['message, 'a] modulo instance_name status_pipe : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "clock"
    val mutable state = None

    method! private loop () =
      let%lwt n = now () in

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
    match state with
    | None -> "{}"
    | Some s -> Printf.sprintf "{ %s }" s
end