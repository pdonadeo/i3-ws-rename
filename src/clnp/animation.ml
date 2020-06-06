open Corsair_Lighting_Node_Pro

type 'a animation = state:'a -> float -> (frame * float) option * 'a

let run_animation controller stopped ~init ~(a : 'a animation) =
  let sample_period = 1.0 /. 50.0 in
  let loop_start_ts = Unix.gettimeofday () in

  let current_frame = ref None in
  let current_frame_d = ref 0.0 in
  let current_frame_start = ref loop_start_ts in

  let rec loop ~state () =
    if Lwt.is_sleeping stopped then begin
      let now = Unix.gettimeofday () in

      match !current_frame with
      | None -> begin
        let new_frame, state = a ~state (now -. loop_start_ts) in
        match new_frame with
        | None -> Lwt.return ()
        | Some (new_frame, duration) -> begin
          current_frame := Some new_frame;
          current_frame_d := duration;
          current_frame_start := now;
          let%lwt _ = send_frame controller First new_frame in

          let%lwt () = Lwt_unix.sleep sample_period in
          loop ~state ()
        end
      end
      | Some _ -> begin
        if !current_frame_start +. !current_frame_d < now then begin
          let new_frame, state = a ~state (now -. loop_start_ts) in
          match new_frame with
          | None -> Lwt.return ()
          | Some (new_frame, duration) -> begin
            current_frame := Some new_frame;
            current_frame_d := duration;
            current_frame_start := now;
            let%lwt _ = send_frame controller First new_frame in

            let%lwt () = Lwt_unix.sleep sample_period in
            loop ~state ()
          end
        end else begin
          let%lwt () = Lwt_unix.sleep sample_period in
          loop ~state ()
        end
      end
    end else Lwt.return ()
  in

  loop ~state:init ()