module PulseAudioInterface = struct
  open Ctypes

  open Glib
  open Pulse


  type userdata = {
    pipe : (float * bool, [ `r | `w]) Lwt_pipe.t;
    resolver : int Lwt.u;
    mainloop : Pa.glib_mainloop structure ptr;
    mainloop_api : Pa.mainloop_api structure ptr;
    mutable unix_signal_ev_src : Pa.signal_event structure ptr;
    mutable context : Pa.context structure ptr;
  }

  let sigint = 2

  let deallocate ud =
    if not (is_null ud.context)
    then Pa.context_unref ud.context;
    Pa.signal_free ud.unix_signal_ev_src;
    Pa.glib_mainloop_free ud.mainloop

  let quit ret_code ud =
    Lwt.wakeup_later ud.resolver ret_code

  let exit_signal_callback _m _e _sig userdata =
    let ud : userdata = Ctypes.Root.get userdata in
    quit 0 ud

  let sink_info_callback _c (sink_info : Pa.Sink_info.t structure ptr) _eol userdata =
    let ud : userdata = Ctypes.Root.get userdata in
    if not (is_null sink_info) then begin
      let volume = getf (!@sink_info) Pa.Sink_info.volume in
      let vol_addr = addr volume in
      let vol_avg = Pa.cvolume_avg vol_addr |> Unsigned.UInt32.to_int |> float_of_int in
      let v = vol_avg /. (float_of_int Pa.volume_norm) *. 100. in
      let mute = getf (!@sink_info) Pa.Sink_info.mute in
      let mute = if mute = 1 then true else false in
      let mute_str = if mute then " (muted)" else "" in
      Lwt.async (fun () ->
        Logs.debug (fun m -> m "percent volume = %.0f%%%s" v mute_str);
        let%lwt _ = Lwt_pipe.write ud.pipe (v, mute) in
        Lwt.return ()
      )
    end

  let server_info_callback c (i : Pa.Server_info.t structure ptr) userdata =
    let default_sink_name = getf (!@i) Pa.Server_info.default_sink_name in
    Logs.debug (fun m -> m "default sink name = %s" default_sink_name);
    let op = Pa.context_get_sink_info_by_name c default_sink_name sink_info_callback userdata in
    Pa.operation_unref op

  let subscribe_callback _c type_ _idx ud_c =
    let ud : userdata = Ctypes.Root.get ud_c in
    let facility = (Pa.Subscription_event_type_t.to_int type_)
                    land (Pa.Subscription_event_type_t.to_int `PA_SUBSCRIPTION_EVENT_FACILITY_MASK)
                    |> Pa.Subscription_event_type_t.of_int in
    match facility with
    | ` PA_SUBSCRIPTION_EVENT_SINK -> begin
      let op = Pa.context_get_server_info ud.context server_info_callback ud_c in
      Pa.operation_unref op;
    end
    | _ -> failwith "subscribe_callback: Got event we aren't expecting."

  let rec context_state_callback context ud_c =
    let ud : userdata = Ctypes.Root.get ud_c in

    let state = Pa.context_get_state context in
    match state with
    | `PA_CONTEXT_UNCONNECTED
    | `PA_CONTEXT_CONNECTING
    | `PA_CONTEXT_AUTHORIZING
    | `PA_CONTEXT_SETTING_NAME -> ()
    | `PA_CONTEXT_READY -> begin
      Logs.debug (fun m -> m "PulseAudio connection established.");
      let op = Pa.context_get_server_info context server_info_callback ud_c in
      Pa.operation_unref op;
      Pa.context_set_subscribe_callback context subscribe_callback ud_c;
      let op = Pa.context_subscribe context `PA_SUBSCRIPTION_MASK_SINK null null in
      Pa.operation_unref op
    end
    | `PA_CONTEXT_FAILED -> begin
      connect_to_pulse ud_c |> ignore
    end
    | `PA_CONTEXT_TERMINATED -> begin
      quit 0 ud
    end

  and connect_to_pulse ud_c =
    let ud : userdata = Ctypes.Root.get ud_c in
    if not (is_null ud.context)
    then begin
      Pa.context_disconnect ud.context;
      Pa.context_unref ud.context;
      ud.context <- from_voidp Pa.context null
    end;

    ud.context <- Pa.context_new ud.mainloop_api "PulseAudio Test";
    Pa.context_set_state_callback ud.context context_state_callback ud_c;
    let _status = Pa.context_connect ud.context null `PA_CONTEXT_NOFAIL null in
    0

  let main (pipe : (float * bool, [ `r | `w]) Lwt_pipe.t) =
    let mainloop = Pa.glib_mainloop_new (g_main_context_default ()) in
    let promise, resolver = Lwt.wait () in
    let ud = {
      pipe;
      resolver;
      mainloop;
      mainloop_api = Pa.glib_mainloop_get_api mainloop;
      unix_signal_ev_src = from_voidp Pa.signal_event null;
      context = from_voidp Pa.context null;
    } in
    let ud_c = Ctypes.Root.create ud in
    Pa.signal_init ud.mainloop_api |> ignore;
    ud.unix_signal_ev_src <- Pa.signal_new sigint exit_signal_callback ud_c;

    connect_to_pulse ud_c |> ignore;

    let%lwt ret = promise in

    Logs.debug (fun m -> m "Esco da main");
    Lwt.return (ret, ud, ud_c)
end

let spf = Printf.sprintf

class ['message, 'a] modulo instance_name status_pipe color color_muted sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "volume"
    val pipe_from_pulse : (float * bool, 'a) Lwt_pipe.t = Lwt_pipe.create ~max_size:10 ()
    val mutable state = None

    method private main () =
      let%lwt _, ud, ud_c = PulseAudioInterface.main pipe_from_pulse in
      PulseAudioInterface.deallocate ud;
      Ctypes.Root.release ud_c;
      Lwt.return ()

    method! private loop () =
      match%lwt Lwt_pipe.read_with_timeout pipe_from_pulse ~timeout:(Some 1.0) with
      | Timeout -> self#loop ()
      | Data_available t -> begin
        match t with
        | (volume, mute) -> begin
          match state with
          | Some (old_vol, old_mute) -> begin
            if volume <> old_vol || mute <> old_mute then begin
              state <- Some (volume, mute);
              let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
              self#loop ()
            end else self#loop ()
          end
          | None -> begin
            state <- Some (volume, mute);
            let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
            self#loop ()
          end
        end
      end
      | Pipe_closed
      | Nothing_available -> Lwt.return ()

    method! run () : unit Lwt.t =
      Lwt.async (self#loop);
      Lwt.async (self#main);
      Lwt.wakeup (snd ready) ();
      (fst ready)

    method! json () =
      let note = "♪" in
      let full_text, short_text, color =
        match state with
        | None -> begin
          note^"?", "", color_muted
        end
        | Some (vol_float, true) -> begin
          let vol_int = Float.round vol_float |> Float.to_int in
          spf "%s: %d%% (muted)" note vol_int, "(mute)", color_muted
        end
        | Some (vol_float, false) -> begin
          let vol_int = Float.round vol_float |> Float.to_int in
          spf "%s: %d%%" note vol_int, spf "%s %d" note vol_int, color
        end in
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
