let spf = Printf.sprintf
let iof = int_of_float
let foi = float_of_int

let utf8_of_string s =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec loop ?(acc=[]) () =
    match Uutf.decode dec with
    | `Await -> assert false
    | `Uchar u_c -> loop ~acc:(u_c::acc) ()
    | `End -> (Ok (List.rev acc |> Array.of_list))
    | `Malformed s -> Error (`Malformed s)
  in
  loop ()

let string_of_utf8 u =
  let b = Buffer.create 128 in
  let enc = Uutf.encoder `UTF_8 (`Buffer b) in
  ArrayLabels.iter u ~f:(fun uc -> Uutf.encode enc (`Uchar uc) |> ignore);
  Uutf.encode enc `End |> ignore;
  Buffer.contents b

let string_carousel ?(max_l=40) ~start str =
  assert (start < max_l);
  let ustr_or_err = utf8_of_string str in
  match ustr_or_err with
  | Ok ustr -> begin
    let l = Array.length ustr in
    if l > max_l then (
      let ustr = Array.append ustr [| (Uchar.of_int 32); (Uchar.of_int 8212); (Uchar.of_int 32)|] in
      let copy_n = 1 + ((foi (l + 3)) /. (foi max_l) |> ceil |> iof) in
      let rep = Array.make copy_n ustr |> Array.to_list |> Array.concat in
      let part = Array.sub rep start max_l in
      Ok (string_of_utf8 part)
    ) else Ok (ustr |> string_of_utf8)
  end
  | Error e -> Error e

exception Service_unknown of string
module M1 =
  OBus_error.Register(
    struct
      exception E = Service_unknown
      let name = "org.freedesktop.DBus.Error.ServiceUnknown"
    end)

exception No_reply of string
module M2 =
  OBus_error.Register(
    struct
      exception E = No_reply
      let name = "org.freedesktop.DBus.Error.NoReply"
    end)

type spotify_status =
  | Not_running
  | Playing
  | Paused

let string_of_spotify_status = function
  | Not_running -> "Not_running"
  | Playing -> "Playing"
  | Paused ->  "Paused"

let spotify_status_of_string = function
  | "Playing" -> Playing
  | "Paused" -> Paused
  | _ -> Not_running

class ['a] modulo instance_name status_pipe color_play color_pause sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "spotify"
    val banner_max_length = 40
    val mutable current_song_props = BatMap.String.empty
    val mutable current_status = Not_running
    val mutable banner_index = 0
    val mutable banner = ""

    method private map_of_metadata m =
      let open OBus_value in

      ListLabels.fold_left m ~init:BatMap.String.empty ~f:(fun props (k, v) ->
        if (V.type_of_single v) = T.basic_string
        then BatMap.String.add k (C.cast_single (C.basic_string) v) props
        else if (V.type_of_single v) = T.array T.basic_string
        then begin
          let v = C.cast_single (C.array C.basic_string) v |> String.concat " " in
          BatMap.String.add k v props
        end else props
      )

    method private update_song_props song_props =
      if not (BatMap.String.equal String.equal song_props current_song_props)
      then begin
        current_song_props <- song_props;
        let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
        Lwt.return ()
      end else Lwt.return ()

    method private process_metadata (arg_val : OBus_value.V.single) =
      let open OBus_value in

      let arg_val = C.cast_single (C.dict C.string C.variant) arg_val in
      let song_props = self#map_of_metadata arg_val in
      self#update_song_props song_props

    method private process_status (v : OBus_value.V.single) =
      let open OBus_value in

      let status = C.cast_single C.basic_string v |> spotify_status_of_string in
      if status <> current_status
      then begin
        current_status <- status;
        let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
        Lwt.return ()
      end else Lwt.return ()

    method private status_listener player_proxy () =
      let open Lwt_react in

      let%lwt (event : (string * (string * OBus_value.V.single) list * string list) event)=
        OBus_signal.connect
          (Spotify_client.Org_freedesktop_DBus_Properties.properties_changed player_proxy) in

      E.keep (
        E.map_s (fun (_, arguments, _) ->
          Lwt_list.iter_s (fun (sig_arg, v) ->
            if sig_arg = "Metadata"
            then self#process_metadata v
            else self#process_status v
          ) arguments
        ) event
      ) |> Lwt.return

    method private update_alive player_proxy =
      try%lwt
        let%lwt () = Spotify_client.Org_freedesktop_DBus_Peer.ping player_proxy in
        let%lwt status = OBus_property.get
            (Spotify_client.Org_mpris_MediaPlayer2_Player.playback_status player_proxy) in
        let status = spotify_status_of_string status in
        if current_status <> status then begin
          let%lwt metadata = OBus_property.get
            (Spotify_client.Org_mpris_MediaPlayer2_Player.metadata player_proxy) in
          let song_props = self#map_of_metadata metadata in
          let%lwt () = self#update_song_props song_props in
          current_status <- status;
          let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
          Lwt.return ()
        end else Lwt.return ()
      with
      | No_reply _msg
      | Service_unknown _msg -> begin
        if current_status <> Not_running then begin
          current_status <- Not_running;
          current_song_props <- BatMap.String.empty;
          let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
          Lwt.return ()
        end else Lwt.return ()
      end

    method private alive_loop player_proxy () =
      let%lwt () = self#update_alive player_proxy in
      let%lwt () = Lwt_unix.sleep 1.0 in
      self#alive_loop player_proxy ()

    method private read_loop' player_proxy () =
      let%lwt maybe_msg = Lwt_pipe.read pipe in
      let%lwt () = match maybe_msg with
      | Some { button = b; _ } -> begin
        match b with
        | 1 -> Spotify_client.Org_mpris_MediaPlayer2_Player.play_pause player_proxy
        | 3 -> Spotify_client.Org_mpris_MediaPlayer2_Player.next player_proxy
        | _ -> Lwt.return ()
      end
      | None -> Lwt.return () in
      self#read_loop' player_proxy ()

    method private carousel () =
      let%lwt () = Lwt_unix.sleep 1.0 in
      if current_status <> Not_running then begin
        let text =
          let artist = BatMap.String.find_default "" "xesam:artist" current_song_props in
          let title = BatMap.String.find_default "" "xesam:title" current_song_props in
          spf "%s — %s" artist title
        in
        if String.length text > banner_max_length then begin
          let new_banner =
            string_carousel ~max_l:banner_max_length ~start:banner_index text |> Stdlib.Result.get_ok in
          banner_index <- (banner_index + 1) mod banner_max_length;
          banner <- new_banner;
          let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
          self#carousel ()
        end else begin
          if banner <> text then begin
            banner <- text;
            let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
            self#carousel ()
          end else self#carousel ()
        end
      end else self#carousel ()

    method! run () : unit Lwt.t =
      let%lwt bus = OBus_bus.session () in
      let spotify_peer =
        OBus_peer.make
          ~connection:bus
          ~name:"org.mpris.MediaPlayer2.spotify" in
      let player_proxy = OBus_proxy.make
        ~peer:spotify_peer
        ~path:["org"; "mpris"; "MediaPlayer2"] in

      Lwt.async (self#status_listener player_proxy);
      Lwt.async (self#alive_loop player_proxy);
      Lwt.async (self#read_loop' player_proxy);
      Lwt.async (self#carousel);

      Lwt.wakeup (snd ready) ();
      (fst ready)

    method! json () =
      let icon = "" in
      let summary = spf "%s %s" icon banner in

      let full_text, color =
        match current_status with
        | Not_running -> "", color_play
        | Playing -> summary, color_play
        | Paused -> summary, color_pause in

      let bl = {I3bar_protocol.Block.default with
        full_text;
        short_text = "";
        color;
        name;
        instance = instance_name;
        separator = sep;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
  end
