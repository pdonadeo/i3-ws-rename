let spf = Printf.sprintf

let base32_to_hex encoded_text =
  let padding = String.make ((8 - (String.length encoded_text) mod 8) mod 8) '=' in
  let encoded_text = encoded_text ^ padding in
  Base32.Std.decode_string encoded_text
  |> Option.get
  |> String.to_seq
  |> Seq.to_list
  |> ListLabels.map ~f:Char.code
  |> ListLabels.map ~f:(Printf.sprintf "%02x")
  |> StringLabels.concat ~sep:""

let hmac_sha1 key str =
  let open Cryptokit in
  let hmac = MAC.hmac_sha1 (transform_string (Hexa.decode ()) key) in
  hmac#add_string (transform_string (Hexa.decode ()) str);
  hmac#result |> (transform_string (Hexa.encode ()))

let compute_otp secret =
  let secret_hex = base32_to_hex secret in
  let epoch_div_30 = (Unix.time () |> int_of_float) / 30 |> spf "%016x" in
  let epoch_mod_30 = (Unix.time () |> int_of_float) mod 30 in
  let hmac = hmac_sha1 secret_hex epoch_div_30 in
  let offset = int_of_string ("0x"^(String.make 1 hmac.[39])) in
  let otp' =
    int_of_string ("0x"^(String.sub hmac (offset*2) 8)) land 0x7fffffff |>
    string_of_int in
  String.sub otp' ((String.length otp') - 6) 6, (30 - epoch_mod_30)


type status = {
  current_secret : int;
  show : bool;
} [@@deriving yojson, show]

let default = {
  current_secret = 0;
  show = false;
}

class ['a] modulo instance status_pipe color color_degraded color_bad separator : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w ]

    inherit ['a] Lwt_module.base_modulo instance status_pipe

    val! name = "otp"
    val mutable state = default

    method private update_state () =
      if state.show then begin
        let%lwt () = Lwt_unix.sleep 0.25 in
        let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance)) in
        self#update_state ();
      end else Lwt.return_unit

    method! private read_loop () =
      let%lwt maybe_msg = Lwt_pipe.read pipe in
      let%lwt () =
        match maybe_msg with
        | Some { button = b; _ } -> begin
            match b with
            | 1 -> begin
              (* Left click *)
              let how_many_secrets = List.length !Conf.otp_global_configuration in
              if state.show then begin
                state <- { state with current_secret = (state.current_secret + 1) mod how_many_secrets };
                Lwt.return ()
              end else begin
                state <- { show = true; current_secret = 0 };
                Utils.detach_promise self#update_state "Otp.modulo#update_state";
                Lwt.return ()
              end
            end
            | 3 -> begin
              (* Right click *)
              let otp, _ = compute_otp (List.nth !Conf.otp_global_configuration state.current_secret).secret in
              let%lwt () = Lwt_process.pwrite ("/usr/bin/xclip", [|"xclip"; "-selection"; "clipboard"|]) otp in
              state <- { state with show = false };
              let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance)) in
              Lwt.return ()
            end
            | _ -> Lwt.return ()
          end
        | None -> Lwt.return () in
      self#read_loop ()

    method! json () =
      let conf = List.nth !Conf.otp_global_configuration state.current_secret in
      let otp, remaining = compute_otp conf.secret in
      let otp_1 = String.sub otp 0 3 in
      let otp_2 = String.sub otp 3 3 in

      let text =
        let conf_icon = conf.icon in
        let conf_name = conf.name in
        if state.show
          then begin
            let icon = match conf_icon with | Some i -> spf "%s " i | None -> "" in
            spf "%s%s: %s %s (-%ds)" icon conf_name otp_1 otp_2 remaining
          end
          else "" in

      let color = match state.show, remaining with
      | false, _ -> color
      | true, r when r > 15 -> color
      | true, r when r > 5 -> color_degraded
      | true, _ -> color_bad in

      let bl = {I3bar_protocol.Block.default with
        full_text = text;
        short_text = text;
        color;
        name;
        instance;
        separator;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
