let delay = 5.0

let spf = Printf.sprintf

type status =
  | Unknown_state
  | Internet_is_up of string
  | Internet_is_down of Curl.curlCode

let get_public_ip ip_url =
  let ip = ref "" in
  let write s = ip := s; String.length s in

  let open Curl in
  global_init CURLINIT_GLOBALSSL;
  let curl = init () in
  setopt curl (CURLOPT_URL ip_url);
  set_writefunction curl write;

  let%lwt res = Curl_lwt.perform curl in

  cleanup curl;
  global_cleanup ();

  match res with
  | CURLE_OK -> Lwt.return (Ok (String.trim !ip))
  | e -> Lwt.return (Error e)

class ['a] modulo instance_name status_pipe color_good color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "whatsmyip"
    val mutable internet_state = Unknown_state
    val mutable show_state = false

    method! private loop () =
      let%lwt res = get_public_ip "https://ifconfig.co/ip" in

      let state_changed = match internet_state, res with
      | Unknown_state, Ok ip -> internet_state <- Internet_is_up ip; true
      | Internet_is_up ip, Ok ip' when ip <> ip' -> internet_state <- Internet_is_up ip'; true
      | Internet_is_down _, Ok ip -> internet_state <- Internet_is_up ip; true
      | Unknown_state, Error e -> internet_state <- Internet_is_down e; true
      | Internet_is_up _, Error e -> internet_state <- Internet_is_down e; true
      | Internet_is_down e, Error e' when e <> e' -> internet_state <- Internet_is_down e'; true
      | _ -> false in

      let%lwt _unused = if state_changed
      then Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
      else Lwt.return true in

      let%lwt () = Lwt_unix.sleep delay in
      self#loop ()

    method! private read_loop () =
      let%lwt maybe_msg = Lwt_pipe.read pipe in
      let%lwt _unused = match maybe_msg with
      | Some _ -> begin
        show_state <- (not show_state);
        Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
      end
      | None -> Lwt.return true in
      self#read_loop ()

    method! json () =
      let icon, color = match internet_state with
      | Internet_is_up _ -> "", color_good
      | Unknown_state
      | Internet_is_down _ -> "", color_bad in

      let full_text =
        match internet_state, show_state with
        | Unknown_state, true -> icon
        | Internet_is_up ip, true ->  spf " %s" ip
        | Internet_is_down e, true -> (Curl.strerror e)
        | Unknown_state, false -> icon
        | Internet_is_up _, false ->  icon
        | Internet_is_down _, false ->  icon in

      let bl = {I3bar_protocol.Block.default with
        full_text;
        short_text = full_text;
        color;
        name;
        instance = instance_name;
        separator = sep;
      } in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return
end
