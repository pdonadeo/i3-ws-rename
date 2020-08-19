open Constants
open Utils


let delay = 10.0
let request_timeout = (delay /. 2.0) -. 1.0

let ip_regexp = Str.regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"

type status =
  | Unknown_state
  | Internet_is_up of string
  | Internet_is_down of string

let get_public_ip ip_url =
  let%lwt result = get_or_timeout ~timeout:request_timeout ip_url in
  match result with
  | `Response (resp, body) -> begin
    match resp.status with
    | `OK -> begin
      let%lwt body_text = Cohttp_lwt.Body.to_string body in
      let body_text = String.trim body_text in
      if Str.string_match ip_regexp body_text 0
      then Lwt.return (`Ip_address body_text)
      else Lwt.return `Not_an_ip_address
    end
    | _ -> Lwt.return `Http_error
  end
  | `Timeout -> Lwt.return `Timeout
  | `Exception exn -> Lwt.return (`Exception exn)

class ['a] modulo instance_name status_pipe color_good color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "whatsmyip"
    val mutable internet_state = Unknown_state
    val mutable show_state = false

    method! private loop () =
      let%lwt res = get_public_ip "https://dns.4sigma.it/ip/" in

      let state_changed = match internet_state, res with
      | Unknown_state       , `Ip_address ip -> internet_state <- Internet_is_up ip; true
      | Internet_is_up ip   , `Ip_address ip' when ip <> ip' -> begin
        Logs.info (fun m -> m "(%s,%s) IP address chenged: %s => %s" name instance_name ip ip');
        internet_state <- Internet_is_up ip';
        true
      end
      | Internet_is_down _  , `Ip_address ip -> begin
        Logs.info (fun m -> m "(%s,%s) Internet is back. IP address is %s" name instance_name ip);
        internet_state <- Internet_is_up ip;
        true
      end

      | Internet_is_down _  , `Not_an_ip_address -> internet_state <- Internet_is_up "N/A"; true

      | Internet_is_down _  , `Http_error -> internet_state <- Internet_is_up "N/A"; true

      | Internet_is_up _    , `Timeout -> begin
        Logs.info (fun m -> m "(%s,%s) Internet is down :-/" name instance_name);
        internet_state <- Internet_is_down "Timeout";
        true
      end

      | _                   , `Exception exn -> begin
        Logs.err (fun m -> m "(%s,%s) Exception raised while checking Internet connection" name instance_name);
        Logs.err (fun m -> m "(%s,%s)     %s" name instance_name (Printexc.to_string exn));
        Logs.info (fun m -> m "(%s,%s) Internet is down :-/" name instance_name);
        internet_state <- Internet_is_down "Exception";
        true
      end

      | Internet_is_down _  , `Timeout
      | Internet_is_up _    , `Http_error
      | Internet_is_up _    , `Ip_address _
      | Internet_is_up _    , `Not_an_ip_address
      | Unknown_state       , `Http_error
      | Unknown_state       , `Not_an_ip_address
      | Unknown_state       , `Timeout -> false in

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
        | Internet_is_down e, true -> e
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
