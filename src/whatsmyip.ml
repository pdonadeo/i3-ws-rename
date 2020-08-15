open Constants


let delay = 10.0
let curl_timeout = 2.0

let ip_regexp = Str.regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$"

type status =
  | Unknown_state
  | Internet_is_up of string
  | Internet_is_down of string

let get_public_ip ip_url =
  let open Lwt_process in

  let pr = open_process_in ~timeout:curl_timeout ~stderr:`Dev_null ("curl", [| "/usr/bin/curl"; ip_url |]) in
  let ic = pr#stdout in
  let%lwt output =
    try%lwt begin
      let%lwt str = Lwt_io.read ic in
      let str = String.trim str in
      if Str.string_match ip_regexp str 0
      then Lwt.return (`Correct_ip str)
      else Lwt.return (`Error_service_returned_trash)
    end
    with Lwt_io.Channel_closed _ -> Lwt.return `Error_channel_closed in

  let%lwt status = pr#close in
  match output with
  | `Correct_ip output -> Lwt.return (`Correct_ip output)
  | `Error_service_returned_trash -> Lwt.return `Error_service_returned_trash
  | `Error_channel_closed -> begin
    match status with
    | Unix.WEXITED st -> Lwt.return (`Error_process_exited_status st)
    | Unix.WSIGNALED s
    | Unix.WSTOPPED s -> Lwt.return (`Error_process_signaled s)
  end

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
      | _, `Error_process_exited_status st -> internet_state <- Internet_is_down (spf "curl exited with %d" st); true
      | _, `Error_process_signaled _sig -> internet_state <- Internet_is_down (spf "curl signaled with %d" _sig); true
      | Unknown_state, `Error_service_returned_trash -> internet_state <- Unknown_state; false
      | Internet_is_down _, `Error_service_returned_trash -> internet_state <- Unknown_state; true

      | Internet_is_up ip, `Correct_ip ip' when ip <> ip' -> internet_state <- Internet_is_up ip'; true

      | Internet_is_up _, `Correct_ip _
      | Internet_is_up _, `Error_service_returned_trash -> false

      | Unknown_state, `Correct_ip ip
      | Internet_is_down _, `Correct_ip ip -> internet_state <- Internet_is_up ip; true in

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
