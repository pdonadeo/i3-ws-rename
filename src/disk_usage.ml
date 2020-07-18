open Constants


let delay = 60.0

let df () =
  let open BatString in
  try%lwt
    let%lwt res = Lwt_process.pread ("/usr/bin/df", [|"df"; "-k"|]) in
    let lines = split_on_string ~by:"\n" res |> List.tl |> List.filter ((<>) "") in
    let init = BatMap.String.empty in
    let map = List.fold_left (fun m line ->
      let usage_mount = BatList.split_at 4 (split_on_char ' ' line |> List.filter ((<>) "")) |> snd in
      let usage = List.nth usage_mount 0 |> strip ~chars:"%" |> int_of_string in
      let mount = List.nth usage_mount 1 in
      BatMap.String.add mount usage m
    ) init lines in
    Lwt.return map
  with _ -> Lwt.return BatMap.String.empty

class ['a] modulo instance_name status_pipe color_good color_degraded color_bad sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [ `r | `w]

    inherit ['a] Lwt_module.base_modulo instance_name status_pipe

    val! name = "disk"
    val mutable state = None

    method! private loop () =
      let%lwt res = df () in
      let usage =
        try BatMap.String.find instance_name res
        with Not_found -> 0 in

      let%lwt result =
        if state <> (Some usage) then begin
          state <- Some usage;
          Lwt_pipe.write status_pipe (`Status_change (name, instance_name))
        end else Lwt.return true in

      let%lwt () = Lwt_unix.sleep delay in
      if result = true
      then self#loop ()
      else Lwt.return ()

    method! json () =
      let icon = match instance_name with
        | "/" -> ""
        | "/home" -> ""
        | _ -> "?" in
      let full_text, short_text = match state with
      | None -> "N/A", ""
      | Some s -> spf "%s %d%%" icon s, spf "%s %d" icon s in
      let color =
        match state with
        | None -> color_good
        | Some u when 0 <= u && u <= 75 -> color_good
        | Some u when 75 < u && u <= 95 -> color_degraded
        | Some _ -> color_bad in
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
