open Utils


type conf_record = {
  fa_icon: string;
  window_class: string;
  window_instance: string option [@default None];
} [@@deriving yojson]

type configuration = conf_record list [@@deriving yojson]

let spf = Printf.sprintf

let  default_configuration = []

let record_lowercase r = {
  fa_icon = String.lowercase_ascii r.fa_icon;
  window_class = String.lowercase_ascii r.window_class;
  window_instance = opt_map r.window_instance ~f:String.lowercase_ascii
}

let read_configuration fname =
  match fname with
  | Some fname ->
    Lwt_io.with_file fname ~mode:Lwt_io.input (fun f ->
      let%lwt conf_str = Lwt_io.read f in
      let conf_j = Yojson.Safe.from_string conf_str in
      let conf_or_error = configuration_of_yojson conf_j in
      match conf_or_error with
      | Ok conf -> begin
          Logs.debug (fun m -> m "Configuration successfully loaded from %s" fname);
          Lwt.return (List.map record_lowercase conf)
      end
      | Error e -> failwith (spf "Parse error: %s" e) (* TODO *)
    )
  | None -> Lwt.return default_configuration

let hd_opt xs =
  match xs with
  | [] -> None
  | hd::_ -> Some hd

let search_class conf class_ =
  let class_ = String.lowercase_ascii class_ in
  Logs.debug (fun m -> m "search_class %s" class_);
  List.filter (fun record -> record.window_class = class_) conf
  |> hd_opt
  |> opt_map ~f:(fun r ->
      let icon = r.fa_icon in
      Fa_icons.get_icon_string icon
    )

let search_instance conf instance =
  let instance = String.lowercase_ascii instance in
  Logs.debug (fun m -> m "search_instance %s" instance);
  List.filter (fun record -> record.window_instance = Some instance) conf
  |> hd_opt
  |> opt_map ~f:(fun r ->
      let icon = r.fa_icon in
      Fa_icons.get_icon_string icon
    )

let search_class_instance conf class_ instance =
  let class_ = String.lowercase_ascii class_ in
  let instance = String.lowercase_ascii instance in
  Logs.debug (fun m -> m "search_class_instance %s %s" class_ instance);
  List.filter (fun record -> record.window_class = class_) conf
  |> List.filter (fun record ->
      let window_instance =
        match record.window_instance with
        | Some w -> w | None -> "THIS SHOULD NEVER MATCH" in
      if BatString.ends_with window_instance "*"
      then Str.string_match (Str.regexp window_instance) instance 0
      else record.window_instance = Some instance
  )
  |> hd_opt
  |> opt_map ~f:(fun r ->
      let icon = r.fa_icon in
      Fa_icons.get_icon_string icon
    )
