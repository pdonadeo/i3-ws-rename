open Utils

type icon_conf_record = {
  icon : string;
  window_class : string option; [@default None]
  window_instance : string option; [@default None]
  app_id : string option; [@default None]
  name : string option; [@default None]
}
[@@deriving yojson, show]

type icons_configuration = icon_conf_record list [@@deriving yojson, show]

let spf = Printf.sprintf
let default_configuration = []

let record_lowercase r =
  {
    icon = r.icon;
    window_class = opt_map r.window_class ~f:String.lowercase_ascii;
    window_instance = opt_map r.window_instance ~f:String.lowercase_ascii;
    app_id = opt_map r.app_id ~f:String.lowercase_ascii;
    name = opt_map r.name ~f:String.lowercase_ascii;
  }

let read_icons_configuration fname =
  let%lwt orig_conf = Utils.read_json_configuration icons_configuration_of_yojson fname in
  Lwt.return (orig_conf |> List.map record_lowercase)

let hd_opt xs =
  match xs with
  | [] -> None
  | hd :: _ -> Some hd

let search_class conf class_ =
  let class_ = String.lowercase_ascii class_ in
  Logs.debug (fun m -> m "search_class %s" class_);
  List.filter (fun record -> record.window_class = Some class_) conf |> hd_opt |> opt_map ~f:(fun r -> r.icon)

let search_instance conf instance =
  let instance = String.lowercase_ascii instance in
  Logs.debug (fun m -> m "search_instance %s" instance);
  List.filter (fun record -> record.window_instance = Some instance) conf |> hd_opt |> opt_map ~f:(fun r -> r.icon)

let search_class_instance conf class_ instance =
  let class_ = String.lowercase_ascii class_ in
  let instance = String.lowercase_ascii instance in
  Logs.debug (fun m -> m "search_class_instance %s %s" class_ instance);
  List.filter (fun record -> record.window_class = Some class_) conf
  |> List.filter (fun record ->
         let window_instance =
           match record.window_instance with
           | Some w -> w
           | None -> "THIS SHOULD NEVER MATCH"
         in
         if BatString.ends_with window_instance "*"
         then Str.string_match (Str.regexp window_instance) instance 0
         else record.window_instance = Some instance)
  |> hd_opt
  |> opt_map ~f:(fun r -> r.icon)
