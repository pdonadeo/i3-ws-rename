module StringMap = Map.Make(String)

type conf_record = {
  fa_icon: string;
  window_class: string;
  window_instance: string option [@default None];
} [@@deriving yojson]

type configuration = conf_record list [@@deriving yojson]

let spf = Printf.sprintf

let read_configuration fname =
  Lwt_io.with_file fname ~mode:Lwt_io.input (fun f ->
    let%lwt conf_str = Lwt_io.read f in
    let conf_j = Yojson.Safe.from_string conf_str in
    let conf_or_error = configuration_of_yojson conf_j in
    match conf_or_error with
    | Ok conf -> Lwt.return conf
    | Error e -> failwith (spf "Parse error: %s" e)
  )

let hd_opt xs =
  match xs with
  | [] -> None
  | hd::_ -> Some hd

let opt_map o ~f =
  match o with
  | None -> None
  | Some v -> Some (f v)

let search_class conf class_ =
  List.filter (fun record -> record.window_class = class_) conf
  |> hd_opt
  |> opt_map ~f:(fun r ->
      let icon = r.fa_icon in
      Fa_icons.get_icon_string icon
    )
