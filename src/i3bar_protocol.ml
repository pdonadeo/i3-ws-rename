module Block = struct
  type align =
  | Center
  | Right
  | Left

  let align_to_yojson al =
    match al with
    | Center -> `String "center"
    | Right -> `String "right"
    | Left -> `String "left"

  type min_width =
  | Int of int
  | String of string

  let min_width_to_yojson mw =
    match mw with
    | Int i -> `Int i
    | String s -> `String s

  type markup =
  | None
  | Pango

  let markup_to_yojson m =
    match m with
    | None -> `String "none"
    | Pango -> `String "pango"

  type t = {
    full_text : string;
    short_text : string;
    color : string;
    (* background : string; *)
    (* border : string; *)
    border_top : int;
    border_right : int;
    border_bottom : int;
    border_left : int;
    min_width : min_width;
    align : align;
    urgent : bool;
    name : string;
    instance : string;
    separator : bool;
    (* separator_block_width : int; *)
    markup : markup
  } [@@deriving to_yojson]

  let default = {
    full_text = "";
    short_text = "";
    color = "#FFFFFF";
    (* background = "#000000"; *)
    (* border = "#FF0000"; *)
    border_top = 1;
    border_right = 1;
    border_bottom = 1;
    border_left = 1;
    min_width = Int 0;
    align = Left;
    urgent = false;
    name = "DEF_NAME";
    instance = "DEF_INSTANCE";
    separator = true;
    (* separator_block_width = 9; *)
    markup = None
  }
end