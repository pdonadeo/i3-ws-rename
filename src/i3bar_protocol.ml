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
    background : string;
    border : string;
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
    separator_block_width : int;
    markup : markup
  } (* [@@deriving to_yojson] *)

  let default = {
    full_text = "";
    short_text = "";
    color = "#FFFFFF";
    background = "#000000";
    border = "#FF0000";
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
    separator_block_width = 9;
    markup = None
  }

  let to_yojson t : Yojson.Safe.t =
    let l = [] in
    let l = ("full_text", `String t.full_text)::l in
    let l = ("short_text", `String t.short_text)::l in
    let l = if t.color <> default.color
      then ("color", `String t.color)::l else l in
    let l = if t.background <> default.background
      then ("background", `String t.background)::l else l in
    let l = if t.border <> default.border
      then ("border", `String t.border)::l else l in
    let l = if t.border_top <> default.border_top
      then ("border_top", `Int t.border_top)::l else l in
    let l = if t.border_right <> default.border_right
      then ("border_right", `Int t.border_right)::l else l in
    let l = if t.border_bottom <> default.border_bottom
      then ("border_bottom", `Int t.border_bottom)::l else l in
    let l = if t.border_left <> default.border_left
      then ("border_left", `Int t.border_left)::l else l in
    let l = if t.min_width <> default.min_width
      then ("min_width", min_width_to_yojson t.min_width)::l else l in
    let l = if t.align <> default.align
      then ("align", align_to_yojson t.align)::l else l in
    let l = if t.urgent <> default.urgent
      then ("urgent", `Bool t.urgent)::l else l in
    let l = ("name", `String t.name)::l in
    let l = ("instance", `String t.instance)::l in
    let l = if t.separator <> default.separator
      then ("separator", `Bool t.separator)::l else l in
    let l = if t.separator_block_width <> default.separator_block_width
      then ("separator_block_width", `Int t.separator_block_width)::l else l in
    let l = if t.markup <> default.markup
      then ("markup", markup_to_yojson t.markup)::l else l in
    `Assoc (List.rev l)
end

module Click_event = struct
  type t = {
    name: string;
    instance: string;
    button: int;
    modifiers: string list;
    x: int;
    y: int;
    relative_x: int;
    relative_y: int;
    width: int;
    height: int;
  } [@@deriving of_yojson, show]
end