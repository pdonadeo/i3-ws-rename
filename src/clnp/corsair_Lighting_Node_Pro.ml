open USB

module List = ListLabels

let fst3 (x, _, _) = x
let snd3 (_, x, _) = x
let trd3 (_, _, x) = x

type effect_mode =
  | Rainbow_wave  (* 0x00 *)
  | Color_shift   (* 0x01 *)
  | Color_pulse   (* 0x02 *)
  | Color_wave    (* 0x03 *)
  | Static        (* 0x04 *)
  | Temperature   (* 0x05 *)
  | Visor         (* 0x06 *)
  | Marquee       (* 0x07 *)
  | Blink         (* 0x08 *)
  | Sequential    (* 0x09 *)
  | Rainbow       (* 0x0A *)

let int_of_effect_mode = function
  | Rainbow_wave  -> 0x00
  | Color_shift   -> 0x01
  | Color_pulse   -> 0x02
  | Color_wave    -> 0x03
  | Static        -> 0x04
  | Temperature   -> 0x05
  | Visor         -> 0x06
  | Marquee       -> 0x07
  | Blink         -> 0x08
  | Sequential    -> 0x09
  | Rainbow       -> 0x0A

type effect_speed =
  | Fast    (* 0x00 *)
  | Medium  (* 0x01 *)
  | Slow    (* 0x02 *)

let int_of_effect_speed = function
  | Fast    -> 0x00
  | Medium  -> 0x01
  | Slow    -> 0x02

type effect_direction =
  | Clockwise         (* 0x00 *)
  | CounterClockwise  (* 0x01 *)

let int_of_effect_direction = function
  | Clockwise -> 0x00
  | CounterClockwise -> 0x01

type color_mode =
  | Alternating   (* 0x00 *)
  | Random_color  (* 0x01 *)

let int_of_color_mode = function
  | Alternating   -> 0x00
  | Random_color  -> 0x01

type color = int * int * int

let to_color c : color =
  let c = Color.to_rgba c in
  c.r, c.g, c.b

let black = 0, 0, 0
let white = 0xff, 0xff, 0xff
let red = 0xff, 0, 0
let blue = 0, 0, 0xff

type effect_configuration = {
  mode : effect_mode;
  speed : effect_speed;
  dir : effect_direction;
  color_mode : color_mode;
  color1 : color;
  color2 : color;
  color3 : color;
}

type color_channel =
  | R
  | G
  | B

let int_of_color_channel = function
  | R -> 0
  | G -> 1
  | B -> 2

type port_control =
  | Hardware
  | Software

let int_of_port_control = function
  | Hardware -> 1
  | Software -> 2

type channel =
  | First
  | Second

let int_of_channel = function
  | First -> 0
  | Second -> 1

type command =
  | WriteLedColorValues of color_channel * int list (* color channel, values *)
  | WriteLedTrigger
  | WriteLedClear of channel                        (* channel *)
  | WriteLedGroupSet of effect_configuration        (* config *)
  | WriteLedGroupsClear of channel                  (* channel *)
  | WriteLedMode of channel * port_control          (* channel, port control *)

let packet_of_int_list l =
  let buf = Bytes.make 64 '\x00' in
  List.iteri ~f:(fun i el -> Bytes.set buf i (Char.chr el)) l;
  buf

let packet_of_command c =
  match c with
  | WriteLedColorValues (color_channel, values) -> begin
      let count = Stdlib__listLabels.length values in
      packet_of_int_list ([ 0x32; 0; 0; count; int_of_color_channel color_channel] @ values)
    end
  | WriteLedTrigger -> packet_of_int_list [0x33; 0xff]
  | WriteLedClear chan -> packet_of_int_list [0x34; int_of_channel chan]
  | WriteLedGroupSet conf -> packet_of_int_list [
      0x35; 0; 0; 0x20;
      int_of_effect_mode conf.mode;
      int_of_effect_speed conf.speed;
      int_of_effect_direction conf.dir;
      int_of_color_mode conf.color_mode;
      0x00;
      fst3 conf.color1; snd3 conf.color1; trd3 conf.color1;
      fst3 conf.color2; snd3 conf.color2; trd3 conf.color2;
      fst3 conf.color3; snd3 conf.color3; trd3 conf.color3;
    ]
  | WriteLedGroupsClear chan -> packet_of_int_list [0x37; int_of_channel chan]
  | WriteLedMode (chan, p_ctrl) -> packet_of_int_list [0x38; int_of_channel chan; int_of_port_control p_ctrl]

type controller = {
  usb_device : USB.handle;
  interface : int;
  mutable kernel_driver_was_attached : bool;
}

let send_command controller command =
  let buf = packet_of_command command in
  try%lwt
    bulk_send ~handle:controller.usb_device ~endpoint:1 ~timeout:1.0 (Bytes.to_string buf) 0 64
  with USB.Transfer (err, fun_name) -> begin
      let%lwt () = Lwt_io.eprintf "USB transfer error: '%s' failed: %s" fun_name (USB.transfer_error_message err) in
      let%lwt () = USB.reset_device controller.usb_device in
      bulk_send ~handle:controller.usb_device ~endpoint:1 ~timeout:1.0 (Bytes.to_string buf) 0 64
    end

let send_commands controller commands =
  Lwt_list.iter_s (fun c ->
      let%lwt _ = send_command controller c in
      Lwt.return ()
    ) commands

type fan_leds = {
  i_top : color;
  i_right : color;
  i_bottom : color;
  i_left : color;

  o_12 : color;
  o_01 : color;
  o_02 : color;
  o_03 : color;
  o_04 : color;
  o_05 : color;
  o_06 : color;
  o_07 : color;
  o_08 : color;
  o_09 : color;
  o_10 : color;
  o_11 : color;
}

type frame = fan_leds * fan_leds

let monochrome c = {
  i_top = c;
  i_right = c;
  i_bottom = c;
  i_left = c;

  o_12 = c;
  o_01 = c;
  o_02 = c;
  o_03 = c;
  o_04 = c;
  o_05 = c;
  o_06 = c;
  o_07 = c;
  o_08 = c;
  o_09 = c;
  o_10 = c;
  o_11 = c;
}

let fan_off = monochrome black

type led_coord =
  | Inner of int
  | Outer of int

let change_led leds led color =
  match led with
  | Inner l -> begin
      match l with
      | 1 -> { leds with i_top = color }
      | 2 -> { leds with i_right = color }
      | 3 -> { leds with i_bottom = color }
      | 4 -> { leds with i_left = color }
      | x -> failwith (Printf.sprintf "Inner led %d does not exits" x)
    end
  | Outer l -> begin
      match l with
      | 1  -> { leds with o_12 = color }
      | 2  -> { leds with o_01 = color }
      | 3  -> { leds with o_02 = color }
      | 4  -> { leds with o_03 = color }
      | 5  -> { leds with o_04 = color }
      | 6  -> { leds with o_05 = color }
      | 7  -> { leds with o_06 = color }
      | 8  -> { leds with o_07 = color }
      | 9  -> { leds with o_08 = color }
      | 10 -> { leds with o_09 = color }
      | 11 -> { leds with o_10 = color }
      | 12 -> { leds with o_11 = color }
      | x -> failwith (Printf.sprintf "Outer led %d does not exits" x)
    end

let component f fan =
  [
    f fan.i_top;
    f fan.i_left;
    f fan.i_bottom;
    f fan.i_right;

    f fan.o_10;
    f fan.o_09;
    f fan.o_08;
    f fan.o_07;
    f fan.o_06;
    f fan.o_05;
    f fan.o_04;
    f fan.o_03;
    f fan.o_02;
    f fan.o_01;
    f fan.o_12;
    f fan.o_11;
  ]

let reds = component fst3
let greens = component snd3
let blues = component trd3

let send_frame device channel frame =
  let fan1 = fst frame in
  let fan2 = snd frame in

  let%lwt _ = send_commands device [
      WriteLedMode (channel, Software);
      WriteLedColorValues (R, (reds fan2  ) @ (reds fan1));
      WriteLedColorValues (G, (greens fan2) @ (greens fan1));
      WriteLedColorValues (B, (blues fan2 ) @( blues fan1));
      WriteLedTrigger;
    ] in
  Lwt.return ()

let open_controller ?(vendor_id=0x1b1c) ?(product_id=0x0c0b) ?(interface=0) () =
  let controller = {
    usb_device = open_device_with ~vendor_id ~product_id;
    interface;
    kernel_driver_was_attached = false;
  } in
  if USB.kernel_driver_active controller.usb_device interface
  then (
    controller.kernel_driver_was_attached <- true;
    detach_kernel_driver controller.usb_device interface
  );

  let%lwt () = claim_interface controller.usb_device interface in

  let%lwt () = send_commands controller [
      WriteLedGroupsClear First;
      WriteLedClear First;
      WriteLedMode (First, Hardware);
      WriteLedGroupsClear Second;
      WriteLedClear Second;
      WriteLedMode (Second, Hardware);
      WriteLedTrigger
    ] in

  Lwt.return controller

let set_hw_animation controller conf =
  send_commands controller [
      WriteLedGroupsClear First;
      WriteLedClear First;
      WriteLedMode (First, Hardware);
      WriteLedGroupSet conf;

      WriteLedTrigger
    ]

let default_effect_configuration =
  (* let color1 = 0xff, 0x37, 0x00 in
  let color2 = 0x37, 0x00, 0xff in *)
  {
    mode = Static;
    speed = Slow;
    dir = Clockwise;
    color_mode = Alternating;
    color1 = black;
    color2 = black;
    color3 = black;
  }

let close_controller ?(last_conf=default_effect_configuration) controller =
  let%lwt () = send_commands controller [
      WriteLedGroupsClear First;
      WriteLedClear First;
      WriteLedMode (First, Hardware);
      WriteLedGroupSet last_conf;

      WriteLedGroupsClear Second;
      WriteLedClear Second;
      WriteLedMode (Second, Hardware);
      WriteLedGroupSet last_conf;

      WriteLedTrigger
    ] in

  if controller.kernel_driver_was_attached
  then attach_kernel_driver controller.usb_device controller.interface;
  let%lwt () = release_interface controller.usb_device controller.interface in
  close controller.usb_device;
  Lwt.return ()
