open Constants

let name_visible_timeout = 10.0

let rainbow_radar ~state _time =
  let open Corsair_Lighting_Node_Pro in
  let open Color in
  let count = state in
  let duration = 1.0 /. 12.0 in

  let h, s, l = (count mod 360 |> float_of_int, 1.0, 0.5) in
  let dimmed_color = Color.of_hsl h s (l /. 2.) in
  let more_dimmed_color = Color.of_hsl h s (l /. 4.) in
  let color = Color.of_hsl h s l in

  let radar_prev_2 = (count - 2) mod 12 in
  let radar_prev_1 = (count - 1) mod 12 in
  let radar_curr_p = count mod 12 in

  (* FAN 1 *)
  let fan1 = change_led fan_off (Outer (radar_prev_2 + 1)) (to_color more_dimmed_color) in
  let fan1 = change_led fan1 (Outer (radar_prev_1 + 1)) (to_color dimmed_color) in
  let fan1 = change_led fan1 (Outer (radar_curr_p + 1)) (color |> to_color) in

  let fan1 = change_led fan1 (Inner 1) (to_color more_dimmed_color) in
  let fan1 = change_led fan1 (Inner 2) (to_color more_dimmed_color) in
  let fan1 = change_led fan1 (Inner 3) (to_color more_dimmed_color) in
  let fan1 = change_led fan1 (Inner 4) (to_color more_dimmed_color) in

  (* FAN 2 *)
  (* https://www.wolframalpha.com/input/?i=%28Sin%5B%28x*2*pi%29%2F192%5D+%2B+1%29%2F4 *)
  (*let l = (1. /. 4.) *. (sin ((pi *. (float_of_int count)) /. 96.) +. 1.) in*)

  (* https://www.wolframalpha.com/input/?i=plot+Abs%5B%28Sin%5B%28x*2*pi%29%2F192%5D+%29%2F2%5D%2C+x+%3D+0+to+192 *)
  (*let l = (1. /. 2.) *. abs_float(sin ((pi *. (float_of_int count)) /. 96.)) in
    let c = of_hsl h s l |> to_rgba in
    let fan2 = monochrome (c.r, c.g, c.b) in*)

  (* https://gamedev.stackexchange.com/questions/53419/animating-a-background-pulse-with-easing-equation
   * https://www.wolframalpha.com/input/?i=plot+%28%28%28%280.5-x%29%2F12%29+-+Floor%28%280.5-x%29%2F12%29%29%5E3%29%2F2%2C+x+%3D+0+to+24
   *)
  let l_out =
    (((1. /. 24. *. (0.5 -. float_of_int count)) -. floor (1. /. 24. *. (0.5 -. float_of_int count))) ** 3.) /. 2.
  in
  let l_in =
    (((1. /. 24. *. (0.5 -. float_of_int (count + 2))) -. floor (1. /. 24. *. (0.5 -. float_of_int (count + 2)))) ** 3.)
    /. 2.
  in
  let c_out = of_hsl h s l_out |> to_rgba in
  let c_in = of_hsl h s l_in |> to_rgba in
  let fan2 = monochrome (c_out.r, c_out.g, c_out.b) in
  let fan2 = change_led fan2 (Inner 1) (c_in.r, c_in.g, c_in.b) in
  let fan2 = change_led fan2 (Inner 2) (c_in.r, c_in.g, c_in.b) in
  let fan2 = change_led fan2 (Inner 3) (c_in.r, c_in.g, c_in.b) in
  let fan2 = change_led fan2 (Inner 4) (c_in.r, c_in.g, c_in.b) in

  (Some ((fan1, fan2), duration), count + 1)

type 'a anim_type =
  | Hw of Corsair_Lighting_Node_Pro.effect_configuration
  | Sw of 'a Animation.animation

type 'a animation = {
  anim_type : 'a anim_type;
  name : string;
}

let default_off_conf = Corsair_Lighting_Node_Pro.default_effect_configuration

let animations =
  [
    { anim_type = Hw default_off_conf; name = "Off" };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Rainbow_wave;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Rainbow wave";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Color_shift;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Color shift";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Color_pulse;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Color pulse";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Color_wave;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Color wave";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Visor;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Visor";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Marquee;
              speed = Fast;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = (0x24, 0x26, 0x4d);
              color2 = black;
              color3 = black;
            };
      name = "Marquee";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Blink;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Blink";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Sequential;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Sequential";
    };
    {
      anim_type =
        Hw
          Corsair_Lighting_Node_Pro.
            {
              mode = Rainbow;
              speed = Medium;
              dir = Clockwise;
              color_mode = Random_color;
              color1 = black;
              color2 = black;
              color3 = black;
            };
      name = "Rainbow";
    };
    { anim_type = Sw rainbow_radar; name = "Rainbow radar" };
  ]

type status = {
  current_animation : int;
  show_animation_name : bool;
  show_started : float;
}
[@@deriving yojson, show]

let default_status = { current_animation = 0; show_animation_name = false; show_started = 0.0 }
let get_current_name s = (List.nth animations s.current_animation).name
let toggle_show s = { s with show_animation_name = not s.show_animation_name; show_started = Unix.gettimeofday () }

let change_animation s =
  let how_many = List.length animations in
  { s with current_animation = (s.current_animation + 1) mod how_many; show_started = Unix.gettimeofday () }

class ['a] modulo instance_name status_pipe color_off color_hw color_sw sep : ['a] Lwt_module.modulo =
  object (self)
    constraint 'a = [`r | `w]
    inherit ['a] Lwt_module.base_modulo instance_name status_pipe
    val! name = "fan_leds"
    val mutable state = default_status
    val mutable controller = None
    val mutable stop_animation = Lwt.wait ()

    method! private loop () =
      let%lwt () = Lwt_unix.sleep 0.25 in
      let%lwt () =
        if state.show_animation_name = true
        then begin
          if Unix.gettimeofday () -. state.show_started > name_visible_timeout
          then begin
            state <- toggle_show state;
            let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
            Lwt.return_unit
          end
          else Lwt.return_unit
        end
        else Lwt.return_unit
      in
      self#loop ()

    method private get_color () =
      if state.current_animation = 0
      then color_off
      else begin
        match (List.nth animations state.current_animation).anim_type with
        | Sw _ -> color_sw
        | Hw _ -> color_hw
      end

    method! json () =
      let color = self#get_color () in
      let full_text, sep =
        match controller with
        | Some _ -> begin
          if state.show_animation_name then (spf "%s %s" fan (get_current_name state), sep) else (fan, sep)
        end
        | None -> ("", false)
      in
      let bl =
        {
          I3bar_protocol.Block.default with
          full_text;
          short_text = full_text;
          color;
          name;
          instance = instance_name;
          separator = sep;
        }
      in
      Yojson.Safe.to_string (I3bar_protocol.Block.to_yojson bl) |> Lwt.return

    method private set_animation () =
      let current = List.nth animations state.current_animation in
      match controller with
      | Some c -> begin
        match current.anim_type with
        | Hw conf -> Corsair_Lighting_Node_Pro.set_hw_animation c conf
        | Sw anim -> begin
          let stop_anim_or_thread = Lwt.choose [fst stopped; fst stop_animation] in
          Utils.detach_promise (fun () ->
            Animation.run_animation c stop_anim_or_thread ~init:2 ~a:anim
          ) "Case_fan_leds.modulo #set_animation method, running run_animation";

          Lwt.return_unit
        end
      end
      | None ->
        Logs.info (fun m ->
            m "(%s,%s) set_animation: Corsair Lighting Node Pro not detected during initialization" name instance_name)
        |> Lwt.return

    method! private read_loop () =
      let%lwt maybe_msg = Lwt_pipe.read pipe in
      let%lwt () =
        match maybe_msg with
        | Some { button = b; _ } -> begin
          match b with
          | 1 -> begin
            state <- change_animation state;
            Lwt.wakeup (snd stop_animation) ();
            let%lwt () = Lwt_unix.sleep 0.2 in
            stop_animation <- Lwt.wait ();

            let%lwt () = self#set_animation () in

            let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
            Lwt.return ()
          end
          | 3 -> begin
            state <- toggle_show state;
            let%lwt _ = Lwt_pipe.write status_pipe (`Status_change (name, instance_name)) in
            Lwt.return ()
          end
          | _ -> Lwt.return ()
        end
        | None -> Lwt.return ()
      in
      self#read_loop ()

    method private init_controller () =
      try%lwt
        begin
          match controller with
          | None -> begin
            let%lwt controller' = Corsair_Lighting_Node_Pro.open_controller () in
            controller <- Some controller';
            Lwt.return `Ok
          end
          | Some c -> begin
            let%lwt () = Corsair_Lighting_Node_Pro.close_controller c in
            let%lwt () = Lwt_unix.sleep 0.2 in
            let%lwt controller' = Corsair_Lighting_Node_Pro.open_controller () in
            controller <- Some controller';
            Lwt.return `Ok
          end
        end
      with exn ->
        let open Printexc in
        let exn_str = to_string exn in
        let backtrace = get_backtrace () in
        Logs.info (fun m ->
            m "Exception during initialization of Corsair Lighting Node Pro\n\n%s\n%s" exn_str backtrace);
        Lwt.return `Init_problems

    method! dump_state () = status_to_yojson state |> Yojson.Safe.to_string |> Lwt.return

    method! load_state (s : string) =
      try%lwt
        match Yojson.Safe.from_string s |> status_of_yojson with
        | Ok status -> Lwt.return (state <- status)
        | Error _ -> Lwt.return_unit
      with _ -> Lwt.return_unit

    method! run () : unit Lwt.t =
      let%lwt init_result = self#init_controller () in
      let%lwt () =
        match init_result with
        | `Ok -> begin
          let%lwt () = self#set_animation () in
          Utils.detach_promise self#loop "Case_fan_leds.modulo #loop method";
          Utils.detach_promise self#read_loop "Case_fan_leds.modulo #read_loop method";
          Lwt.return_unit
        end
        | `Init_problems -> Lwt.return_unit
      in
      Lwt.wakeup (snd ready) ();
      fst ready

    method! stop () : unit Lwt.t =
      let%lwt () = fst ready in
      Lwt.wakeup (snd stopped) ();
      let%lwt () = Lwt_unix.sleep 0.2 in

      let last_conf =
        match (List.nth animations state.current_animation).anim_type with
        | Sw _ -> default_off_conf
        | Hw c -> c
      in

      let%lwt () =
        match controller with
        | Some c -> Corsair_Lighting_Node_Pro.close_controller ~last_conf c
        | None -> Lwt.return_unit
      in
      fst stopped
  end
