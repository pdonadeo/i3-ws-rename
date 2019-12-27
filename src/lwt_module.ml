open Modules_inventory


class type virtual ['a] modulo =
  object
    constraint 'a = [< `r | `w]

    val ready : unit Lwt.t * unit Lwt.u
    val stopped : unit Lwt.t * unit Lwt.u
    val mutable stop_request : bool
    val tags : (string * string) list
    val name : string
    val pipe : (message_to_modules, 'a) Lwt_pipe.t
    val status_pipe : (message_to_status, 'a) Lwt_pipe.t

    method pipe : (message_to_modules, 'a) Lwt_pipe.t
    method stopped : unit Lwt.t

    method run : unit -> unit Lwt.t
    method ready : unit -> unit Lwt.t
    method stop : unit -> unit Lwt.t
    method name : string
    method instance : string
    method json : unit -> string
  end

class virtual ['a] base_modulo instance status_pipe =
  object (self)
    constraint 'a = [< `r | `w]

    val ready = Lwt.wait ()
    val stopped = Lwt.wait ()
    val mutable stop_request = false
    val tags = ["GENERIC_THREAD", ""]
    val name = "GENERIC NAME"
    val pipe = Lwt_pipe.create ~max_size:10 ()
    val status_pipe = status_pipe

    (* method private reader : 'message Lwt_pipe.Reader.t = pipe *)
    method pipe : (message_to_modules, 'a) Lwt_pipe.t = pipe
    method stopped = fst stopped

    method private loop () =
      let%lwt () = Lwt_io.printf "Default loop\n%!" in
      let%lwt () = Lwt_unix.sleep 1.0 in
      self#loop ()

    method run () : unit Lwt.t =
      Lwt.async (self#loop);
      Lwt.wakeup (snd ready) ();
      (fst ready)

    method stop () : unit Lwt.t =
      (* Log.Global.debug ~tags "Thread is terminating"; *)
      let%lwt () = (fst ready) in
      stop_request <- true;
      fst stopped

    method ready () : unit Lwt.t =
      (fst ready)

    method name = name

    method instance : string = instance

    method json () = "{}"
  end
