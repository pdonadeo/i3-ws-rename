type message = [
  | `Status_change of string * string
]

type module_instance = {
  name : string;
  instance : string;
  json : unit -> string;
}

module type STATUS_MODULE = sig
  type t = module_instance

  val name : string
  val create : (message, [< `r | `w > `w ]) Lwt_pipe.t -> string -> t Lwt.t
end