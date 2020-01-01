(* This binding to getloadavg has been stoled by Jane Street Shell library:
   https://github.com/janestreet/shell
*)

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"