open Logs

let ts () =
  let open Unix in

  let n = gettimeofday () in
  let {tm_sec = sec;
       tm_min = min;
       tm_hour = hour;
       tm_mday = mday;
       tm_mon = mon;
       tm_year = year;
       tm_wday = _wday;
       tm_yday = _yday;
       tm_isdst = _isdst} = localtime n in
  let milli = (n -. (Float.floor n)) *. 1000. |> Float.round |> Float.to_int |> string_of_int in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%s" (year + 1900) (mon + 1) mday hour min sec milli

let my_reporter formatter =
  let report _src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags fmt ->
    ignore(tags);
    Format.kfprintf k formatter ("%s %a @[" ^^ fmt ^^ "@]@.") (ts ()) Logs.pp_header (level, header)
  in
  { report }

let lwt_file_reporter (log_fname : string option) =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let fmt, flush = buf_fmt ~like:Fmt.stdout in
  let reporter = my_reporter fmt in
  let report src level ~over k msgf =
    let k () =
      let write () =
        let module U = Unix in
        match log_fname with
        | None -> Lwt.return ()
        | Some log_fname -> begin
          Lwt_io.with_file
            ~flags:[U.O_WRONLY; U.O_APPEND; U.O_CREAT]
            ~mode:Lwt_io.output log_fname
            (fun oc -> Lwt_io.write oc (flush ()))
        end in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }
