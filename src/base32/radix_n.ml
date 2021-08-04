(*
    The code for the base32 decoding is taken from
    the orsetto library, by James Woodyatt:
    https://bitbucket.org/jhw/orsetto/src/principal/

    I didn'n use it directly because at present orsetto doesn't
    install using opam, due to its dependency on "omake" which
    seems to be broken.
*)


type check = Digit | Skip | Pad | Invalid
type req = Must | Should

type basis = Basis of {
    pad: (req * char) option;
    check: char -> check;
    decode: char -> int;
    encode: int -> char;
    radix: int;
}

type 'n aux = Aux of {
    pad: (req * char) option;
    check: char -> check;
    width: int;
    digits: int;
    octets: int;
    init: 'n;
    of_octet: 'n -> char -> 'n;
    of_digit: 'n -> char -> 'n;
    of_pad: 'n -> 'n;
    to_octet: 'n -> int -> char;
    to_digit: 'n -> int -> char;
    wordstr: 'n -> string
}

let auxfail n =
    n |> Printf.sprintf "Radix_n: radix %u words too large." |> invalid_arg

let aux0 (Basis b) =
    let pad = b.pad and check = b.check in
    let decode = b.decode and encode = b.encode in
    let init = 0
    and of_octet w c = (w lsl 8) lor (Char.code c)
    and to_octet w ws = (w lsr ws) land 0xff |> Char.chr
    and to_digit mask w ws = (w lsr ws) land mask |> encode
    in
    let wordstr n = Printf.sprintf "0x%X" n in
    match b.radix with
    | 16 ->
        let width = 4 and digits = 2 and octets = 1 in
        let of_pad w = w lsl width in
        let of_digit w c = (of_pad w) lor (decode c) in
        let to_digit = to_digit 0xf in
        Aux {
            pad; check; width; octets; digits; init; of_octet; of_pad;
            of_digit; to_octet; to_digit; wordstr
        }
    | 32 when Sys.int_size > 31 ->
        let width = 5 and digits = 8 and octets = 5 in
        let of_pad w = w lsl width in
        let of_digit w c = (of_pad w) lor (decode c) in
        let to_digit = to_digit 0x1f in
        Aux {
            pad; check; width; octets; digits; init; of_octet; of_pad;
            of_digit; to_octet; to_digit; wordstr
        }
    | 64 ->
        let width = 6 and digits = 4 and octets = 3 in
        let of_pad w = w lsl width in
        let of_digit w c = (of_pad w) lor (decode c) in
        let to_digit = to_digit 0x3f in
        Aux {
            pad; check; width; octets; digits; init; of_octet; of_pad;
            of_digit; to_octet; to_digit; wordstr
        }
    | r ->
        auxfail r

exception Error

let decode_seq_aux =
    let push tin =
        match tin with
        | None -> None
        | Some (ti, tn) -> Some (succ ti, tn)
    and tin0 = function
        | None ->
            None
        | Some n ->
            if n < 0 then invalid_arg "Radix_n: length < 0";
            Some (0, n)
    and ofwp ~aux wp =
        let Aux a = aux in
        if wp > 0 && wp < a.digits then
            (a.digits - wp) * a.width / 8
        else
            a.octets
    in
    let rec loop ~aux ?tin ~w ~wp ~ws s =
        match s () with
        | Stdlib.Seq.Nil -> (unpad[@tailcall]) ~aux ?tin ~w ~ws ~wp wp
        | Stdlib.Seq.Cons (hd, tl) -> (check[@tailcall]) ~aux ?tin ~w ~wp ~ws ~tl hd
    and unpad ~aux ?tin ~w ~ws ~wp wn =
        let Aux a = aux in
        if wn < a.digits then begin
            if wn > 0 then begin
                match a.pad with
                | Some (Must, _) ->
                    raise Error
                | (Some (Should, _) | None) ->
                    let w = a.of_pad w and ws = ws + a.width in
                    (unpad[@tailcall]) ~aux ?tin ~w ~ws ~wp (pred wp)
            end
            else begin
                let wi = 0 and wn = ofwp ~aux wp in
                (finish[@tailcall]) ~aux ?tin ~w ~wi ~ws ~wn ()
            end
        end
        else
            Stdlib.Seq.Nil
    and check ~aux ?tin ~w ~wp ~ws ~tl c =
        let Aux a = aux in
        match a.check c with
        | Digit -> (digit[@tailcall]) ~aux ?tin ~w ~wp ~ws ~tl c
        | Skip -> (loop[@tailcall]) ~aux ?tin ~w ~wp ~ws tl
        | Pad -> (startpad[@tailcall]) ~aux ?tin ~w ~wp ~ws tl
        | Invalid -> raise Error
    and digit ~aux ?tin ~w ~wp ~ws ~tl c =
        let Aux a = aux in
        let w = a.of_digit w c and ws = ws + a.width and wp = pred wp in
        if wp > 0 then
            (loop[@tailcall]) ~aux ?tin ~w ~wp ~ws tl
        else
            (octet[@tailcall]) ~aux ?tin ~w ~wi:0 ~ws ~tl ()
    and octet ~aux ?tin ~w ~wi ~ws ~tl () =
        let Aux a = aux in
        if wi < a.octets then begin
            let ws = ws - 8 and wi = succ wi in
            let c = a.to_octet w ws and tin = push tin in
            Stdlib.Seq.Cons (c, octet ~aux ?tin ~w ~wi ~ws ~tl)
        end
        else
            (loop[@tailcall]) ~aux ?tin ~w:a.init ~wp:a.digits ~ws:0 tl
    and startpad ~aux ?tin ~w ~wp ~ws tl =
        let Aux a = aux in
        if a.pad = None || not (wp > 0 && wp < a.digits) then raise Error;
        let wn = ofwp ~aux wp and wp = pred wp in
        let w = a.of_pad w and ws = ws + a.width in
        (nextpad[@tailcall]) ~aux ?tin ~w ~wp ~wn ~ws tl
    and nextpad ~aux ?tin ~w ~wp ~wn ~ws tl =
        let Aux a = aux in
        match tl () with
        | Stdlib.Seq.Nil ->
            if wp > 0 then
                (unpad[@tailcall]) ~aux ?tin ~w ~ws ~wp wn
            else
                (finish[@tailcall]) ~aux ?tin ~w ~wi:0 ~wn ~ws ()
        | Stdlib.Seq.Cons (hd, tl) ->
            match a.check hd with
            | Pad when wp > 0 && wp < a.digits ->
                let w = a.of_pad w and ws = ws + a.width and wp = pred wp in
                (nextpad[@tailcall]) ~aux ?tin ~w ~wp ~wn ~ws tl
            | (Digit | Skip | Pad | Invalid) ->
                raise Error
    and finish ~aux ?tin ~w ~wi ~wn ~ws () =
        let Aux a = aux in
        if wi < wn then begin
            let wi = succ wi and ws = ws - 8 in
            let c = a.to_octet w ws and tin = push tin in
            Stdlib.Seq.Cons (c, finish ~aux ?tin ~w ~wi ~wn ~ws)
        end
        else begin
            match tin with
            | Some (ti, tn) when ti < tn -> raise Error
            | (Some _ | None) -> Stdlib.Seq.Nil
        end
    in
    let enter ~aux ?n s () =
        let Aux a = aux in
        (loop[@tailcall]) ~aux ?tin:(tin0 n) ~w:a.init ~wp:a.digits ~ws:0 s
    in
    enter

let decode_string_aux ~aux ?n s =
    let z = decode_seq_aux ~aux ?n @@ String.to_seq s in
    try
        Some begin
            match n with
            | None -> Buffer.(contents @@ of_seq z)
            | Some n -> String.of_seq @@ Seq.limit n z
        end
    with
    | Error ->
        None

module type Profile = sig
    val basis: basis

    val decode_string: ?n:int -> string -> string option
end

let create basis =
    let aux = aux0 basis in
    let module M = struct
        let basis = basis
        let decode_string = decode_string_aux ~aux
    end in
    (module M : Profile)
