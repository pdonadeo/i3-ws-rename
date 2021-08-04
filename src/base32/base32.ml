(*
    The code for the base32 decoding is taken from
    the orsetto library, by James Woodyatt:
    https://bitbucket.org/jhw/orsetto/src/principal/

    I didn'n use it directly because at present orsetto doesn't
    install using opam, due to its dependency on "omake" which
    seems to be broken.
*)


open Radix_n

let std =
    let radix = 32
    and pad = Some (Must, '\061')
    and check = function
        | '\097'..'\122' -> Digit
        | '\065'..'\090' -> Digit
        | '\050'..'\055' -> Digit
        | '\061' -> Pad
        | _ -> Invalid
    and decode c =
        let i = Char.code c in
        match c with
        | '\097'..'\122' -> i - 97
        | '\065'..'\090' -> i - 65
        | '\050'..'\055' -> i - 24
        | _ -> assert (not true); 0
    and encode i =
        Char.chr begin
            match i with
            | _ when i < 26 -> i + 65
            | _ when i < 32 -> i + 24
            | _ -> assert (not true); 0
        end
    in
    Basis { pad; check; decode; encode; radix }

module Std = (val create std : Profile)
