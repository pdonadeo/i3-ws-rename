(*
    The code for the base32 decoding is taken from
    the orsetto library, by James Woodyatt:
    https://bitbucket.org/jhw/orsetto/src/principal/

    I didn'n use it directly because at present orsetto doesn't
    install using opam, due to its dependency on "omake" which
    seems to be broken.
*)


open Seq

type progress = Rdy | Fin

let rec repeat x =
    fun () -> Cons (x, repeat x)

let rec compose f c () =
    match f c with
    | None -> Nil
    | Some (c, v) -> Cons (v, compose f c)

let rec zipcompose f c () =
    match f c with
    | None -> Nil
    | Some (c, _ as v) -> Cons (v, zipcompose f c)

let range a b =
    if a > b then invalid_arg (__MODULE__ ^ ".range: a > b.");
    let rec loop i () =
        let next = if i < b && i < max_int then loop (succ i) else fin in
        Cons (i, next)
    and fin () = Nil
    in loop a

let of_option opt () =
    match opt with
    | Some v -> Cons (v, Seq.empty)
    | None -> Nil

let rec of_list s () =
    match s with
    | hd :: tl -> Cons (hd, of_list tl)
    | [] -> Nil

let rec of_channel c () =
    match input_char c with
    | exception End_of_file -> Nil
    | b -> Cons (b, of_channel c)

let cons v s () = Cons (v, s)

let rec exnsafe s () =
    match s () with
    | exception x -> Cons (Error x, Seq.empty)
    | Cons (v, c) -> Cons (Ok v, exnsafe c)
    | Nil -> Nil

let peek =
    let rec loop v s () =
        match s () with
        | Nil -> Cons ((v, Fin), Seq.empty)
        | Cons (v', s) -> Cons ((v', Rdy), loop v' s)
    in
    fun s () ->
        match s () with Nil -> Nil | Cons (v, c) -> loop v c ()

let rec optlift s () =
    match s () with
    | Cons (v, s) -> Cons (Some v, optlift s)
    | Nil -> repeat None ()

let rec optdown s () =
    match s () with
    | Cons (Some v, s) -> Cons (v, optdown s)
    | (Cons (None, _) | Nil) -> Nil

let concat =
    let rec loop a b () =
        match a () with
        | Nil -> b ()
        | Cons (v, a) -> Cons (v, loop a b)
    in
    loop

let rec limit n s () =
    match s () with
    | Cons (v, s) when n > 0 -> Cons (v, limit (pred n) s)
    | (Cons _ | Nil) -> Nil

let buffermap_aux =
    let rec loop b i s () =
        match s () with
        | Nil ->
            if i > 0 then Cons (Bytes.sub_string b 0 i, Seq.empty) else Nil
        | Cons (c, s) ->
            Bytes.unsafe_set b i c;
            if i < Bytes.length b then
                (loop[@tailcall]) b (succ i) s ()
            else
                Cons (Bytes.to_string b, loop b 0 s)
    in
    let enter n s () =
        (loop[@tailcall]) (Bytes.create n) 0 s ()
    in
    enter

let rec zip sa sb () =
    match sa () with
    | Nil -> Nil
    | Cons (a, sa) ->
        match sb () with
        | Nil -> Nil
        | Cons (b, sb) -> Cons ((a, b), zip sa sb)

module P = struct
    type 'a tz = 'a tz0 Lazy.t and 'a tz0 = Nil0 | P0 of 'a * 'a tz

    let rec to_persist c =
        lazy begin
            match c () with
            | Nil -> Nil0
            | Cons (v, c) -> P0 (v, to_persist c)
        end

    let rec of_persist s () =
        match Lazy.force s with
        | Nil0 -> Nil
        | P0 (v, s) -> Cons (v, of_persist s)
end

let persist c = P.to_persist c |> P.of_persist

let buffermap n s = buffermap_aux n s |> persist

type slice = Slice of { b: bytes; n: int }

let of_buffered_channel =
    let rec reader n c () =
        let b = Bytes.create n in
        let n = input c b 0 n in
        if n > 0 then Cons (Slice { b; n }, reader n c) else Nil
    in
    let rec byslice s () =
        match s () with
        | Cons (Slice hd, tl) ->
            let w3 = hd.b, hd.n, tl in
            cursor w3 0 ()
        | Nil ->
            Nil
    and cursor (b, n, tl as w) i () =
        if i < n then
            Cons (Bytes.unsafe_get b i, cursor w (succ i))
        else
            byslice tl ()
    in
    let enter n c = reader n c |> persist |> byslice in
    enter

let rec gate g c () =
    g#syn;
    let c0 = c () in
    g#fin;
    match c0 with
    | Nil -> Nil
    | Cons (hd, tl) -> Cons (hd, gate g tl)

let join =
    let rec inner s ss () =
        match s () with
        | Nil -> (outer[@tailcall]) ss ()
        | Cons (v, s) -> Cons (v, inner s ss)
    and outer ss () =
        match ss () with
        | Nil -> Nil
        | Cons (s, ss) -> (inner[@tailcall]) s ss ()
    in
    outer

let unzip s = map fst s, map snd s

let rec run s = match s () with Nil -> () | Cons (_, s) -> run s

let empty s = match s () with Nil -> true | Cons _ -> false
let head s = match s () with  Cons (hd, _) -> hd | Nil -> raise Not_found
let tail s = match s () with  Cons (_, tl) -> tl | Nil -> raise Not_found
let tryopt s = match s () with Nil -> None | Cons (v, _) -> Some v

let length =
    let rec loop n s =
        match s () with
        | Cons (_, s) -> (loop[@tailcall]) (succ n) s
        | Nil -> n
    in
    fun s -> loop 0 s

let canshift =
    let rec loop n s =
        match s () with
        | Nil -> n == 0
        | Cons (_, s) -> if n > 0 then (loop[@tailcall]) (pred n) s else true
    in
    fun n s ->
        if n < 0 then invalid_arg (__MODULE__ ^ ".canshift: n < 0.");
        loop n s

let shift =
    let rec loop n s () =
        match s () with
        | Cons (_, s) when n > 0 -> (loop[@tailcall]) (pred n) s ()
        | (Cons _ | Nil) as s -> s
    in
    fun n s ->
        if n < 0 then invalid_arg (__MODULE__ ^ ".shift: n < 0.");
        loop n s

let rec has_none f s =
    match s () with
    | Nil -> true
    | Cons (v, s) -> not (f v) && (has_none[@tailcall]) f s

let rec has_some f s =
    match s () with
    | Nil -> false
    | Cons (v, s) -> f v || (has_some[@tailcall]) f s

let has_all =
    let rec loop r f s =
        match s () with
        | Nil -> r
        | Cons (v, s) -> f v && (loop[@tailcall]) true f s
    in
    let enter f s = (loop[@tailcall]) false f s in
    enter

let rec memf f v s =
    match s () with
    | Nil -> false
    | Cons (v', s) -> if f v v' then true else (memf[@tailcall]) f v s

let mem v s = memf Stdlib.(=) v s

let rec find f s =
    match s () with
    | Nil -> raise Not_found
    | Cons (v, s) -> if f v then v else (find[@tailcall]) f s

let rec search f s =
    match s () with
    | Nil -> None
    | Cons (v, s) -> if f v then Some (v, s) else (search[@tailcall]) f s

let rec searchmap f s =
    match s () with
    | Nil -> None
    | Cons (v, s) ->
        match f v with
        | None -> (searchmap[@tailcall]) f s
        | Some v -> Some (v, s)

let rec eqf f a b =
    match a () with
    | Nil ->
        if b () = Nil then true else false
    | Cons (v1, a) ->
        match b () with
        | Nil ->
            false
        | Cons (v2, b) ->
            if not (f v1 v2) then false else (eqf[@tailcall]) f a b

let rec cmpf f a b =
    match a () with
    | Nil ->
        begin match b () with
            | Nil -> 0
            | Cons _ -> -1
        end
    | Cons (v1, a) ->
        begin match b () with
            | Nil -> 1
            | Cons (v2, b) ->
                let d = f v1 v2 in
                if d <> 0 then d else (cmpf[@tailcall]) f a b
        end

let equal a b = eqf Stdlib.( = ) a b
let compare a b = cmpf Stdlib.compare a b

let rec phyeq a b =
    match a () with
    | Nil ->
        begin match b () with
            | Nil -> true
            | Cons _ -> false
        end
    | Cons (v1, a) ->
        begin match b () with
            | Nil ->
                false
            | Cons (v2, b) ->
                if v1 == v2 then (phyeq[@tailcall]) a b else false
        end

let reverse =
    let rec loop r s =
        match s () with
        | Nil -> r
        | Cons (v, s) -> (loop[@tailcall]) (v :: r) s
    in
    fun s -> loop [] s

let to_list s = reverse s |> List.rev

let rec to_buffer b s =
    match s () with
    | Nil ->
        ()
    | Cons (v, s) ->
        Buffer.add_char b v;
        (to_buffer[@tailcall]) b s

let rec to_channel ch s =
    match s () with
    | Nil ->
        ()
    | Cons (v, s) ->
        output_char ch v;
        (to_channel[@tailcall]) ch s

module type Vector = sig
    type 'a element and 'a vector
    val name: string
    val length: 'a vector -> int
    val init: int -> (int -> 'a element) -> 'a vector
    val get: 'a vector -> int -> 'a element
    val set: 'a vector -> int -> 'a element -> unit
end

module Operations(V: Vector) = struct
    let invalid msg =
        invalid_arg (Printf.sprintf "%s.check: %s." V.name msg)

    let fail msg =
        failwith (Printf.sprintf "%s.init: %s." V.name msg)

    let rec of_unsafe v i j () =
        if i < j then Cons (V.get v i, of_unsafe v (succ i) j) else Nil

    let rec to_unsafe v i j s =
        match s () with
        | Cons (e, s) when i < j ->
            V.set v i e;
            (to_unsafe[@tailcall]) v (succ i) j s
        | (Cons _ | Nil) ->
            ()

    let check v i j =
        if i < 0 then invalid "index";
        if i > j || j > V.length v then invalid "length"

    let of_sub v i n =
        let j = i + n in
        check v i j;
        (of_unsafe[@tailcall]) v i j

    let to_sub v i n s =
        let j = i + n in
        check v i j;
        (to_unsafe[@tailcall]) v i j s

    let of_vec v () = (of_sub[@tailcall]) v 0 (V.length v) ()

    let to_vec =
        let consume r _ =
            match !r () with
            | Nil ->
                fail "early termination."
            | Cons (v, c) ->
                r := c;
                v
        in
        let enter n s = V.init n (consume (ref s)) in
        enter
end

module V_bytes = struct
    type 'a element = char and 'a vector = bytes
    let name = __MODULE__
    let init = Bytes.init
    let length = Bytes.length
    external get: bytes -> int -> char = "%bytes_unsafe_get"
    external set: bytes -> int -> char -> unit = "%bytes_unsafe_set"
end

module V_string = struct
    type 'a element = char and 'a vector = string
    let name = __MODULE__
    let init = String.init
    let length = String.length
    external get: string -> int -> char = "%string_unsafe_get"
    let set _ _ _ = assert false
end

module V_array = struct
    type 'a element = 'a and 'a vector = 'a array
    let name = __MODULE__
    let init = Array.init
    let length = Array.length
    external get: 'a array -> int -> 'a = "%array_unsafe_get"
    external set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
end

module Op_bytes = Operations(V_bytes)
module Op_string = Operations(V_string)
module Op_array = Operations(V_array)

let to_subbytes = Op_bytes.to_sub
let of_subbytes = Op_bytes.of_sub
let of_bytes = Op_bytes.of_vec
let to_bytes = Op_bytes.to_vec

let of_substring = Op_string.of_sub
let of_string = Op_string.of_vec
let to_string = Op_string.to_vec

let to_subarray = Op_array.to_sub
let of_subarray = Op_array.of_sub
let of_array = Op_array.of_vec
let to_array = Op_array.to_vec

let string_pipe f s =
    let b = Buffer.create 0 in
    s |> of_string |> f |> to_buffer b;
    Buffer.contents b

let bytes_pipe f s =
    let b = Buffer.create 0 in
    s |> of_bytes |> f |> to_buffer b;
    Buffer.to_bytes b

let array_pipe f s =
    s |> of_array |> f |> to_list |> Array.of_list

module Infix = struct
    let ( @: ) = cons
    let ( @+ ) = concat
end

let predicate = has_all
