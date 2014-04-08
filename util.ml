open Str
open List

let rec flatten list =
  let rec subf(subl, r) =
    match subl with
      [] -> r
    | x::subl -> subf(subl, r@[x])
  in
  subf(list, [])

let length list =
  let rec inner subl r =
    match subl with
      [] -> r
    | _  -> inner (List.tl subl) (r + 1)
  in
  inner list 0

let rec append l1 l2 =
  match l1 with
    []    -> l2
  | [x]   -> x::l2
  | x::xs -> x::(append xs l2)

let reverse ls =
  let rec inner(subl, r) =
    match subl with
      [] -> r
    | _  -> inner((List.tl subl), (List.hd subl)::r)
  in
  inner(ls, [])

let rec member(item, list) =
  (* if (List.hd list) = item then list *)
  (* else member(item, (List.tl list)) *)
  match list with
    []    -> []
  | x::xs -> if x = item then x::xs
    else member(item, xs)

let rec reduce list f seed =
  match list with
    []    -> seed
  | x::xs -> f x (reduce xs f seed)

let rec remove x ls =
  match ls with
    []	  -> []
  | y::ys -> if x = y then remove x ys else y::(remove x ys)

let reader filename f =
  let fin = open_in filename in
  let rec inner r =
    try let c = (f fin) in
	inner (c :: r)
    with
      End_of_file -> close_in fin; List.rev r
  in
  inner []
    
let split str sep =
  let rec inner subst r =
    try let pos = String.index subst sep and
	len = String.length subst
	in
	inner (String.sub subst (pos + 1) (len - pos -1)) ((String.sub subst 0 pos)::r)
    with
      Not_found -> List.rev (subst::r)
  in
  inner str []

(* "f:/kcsv2/00263129_FKAC522_20130612_011.csv" *)
let csv_parser inch =
  split (input_line inch) ','

let csv_reader filename =
  reader filename csv_parser

let directory_list dirname =
  List.map (fun x -> dirname ^ x) (Array.to_list (Sys.readdir dirname))

let rec allf_base f dirname =
  let rec inner d r =
    match d with
      []	-> r
    | x::xs	->
      match (Sys.is_directory x) with
	true	-> inner xs (r @ (allf_base f (x ^ "/")))
      | false	-> if (f x) then inner xs (x::r) else inner xs r
  in
  inner (directory_list dirname) []

let allf dirname =
  allf_base (fun x -> true) dirname

let allf_type suffix dirname =
  allf_base (fun x -> Str.string_match (Str.regexp (".+\\." ^ suffix ^ "$")) x 0) dirname

let filter f xs =
  let rec inner subl r =
    match subl with
      []	-> List.rev r
    | y :: ys	->
      match (f y) with
	true  -> inner ys (y::r)
      | false -> inner ys r
  in
  inner xs [];;

let iota f t step =
  let rec inner c r =
    if c > t then List.rev r
    else inner (c + step) (c::r)
  in
  inner f []

let group list n =
  let rec inner subl c sr r =
    match subl with
      []      ->
	(match sr with
	  [] -> List.rev r
	| n' -> List.rev ((List.rev n')::r))
    | x :: xs -> if c = (n - 1) then inner xs 0 [] ((List.rev (x :: sr)) :: r)
      else inner xs (c + 1) (x::sr) r
  in
  inner list 0 [] [];;

let explode string =
  List.map (fun x -> string.[x - 1]) (iota 1 (String.length string) 1);;

let implode list =
  let rec inner subl r =
    match subl with
      []	-> r
    | x :: xs	-> inner xs (r ^ (String.make 1 x))
  in
  inner list ""

(* ”Žš‚¾‚¯ *)
let to_hankaku str =
  let pot = List.map (fun x -> Char.code x) (explode str) in
  let rec inner l r =
    match l with
      []           -> implode (List.rev (List.map Char.chr r))
    | x :: y :: xs -> if x = 130 && y >= 79 && y <= 88 then inner xs ((y-31)::r)
      else inner xs (y::x::r)
  in
  inner pot [];;



(* List.iter *)
(*   (fun x -> *)
(*     try let gen = int_of_string (List.nth x 3) in *)
(* 	match (gen mod 10) with *)
(* 	  1  -> print_endline "–{l" *)
(* 	| n' -> print_endline "‰Æ‘°" *)
(*     with *)
(*     Failure e -> print_endline "Nothing." *)
(*   ) *)
(*   (csv_reader "f:/FKCA172.csv");; *)
