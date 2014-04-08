let reader filename f =
  let fin = open_in filename in
  let rec inner r =
    try let c = (f fin) in
	inner (c::r)
    with
      End_of_file -> List.rev r
  in
  inner []

let make_string list = 
  let rec inner subl r =
    match subl with
      []    -> r
    | x::xs -> inner xs r^(String.make 1 x)
  in
  inner list ""


(* "f:/kcsv2/00263129_FKAC522_20130819_231.csv" *)
let csv_reader filename = 
  let fin = open_in filename in
  let rec inner(cell, line, r) =
    try let c = input_char(fin) in
	match c with
	  '\n' -> inner([], [], cell::line::r)
	| ','  -> inner([], (List.rev cell)::line, r)
	| n'   -> inner(n'::cell, line, r)
    with
      End_of_file -> cell::line::r
  in
  inner([], [], [])
