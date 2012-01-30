open Batteries_uni
open Printf

type t = string*string*int*int

let asmbl (x,_,_,_) = x
let seq (_,x,_,_) = x
let asmbl_seq ?(require=false) x = match asmbl x with
	| "" when require -> invalid_arg "Position: no assembly name specified"
	| "" -> seq x
	| _ -> asmbl x ^ "." ^ seq x
let lo (_,_,x,_) = x
let hi (_,_,_,x) = x

let length (_,_,lo,hi) = hi-lo+1

let re_asmbl_seq = Str.regexp "\\(\\([\\_A-Za-z0-9]+\\)\\.\\)?\\([\\_A-Za-z0-9]+\\)"
let split_asmbl_seq asmbl_seq = 
	if not (Str.string_match re_asmbl_seq asmbl_seq 0) then invalid_arg "Position.split_asmbl_seq"
	let asmbl = try Str.matched_group 2 asmbl_seq with Not_found -> ""
	let seq = Str.matched_group 3 asmbl_seq
	asmbl, seq

let rm_commas = String.replace_chars (function ',' -> "" | ch -> String.make 1 ch)
let re_pos = Str.regexp "\\(\\([\\_A-Za-z0-9]+\\)\\.\\)?\\([\\_A-Za-z0-9]+\\):\\([0-9,]+\\)-\\([0-9,]+\\)"
let is s = Str.string_match re_pos s 0
let of_string s =
	if is s then
		let asmbl = try Str.matched_group 2 s with Not_found -> ""
		let seq = Str.matched_group 3 s
		let lo = int_of_string (rm_commas (Str.matched_group 4 s))
		let hi = int_of_string (rm_commas (Str.matched_group 5 s))
		if lo <= 0 || hi <= 0 || lo > hi then
			invalid_arg (sprintf "Position.of_string(\"%s\") invalid coordinates" s)
		(asmbl,seq,lo,hi)
	else
		invalid_arg (sprintf "Position.of_string(\"%s\")" s)
		
let add_commas i =
	let rec f i =
		let j = i mod 1000
		if i >= 1000 then
			(sprintf "%03d" j) :: (f (i / 1000))
		else
			[string_of_int j]
	f i |> List.rev |> String.join ","
	
let to_string ?(asmbl=true) ?(commas=true) (assembly,seq,lo,hi) =
	sprintf "%s:%s-%s"
		if asmbl && assembly <> "" then assembly ^ "." ^ seq else seq
		if commas then add_commas lo else string_of_int lo
		if commas then add_commas hi else string_of_int hi

