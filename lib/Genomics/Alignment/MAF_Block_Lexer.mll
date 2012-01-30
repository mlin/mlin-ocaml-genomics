{
open MAF_Defs
open MAF_Block_Parser
}

rule token = parse
	| '#' [^'\n']* '\n'							{ token lexbuf }
	| 'a' ([^'\n']* as lxm) '\n'				{ A(lxm) }
	| 's'
		[' ''\t']+ ([^' ''\t']+ as src)
		[' ''\t']+ (['0'-'9']+ as start) 
		[' ''\t']+ (['0'-'9']+ as size)
		[' ''\t']+ (('+'|'-') as strand)
		[' ''\t']+ (['0'-'9']+ as src_size)
		[' ''\t']+ ([^' ''\t']+ as text)
		'\n'									{ S {
													src = src;
													start = int_of_string start;
													size = int_of_string size;
													strand = (match strand with
																| '+' -> `plus
																| '-' -> `minus
																| _ -> assert false);
													src_size = int_of_string src_size;
													text = text
												 } }
	| 'i' [^'\n']* '\n'							{ token lexbuf }
	| 'e' [^'\n']* '\n'							{ token lexbuf }
	| 'q' [^'\n']* '\n'							{ token lexbuf }
	| '\n'										{ BLANK }
	| eof										{ EOF }
	
