{
open GxFAttParser
}

rule token = parse
	| [' ' '\t' '=']+										{ token lexbuf }
	| ';'													{ SEMI }
	| [^ '\"' '\'' '=' ' ' '\t' ';']+ as lxm				{ STR(lxm) }
	| '\"' (( [^ '\"' '\\'] | ( '\\' _ ) )* as lxm) '\"' 	{ STR(lxm) }
	| '\'' (( [^ '\'' '\\'] | ( '\\' _ ) )* as lxm) '\''	{ STR(lxm) }
	| eof													{ EOF }
