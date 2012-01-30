{
open KeyValueParser
}

rule token = parse
	| [' ' '\t' '\n']+										{ token lexbuf }
	| '='													{ EQ }
	| [^ '\"' '\'' '=' ' ' '\t']+ as lxm					{ WORD(lxm) }
	| ('\"' ( [^ '\"' '\\'] | ( '\\' _ ) )* '\"') as lxm	{ LITERAL(lxm) }
	| ('\'' ( [^ '\'' '\\'] | ( '\\' _ ) )* '\'') as lxm	{ LITERAL(lxm) }
	| eof													{ EOF }
