%{
%}
%token <string> A
%token <MAF_Defs.sequence> S
%token BLANK EOF
%start parse parse_minimally
%type <MAF_Defs.block> parse
%type <MAF_Defs.block> parse_minimally
%%
parse:
	| block BLANK			{ $1 }
	| block EOF				{ $1 }
	| BLANK parse			{ $2 }
;
block:
	| A sequences			{ { MAF_Defs.attributes = KeyValueParser.parse KeyValueLexer.token
														(Lexing.from_string $1);
								sequences = Array.of_list (List.rev $2);
								unparsed = "" } }
;
sequences:
	| S						{ [$1] }
	| sequences S			{ $2 :: $1 }
;
parse_minimally:
	| A S					{ { MAF_Defs.attributes = [];
								sequences = [| $2 |];
								unparsed = "" } }
%%
