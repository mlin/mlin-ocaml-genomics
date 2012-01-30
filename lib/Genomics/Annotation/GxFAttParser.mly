%{
%}
%token <string> STR
%token EQ SEMI EOF
%start parse
%type <[`id of string | `keys_values of (string*string) list]> parse
%%
parse:
	| STR EOF				{ (`id $1) }
	| keys_values 			{ (`keys_values (List.rev $1)) }
;
keys_values:
	| key_value				{ [$1] }
	| keys_values key_value	{ $2 :: $1 }
;
key_value:
	| STR STR SEMI		{ ($1,$2) }
	| STR STR EOF		{ ($1,$2) }
%%
