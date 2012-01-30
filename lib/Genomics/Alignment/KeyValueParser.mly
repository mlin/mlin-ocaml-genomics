%{
%}
%token <string> WORD
%token <string> LITERAL
%token EQ SP EOF
%start parse
%type <(string*string) list> parse
%%
parse:
	| EOF								{ [] }
	| key_value EOF						{ [$1] }
	| parse key_value EOF				{ List.rev ($2 :: $1) }
	
	| parse key_value					{ $2 :: $1 }
	| key_value							{ [$1] }
;
key_value:
	| WORD EQ WORD						{ ($1,$3) }
	| WORD EQ LITERAL					{ ($1,$3) }
	| LITERAL EQ WORD					{ ($1,$3) }
	| LITERAL EQ LITERAL				{ ($1,$3) }
%%
