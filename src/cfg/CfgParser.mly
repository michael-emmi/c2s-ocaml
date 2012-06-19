%{
	open Cfg
	
	let parse_error s =
		let start_pos = Parsing.symbol_start_pos ()
		and end_pos = Parsing.symbol_end_pos () in

		let sl, sc =
			start_pos.Lexing.pos_lnum,
			start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol
				
		and el, ec =
			end_pos.Lexing.pos_lnum,
		end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol in
		
		raise (ParsingUtils.Parse_error (sl,sc,el,ec,""))
%}

%token EOL EOF
%token <PnAst.Identifier.t * Lexing.position> ID
%token ARROW 
%token ALPHABET VARIABLES RULES START
%token <Lexing.position> COMMA SEMI

%start grammar_top
%type <Cfg.Grammar.t> grammar_top

%%

grammar_top:
	grammar EOF { $1 }
;

grammar:
	ALPHABET symbols
	VARIABLES symbols
	RULES rules
	START symbol
	{ $2, $4, $6, $8 }
;

symbols:
	symbol { $1 :: [] }
	| symbol symbols { $1 :: $2 }
;

rules:
	{ [] }
	| rule rules { $1 :: $2 }
;

rule:
	symbol ARROW symbols SEMI { $1, $3 }
;
	
symbol:
	ID { fst $1 }
;
