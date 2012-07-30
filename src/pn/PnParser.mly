%{
	open PnAst
	
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
%token <int> NUMBER
%token ARROW PLUS MINUS GE EQ PRIME
%token VARS RULES INIT TARGET INVARIANTS
%token <Lexing.position> COMMA SEMI

%start program_top
%type <PnAst.Program.t> program_top

%%

program_top:
	program EOF { $1 }
;

program:
	VARS var_decls
	RULES rule_decls
	INIT init_decl
	TARGET target_decl
	invariant_decls_opt
	{ $2, $4, $6, $8 }
;

invariant_decls_opt:
	{ [] }
	| INVARIANTS invariant_decls { $2 }
;

var_decls:
	{ [] }
	| identifier var_decls { $1 :: $2 }
;

rule_decls:
	{ [] }
	| rule_decl rule_decls { $1 :: $2 }
;

rule_decl:
	guards ARROW updates SEMI { $1, $3 }
;

init_decl:
	constraints { $1 }
;

target_decl:
	guards { $1 }
;

invariant_decls:
	constraints { $1 :: [] }
	| constraints invariant_decls { $1 :: $2 }
;

guards:
	guard { $1 :: [] }
	| guard COMMA guards { $1 :: $3 }
;

guard:
	identifier GE NUMBER { $1, $3 }
	| identifier EQ NUMBER { 
		failwith (Printf.sprintf
			"Found reachability query %s = %n; we only handle coverability." 
			$1 $3)
	}
;

constraints:
	constraint_ { $1 :: [] }
	| constraint_ COMMA constraints { $1 :: $3 }
;

constraint_:
	identifier reln NUMBER { $1, $2, $3 }
;

reln: 
	GE { Constraint.GE }
	| EQ { Constraint.EQ }
;

updates:
	update { $1 :: [] }
	| update COMMA updates { $1 :: $3 }
;

update:
	identifier EQ identifier sign NUMBER {
		assert ($1 = ($3 ^ "'"));	
		$3, $4, $5 
	}
;

sign:
	PLUS { Update.PLUS }
	| MINUS { Update.MINUS }
;

identifier:
	ID { fst $1 }
;
