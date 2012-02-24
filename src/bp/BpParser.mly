%{
	(** Parser for Boolean programs. *)
	open Prelude
	open BpAst
	module P = Program
	module E = Expression
	module S = Statement

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
%token <BpAst.Identifier.t * Lexing.position> ID
%token <Lexing.position> LPAREN RPAREN LBRACKET RBRACKET
%token <Lexing.position> LBRACE RBRACE LANGLE RANGLE
%token <Lexing.position> DOT COMMA SEMI COLON ASSIGN

%token <int> INT

%token VOID BOOL TRUE FALSE
%token NOT AND OR EQ NEQ IMPLIES 
%token IF THEN ELSE FI WHILE DO OD
%token ASSUME ASSERT
%token GOTO RETURN SKIP
%token EQ NEQ CHOICE
%token DECL ENFORCE BEGIN END PRINT
%token SCHOOSE CONSTRAIN
%token DEAD
  
%start program_top
%type <BpAst.Program.t> program_top

%nonassoc LPAREN RPAREN
%left COLON
%left IF THEN ELSE
%left IMPLIES
%left OR
%left AND
%nonassoc NOT
%left EQ NEQ
  
%%

program_top:
  program EOF { $1 }
;

program:
	declarations_opt procedures_opt EOF { ($1,$2) }
;
	
declarations_opt:
	{ [] }
  | declarations { $1 }
;

declarations:
	declaration { $1 }
  | declaration declarations { $1 @ $2 } 
;

declaration:
	DECL identifiers SEMI { $2 }
;

identifiers_opt:
	{ [] }
  | identifiers { $1 }
;

identifiers:
    identifier { $1 :: [] }
  | identifier COMMA identifiers { $1 :: $3 }
;

identifier:
	ID { fst $1 }
;

procedures_opt: 
	{ [] }
  | procedures { $1 }
;

procedures:
	procedure { $1 :: [] }
  | procedure procedures { $1 :: $2 }
;

procedure:
	type_ identifier LPAREN identifiers_opt RPAREN BEGIN
	declarations_opt
	enforce_opt
	labeled_statements
	END { ($1,$2,$4,$7,$8,$9) }
;

type_:
	VOID { Type.Void }
  | BOOL { Type.Bool 1 }
  | BOOL LANGLE INT RANGLE { Type.Bool $3 }
;

enforce_opt:
	{ None }
  | enforce { Some $1 }
;

enforce:
	ENFORCE expression SEMI { $2 }
;


labeled_statements:
	labeled_statement { $1 :: [] }
  | labeled_statement labeled_statements { $1 :: $2 }
;

labeled_statement:
    statement { ([],$1) }
  | identifier COLON labeled_statement { ($1::fst $3, snd $3) }
;

statement:
	SKIP SEMI { S.Skip }
  | GOTO identifiers SEMI { S.Goto $2 }
  | RETURN expressions_opt SEMI { S.Return $2 }
  | identifiers ASSIGN expressions constrain_opt SEMI { S.Assign ($1,$3,$4) }
  | IF LPAREN decider RPAREN THEN labeled_statements ELSE
		  labeled_statements FI { S.Ite ($3,$6,$8) }
  | WHILE LPAREN decider RPAREN DO labeled_statements OD { S.While ($3,$6) }
  | ASSERT LPAREN decider RPAREN SEMI { S.Assert $3 }
  | ASSUME LPAREN expression RPAREN SEMI { S.Assume $3 }
  | identifier LPAREN expressions_opt RPAREN SEMI { S.Call ($1,$3,[]) } 
  | identifiers ASSIGN identifier LPAREN expressions_opt RPAREN SEMI
		  { S.Call ($3,$5,$1) } 
  | DEAD identifiers SEMI { S.Dead $2 }
;

constrain_opt:
	{ None }
  | constrain { Some $1 }
;

constrain:
	CONSTRAIN expression { $2 }
;

decider:
	CHOICE { None }
  | expression { Some $1 }
;
	
expressions_opt:
	{ [] }
  | expressions { $1 }
;

expressions:
	expression { $1 :: [] }
  | expression COMMA expressions { $1 :: $3 }
;

expression:
	LPAREN expression RPAREN { $2 }
  | identifier { E.Id $1 }
  | constant { E.Const $1 }
  | expression AND expression { E.Bin (BinaryOp.And,$1,$3) }
  | expression OR expression { E.Bin (BinaryOp.Or,$1,$3) }
  | expression IMPLIES expression { E.Bin (BinaryOp.Imp,$1,$3) }
  | expression EQ expression { E.Bin (BinaryOp.Eq,$1,$3) }
  | expression NEQ expression { E.Bin (BinaryOp.Neq,$1,$3) }
  | NOT expression { E.Not $2 }
  | SCHOOSE LBRACKET expressions RBRACKET { E.Schoose $3 }
;

constant:
	TRUE { Constant.True }
  | FALSE { Constant.False }
;
	
