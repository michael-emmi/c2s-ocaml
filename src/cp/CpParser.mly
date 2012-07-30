%{
	(** Parser for concurrent programs. *)
	open Prelude
	open CpAst
	module P = Program
	module E = Expression
	module Lv = Lvalue
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
%token <CpAst.Identifier.t * Lexing.position> ID
%token <Lexing.position> LPAREN RPAREN LBRACKET RBRACKET
%token <Lexing.position> LBRACE RBRACE LANGLE RANGLE
%token <Lexing.position> DOT COMMA SEMI COLON ASSIGN DOTDOT

%token TYPE
%token VOID BOOL INT
%token TRUE FALSE
%token <int> NUM

%token IMPLIES IFF OR AND
%token EQ NEQ LT GT LTE GTE 
%token PLUS MINUS AST DIV MOD
%token NOT UNDER CHOICE
%token FORALL EXISTS QSEP
%token L_ATTR_DELIM R_ATTR_DELIM

%token IF THEN ELSE FI WHILE DO DONE
%token ASSUME ASSERT
%token GOTO RETURN SKIP
%token VAR CONST FUNC ENFORCE BEGIN END PROC REQUIRES ENSURES INVARIANT
%token SCHOOSE CONSTRAIN
%token DEAD
%token OLD

%token CALL
%token POST
%token NEW
%token YIELD
%token SELF
  
%start program_top declaration_top labeled_statements_top expression_top
%type <CpAst.Program.t> program_top
%type <CpAst.Declaration.t> declaration_top
%type <CpAst.LabeledStatement.t list> labeled_statements_top
%type <CpAst.Expression.t> expression_top

%nonassoc LPAREN RPAREN
%left COLON
%left IF THEN ELSE
%left IFF
%left IMPLIES
%left OR
%left AND
%left EQ NEQ LT GT LTE GTE SUB
%left PLUS MINUS
%left AST DIV MOD
%nonassoc NOT
%nonassoc LBRACKET RBRACKET
%left OLD
%left FORALL EXISTS
  
%%

program_top:
  program EOF { $1 }
;

declaration_top:
	global_declaration EOF { List.hd $1 }
;

labeled_statements_top:
	labeled_statements EOF { $1 }
;

expression_top:
	expression EOF { $1 }
;

program:
	global_declarations_opt EOF { $1 }
;
	
global_declarations_opt:
	{ [] }
  | global_declarations { $1 }
;

global_declarations:
	global_declaration { $1 }
  | global_declaration global_declarations { $1 @ $2 }
;

global_declaration:
  | TYPE attributes_opt identifiers
    { Declaration.Type ($2, List.hd $3, List.tl $3, None) :: [] }

  | TYPE attributes_opt identifiers EQ type_
    { Declaration.Type ($2, List.hd $3, List.tl $3, Some $5) :: [] }

  | VAR attributes_opt identifiers COLON type_
	{ List.map (fun x -> Declaration.Var ($2,x,$5)) $3 }

  | CONST attributes_opt identifiers COLON type_ initial_value_opt
	{ List.map (fun x -> Declaration.Const ($2,x,$5,$6)) $3 }

  | FUNC attributes_opt identifier function_declaration
	  { Declaration.Func ($2,$3,$4) :: [] }

  | PROC attributes_opt identifier procedure_declaration 
	  { Declaration.Proc ($2,$3,$4) :: [] }

  | INVARIANT expression { Declaration.Inv $2 :: [] }
;

initial_value_opt:
	{ None }
  | EQ expression { Some $2 }

attributes_opt:
	{ [] }
  | attributes { $1 }
;

attributes:
	attribute { $1 :: [] }
  | attribute attributes { $1 :: $2 }
;

attribute:
	L_ATTR_DELIM identifier identifiers_opt R_ATTR_DELIM { $2, $3 }
;
	
function_declaration:
  | LPAREN possibly_named_parameters RPAREN COLON type_ { $2, $5, None }
  | LPAREN possibly_named_parameters RPAREN COLON type_	EQ expression { $2, $5, Some $7 }
;

procedure_declaration:
	LPAREN parameters_opt RPAREN COLON
	return_parameters
	requires_opt
	ensures_opt
	BEGIN
	local_declarations_opt
	labeled_statements_opt
	END
	{ $2, $5, $6, $7, $9, $10 }
;

return_parameters:
	VOID { [] }
  | BOOL { [None, Type.Bool] }
  | INT { [None, Type.Int] }
  | identifier { [None, Type.T $1] }
  | LBRACKET types RBRACKET type_ { [None, Type.Map ($2,$4)] }
  | LPAREN possibly_named_parameters RPAREN { $2 }
;

possibly_named_parameters:
	possibly_named_parameter { $1 :: [] }
  | possibly_named_parameter COMMA possibly_named_parameters { $1 :: $3 }
;
	
possibly_named_parameter:
	type_ { None, $1 }
  | identifier COLON type_ { Some $1, $3 }
;

parameters_opt:
	{ [] }
  | parameters { $1 }
;

parameters:
	parameter { $1 }
  | parameter COMMA parameters { $1 @ $3 }
;

parameter:
    VAR identifiers COLON type_
	{ List.map (fun x -> Declaration.Var ([],x,$4)) $2 }

  | CONST identifiers COLON type_
	{ List.map (fun x -> Declaration.Const ([],x,$4,None)) $2 }
;

local_declarations_opt:
	{ [] }
  | local_declarations { $1 }
;

local_declarations:
	local_declaration { $1 }
  | local_declaration local_declarations { $1 @ $2 }
;

local_declaration:
    VAR identifiers COLON type_
	{ List.map (fun x -> Declaration.Var ([],x,$4)) $2 }

  | CONST identifiers COLON type_
	{ List.map (fun x -> Declaration.Const ([],x,$4,None)) $2 }
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

typed_identifiers:
	typed_identifier { $1 :: [] }
  | typed_identifier COMMA typed_identifiers { $1 :: $3 }
;

typed_identifier:
	identifier COLON type_ { $1,$3 }
;

requires_opt:
	{ [] }
  | requiress { $1 }
;

requiress:
	requires { $1 :: [] }
  | requires requiress { $1 :: $2 }
;

requires:
	REQUIRES expression { $2 }
;

ensures_opt:
	{ [] }
  | ensuress { $1 }
;

ensuress:
	ensures { $1 :: [] }
  | ensures ensuress { $1 :: $2 }
;

ensures:
	ENSURES expression { $2 }
;

types:
	type_ { $1 :: [] }
  | type_ COMMA types { $1 :: $3 }
;

type_:
/* VOID { Type.Void } -- only in return types.. */  
  | BOOL { Type.Bool }
  | INT { Type.Int }
  | INT LPAREN NUM DOTDOT NUM RPAREN { Type.Intv ($3,$5) }
  | identifier { Type.T $1 }
  | LPAREN types RPAREN { Type.Tuple $2 }
  | LBRACKET types RBRACKET type_ { Type.Map ($2,$4) }
;

/*enforce_opt:
	{ None }
  | enforce { Some $1 }
;

enforce:
	ENFORCE expression SEMI { $2 }
;
*/

labeled_statements_opt:
	{ [] }
  | labeled_statements { $1 }
;

labeled_statements:
	labeled_statement semi_opt { $1 :: [] }
  | labeled_statement SEMI labeled_statements { $1 :: $3 }
;

semi_opt:
	{ () }
  | SEMI { () }
;

labeled_statement:
    statement { LabeledStatement.S ([],$1) }
  | identifier COLON labeled_statement
		  { LabeledStatement.cons_label $1 $3 }
;

lvalues:
	lvalue { $1 :: [] }
  | lvalue COMMA lvalues { $1 :: $3 }
;

lvalue:
    identifier { Lv.Id $1 }
  | lvalue LBRACKET expressions RBRACKET { Lv.Sel ($1,$3) }
;

statement:
	SKIP { S.Skip }
  | GOTO identifiers { S.Goto $2 }
  | RETURN expressions_opt { S.Return $2 }
  | lvalues ASSIGN expressions constrain_opt { S.Assign ($1,$3,$4) }
  | IF expression THEN labeled_statements ELSE
		  labeled_statements FI { S.Ite ($2,$4,$6) }
  | WHILE expression invariants_opt
		  DO labeled_statements DONE { S.While ($2,$3,$5) }
  | ASSERT expression { S.Assert $2 }
  | ASSUME expression { S.Assume $2 }

  | CALL identifier LPAREN expressions_opt RPAREN { S.Call ($2,$4,[]) }
  | CALL lvalues ASSIGN identifier LPAREN expressions_opt RPAREN 
		  { S.Call ($4,$6,$2) }

  | DEAD identifiers { S.Dead $2 }

  | NEW expression { S.New $2 }
  | POST identifier LPAREN expressions_opt RPAREN expression_opt
		  { S.Post ($2,$4,$6) }

  | YIELD { S.Yield }
;

invariants_opt:
	{ [] }
  | invariants { $1 }
;

invariants:
	invariant { $1 :: [] }
  | invariant invariants { $1 :: $2 }
;

invariant:
	INVARIANT expression { $2 }
;

constrain_opt:
	{ None }
  | constrain { Some $1 }
;

constrain:
	CONSTRAIN expression { $2 }
;

expressions_opt:
	{ [] }
  | expressions { $1 }
;

expressions:
	expression { $1 :: [] }
  | expression COMMA expressions { $1 :: $3 }
;

expression_opt:
	{ None }
  | expression { Some $1 }
;

expression:
  | expression IFF expression { E.Bin (BinaryOp.Iff,$1,$3) }
  | expression IMPLIES expression { E.Bin (BinaryOp.Imp,$1,$3) }
  | expression OR expression { E.Bin (BinaryOp.Or,$1,$3) }
  | expression AND expression { E.Bin (BinaryOp.And,$1,$3) }

  | expression EQ expression { E.Bin (BinaryOp.Eq,$1,$3) }
  | expression NEQ expression { E.Bin (BinaryOp.Neq,$1,$3) }
  | expression LT expression { E.Bin (BinaryOp.Lt,$1,$3) }
  | expression GT expression { E.Bin (BinaryOp.Gt,$1,$3) }
  | expression LTE expression { E.Bin (BinaryOp.Lte,$1,$3) }
  | expression GTE expression { E.Bin (BinaryOp.Gte,$1,$3) }

  | expression PLUS expression { E.Bin (BinaryOp.Plus,$1,$3) }
  | expression MINUS expression { E.Bin (BinaryOp.Minus,$1,$3) }
  | expression AST expression { E.Bin (BinaryOp.Times,$1,$3) }
  | expression DIV expression { E.Bin (BinaryOp.Div,$1,$3) }
  | expression MOD expression { E.Bin (BinaryOp.Mod,$1,$3) }

  | NOT expression { E.Not $2 }
  | MINUS expression { E.Neg $2 }

  | expression LBRACKET expressions RBRACKET { E.Sel ($1,$3) }
  | expression LBRACKET expressions ASSIGN expression RBRACKET
		  { E.Upd ($1,$3,$5) }

  | OLD LPAREN expression RPAREN { E.Old $3 }
  | identifier LPAREN expressions RPAREN { E.FnApp ($1,$3) }

  | LPAREN quantifier typed_identifiers QSEP expression RPAREN
		  { E.Q ($2,$3,$5) }

  | SELF { E.Self }
  | literal { E.Lit $1 }
  | identifier { E.Id $1 }
  | SCHOOSE LBRACKET expressions RBRACKET { E.Schoose $3 }
  | AST { E.Choice }
  | LPAREN expressions RPAREN {
		match $2 with
		| [] -> assert false
		| e :: [] -> e
		| es -> E.Tuple es
	}

;

quantifier:
	FORALL { E.Forall }
  | EXISTS { E.Exists }
;

literal:
	TRUE { Literal.Bool true }
  | FALSE { Literal.Bool false }
  | NUM { Literal.Num $1 }
;
	
