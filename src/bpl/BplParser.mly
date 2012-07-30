%{
	(** Parser for Boogie programs. *)
	open Prelude
	open BplAst
	module P = Program
	module D = Declaration
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
%token <BplAst.Identifier.t * Lexing.position> ID
%token <Lexing.position> LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token <Lexing.position> L_ATTR_DELIM
%token <Lexing.position> LANGLE RANGLE
%token <Lexing.position> DOT COMMA SEMI COLON ASSIGN

%token <int> NUMBER
%token <int> BVLIT
%token <string> STRING_LIT

%token IMPLIES IFF OR AND
%token SEQ EQ NEQ LT GT LTE GTE SUB
%token CONCAT PLUS MINUS AST DIV MOD
%token NOT UNDER
%token QSEP

%token BOOL INT
%token <int> BV
%token TRUE FALSE 
%token OLD
%token FORALL EXISTS
%token REQUIRES MODIFIES ENSURES POSTS INVARIANT
%token FREE UNIQUE FINITE COMPLETE
%token TYPE CONST FUNCTION AXIOM VAR PROCEDURE IMPLEMENTATION
%token WHERE RETURNS

%token ASSERT ASSUME HAVOC CALL WHILE BREAK RETURN GOTO IF ELSE
%token INVARIANT
  
%start program_top declarations_top labeled_statements_top expression_top
%type <BplAst.Program.t> program_top
%type <BplAst.Declaration.t list> declarations_top
%type <BplAst.LabeledStatement.t list> labeled_statements_top
%type <BplAst.Expression.t> expression_top

%nonassoc LPAREN RPAREN
%left COLON
%left IF THEN ELSE
%left IFF
%left IMPLIES
%left OR
%left AND
%left EQ NEQ LT GT LTE GTE SUB
%left CONCAT
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

declarations_top:
	declarations EOF { $1 }
;

labeled_statements_top:
	labeled_statements EOF { $1 }
;

expression_top:
	expression EOF { $1 }
;

program:
	declarations_opt EOF { $1 }
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
  | TYPE attributes_opt identifier identifiers_opt SEMI
		  { D.TypeCtor ($2,false,$3,$4) :: [] }
  | TYPE attributes_opt FINITE identifier identifiers_opt SEMI
		  { D.TypeCtor ($2,true,$4,$5) :: [] }
  | TYPE attributes_opt identifier identifiers_opt SEQ type_ SEMI
		  { D.TypeSyn ($2,$3,$4,$6) :: [] }
  |	CONST attributes_opt unique_opt typed_identifiers_seq order_spec SEMI
	{ List.map (fun (c,t) -> D.Const ($2,$3,c,t,$5)) $4 }

  | FUNCTION attributes_opt identifier function_signature SEMI {
		let ts, ps, rs = $4 in 
		D.Func ($2,$3,ts,ps,rs,None) :: []
	}
  | FUNCTION attributes_opt identifier function_signature
		  LBRACE expression RBRACE {
			  let ts, ps, rs = $4 in
			  D.Func ($2,$3,ts,ps,rs,Some $6) :: []
		  }
  | AXIOM attributes_opt expression SEMI { D.Axiom ($2,$3) :: [] }

  | VAR attributes_opt constrained_typed_identifiers_seq SEMI {
		List.map (fun (x,t) -> D.Var ($2,x,t,None)) $3
	}
  | PROCEDURE attributes_opt identifier
		  procedure_signature SEMI
		  procedure_specs_opt {
			  let ts, ps, rs = $4 in
			  D.Proc ($2,$3,(ts,ps,rs,$6,[],[])) :: []
		  } 
	/* | PROCEDURE identifier procedure_signature
		  LBRACE RBRACE {
			  D.Proc ([],$2,[],[],[],[],[],[]) :: []
		  } */
  | PROCEDURE attributes_opt identifier
		  procedure_signature
		  procedure_specs_opt
		  LBRACE var_decls_opt labeled_statements_opt RBRACE {
			  let ts, ps, rs = $4 in
			  D.Proc ($2,$3,(ts,ps,rs,$5,$7,$8)) :: []
		  }
  | IMPLEMENTATION attributes_opt identifier
		  procedure_signature
		  LBRACE var_decls_opt labeled_statements_opt RBRACE {
			  let ts, ps, rs = $4 in
			  D.Impl ($2,$3,(ts,ps,rs,[],$6,$7)) :: []
		  } 
;

function_signature:
	type_args_opt LPAREN function_args RPAREN
	RETURNS LPAREN function_arg RPAREN
	{ $1, $3, $7 }
;

procedure_signature:
	type_args_opt LPAREN constrained_typed_identifiers_opt RPAREN
	out_parameters_opt { $1, $3, $5 }
;

type_args_opt:
	{ [] }
  | type_args { $1 }
;

type_args:
	LT identifiers GT { $2 }
;
	

out_parameters_opt:
	{ [] }
  | out_parameters { $1 }
;

out_parameters:
	RETURNS LPAREN constrained_typed_identifiers_opt RPAREN { $3 }
;

procedure_specs_opt:
	{ [] }
  | procedure_specs { $1 }
;

procedure_specs:
	procedure_spec SEMI { $1 :: [] }
  | procedure_spec SEMI procedure_specs { $1 :: $3 }
;

procedure_spec:
	free_opt REQUIRES expression { Specification.Requires ($1,$3) }
  | free_opt MODIFIES identifiers { Specification.Modifies ($1,$3) }
  | free_opt ENSURES expression { Specification.Ensures ($1,$3) }
  | free_opt POSTS identifiers { Specification.Posts ($1,$3)}
; 

free_opt: { false } | FREE { true } ;
unique_opt: { false } | UNIQUE { true } ;

var_decls_opt:
	{ [] }
  | var_decls { $1 }
;

var_decls:
	var_decl { $1 }
  | var_decl var_decls { $1 @ $2 }
;

var_decl:
	VAR attributes_opt constrained_typed_identifiers_seq SEMI
	{ List.map (fun (x,t) -> D.Var ($2,x,t,None)) $3 }
;

/* ToDo: IdsTypeWhere */
constrained_typed_identifiers_opt:
	{ [] }
  | constrained_typed_identifiers_seq { $1 }
;

constrained_typed_identifiers_seq:
  | constrained_typed_identifiers { $1 }
  | constrained_typed_identifiers COMMA constrained_typed_identifiers_seq { $1 @ $3 }
;

typed_identifiers_seq:
  | typed_identifiers { $1 }
  | typed_identifiers COMMA typed_identifiers_seq { $1 @ $3 }
;
	
constrained_typed_identifiers:
  | identifiers COLON type_ WHERE expression { List.map (fun x -> (x,$3)) $1 }
  | identifiers COLON type_ { List.map (fun x -> (x,$3)) $1 }
;

typed_identifiers:
	identifiers COLON type_ { List.map (fun x -> (x,$3)) $1 }
;

function_args:
	function_arg { $1 :: [] }
  | function_arg COMMA function_args { $1 :: $3 }
;

function_arg:
	type_ { None, $1 }
  | identifier COLON type_ { Some $1, $3 }
;



/*typed_identifier:
	identifier COLON type_ { $1, $3 }
;
*/
/* ToDo: OrderSpec */
order_spec:
	{ () }
;

attributes_opt:
	{ [] }
  | attributes { $1 }
;

attributes:
	attribute { $1 :: [] }
  | attribute attributes { $1 :: $2 }
;

attribute:
	L_ATTR_DELIM identifier attr_args_opt RBRACE { $2, $3 }
;

attr_args_opt:
	{ [] }
  | attr_args { $1 }
;

attr_args:
  attr_arg { $1 :: []}
  | attr_arg COMMA attr_args { $1 :: $3 }
;

attr_arg:
  expression { Left $1 }
  | STRING_LIT { Right $1 }
;  

triggers_and_attributes_opt:
	{ [] }
;

/* ToDo: Where */
/* where_opt:
	{ None }
; */

identifiers_opt:
	{ [] }
  | identifiers { $1 }
;

identifiers:
    identifier { $1 :: [] }
  | identifier COMMA identifiers { $1 :: $3 }
;

identifier_opt:
	{ None }
  | identifier { Some $1 }
;

identifier:
	ID { fst $1 }
;

types:
	type_ { $1 :: [] }
  | type_ COMMA types { $1 :: $3 }
;

type_:
	atom_type { $1 }
  | type_args_opt LBRACKET types RBRACKET type_ { Type.Map ($1,$3,$5) }
;

atom_type:
	BOOL { Type.Bool }
  | INT { Type.Int }
  | BV { Type.Bv $1 }
  | identifier { Type.T ($1,[]) }
;

labeled_statements_opt:
	{ [] }
  | labeled_statements { $1 }
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
	ASSERT attributes_opt expression SEMI { S.Assert ($2,$3) }
  | ASSUME attributes_opt expression SEMI { S.Assume ($2,$3) }
  | HAVOC identifiers SEMI { S.Havoc $2 }
  | lvalues ASSIGN expressions SEMI { S.Assign ($1,$3) }
  | CALL attributes_opt identifier LPAREN expressions_opt RPAREN SEMI
		  { S.Call ($2,$3,$5,[]) }
  | CALL attributes_opt identifiers ASSIGN identifier LPAREN expressions_opt RPAREN SEMI
		  { S.Call ($2,$5,$7,$3) } 
	  /* | CALL FORALL identifier LPAREN expressions RPAREN SEMI
			  { S.Call ($3,$5,[]) }  */
  | if_statement { $1 }			  
  | WHILE LPAREN expression_or_wildcard RPAREN invariants_opt block_statement
		  { S.While ($3,$5,$6) }
  | BREAK identifier_opt SEMI { S.Break $2 }
  | RETURN SEMI { S.Return }
  | GOTO identifiers SEMI { S.Goto $2 }
;

lvalues:
	lvalue { $1 :: [] }
  | lvalue COMMA lvalues { $1 :: $3 }
;

lvalue:
	identifier { Lv.Id $1 }
  | lvalue LBRACKET expressions RBRACKET { Lv.Sel ($1,$3) }
;

if_statement:
	IF LPAREN expression_or_wildcard RPAREN
	block_statement else_statement_option
	{ S.If ($3,$5,$6) }
;

else_statement_option:
	{ [] }
  | else_statement { $1 }
;

else_statement:
	ELSE block_statement { $2 }
  | ELSE if_statement { ([],$2) :: [] }
;

invariants_opt:
	{ [] }
  | invariants { $1 }
;

invariants:
	invariant SEMI { $1 :: [] }
  | invariant SEMI invariants { $1 :: $3 }
;

invariant:
	free_opt INVARIANT expression { $3,$1 }
;

block_statement:
	LBRACE labeled_statements_opt RBRACE { $2 }
;
	
expression_or_wildcard:
	AST { None }
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
  | expression SUB expression { E.Bin (BinaryOp.Sub,$1,$3) }

  | expression CONCAT expression { E.Bin (BinaryOp.Conc,$1,$3) }
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

  | literal { E.Lit $1 }
  | identifier { E.Id $1 }
  | identifier LPAREN expressions_opt RPAREN { E.FnApp ($1,$3) }
	
  | OLD LPAREN expression RPAREN { E.Old $3 }

  | LPAREN quantifier type_args_opt typed_identifiers_seq QSEP 
		  triggers_and_attributes_opt expression RPAREN
	  { let ax, ts = Either.separate $6 in
		E.Q ($2,$3,$4,ax,ts,$7) }

  | LPAREN expression RPAREN { $2 }
;

quantifier: 
	FORALL { E.Forall }
  | EXISTS { E.Exists }
;

literal:
	TRUE { Literal.True }
  | FALSE { Literal.False }
  | NUMBER { Literal.Num $1 }
  | BVLIT { Literal.Bv $1 }
;
	
