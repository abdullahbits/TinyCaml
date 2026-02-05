%{
open Ast

let rec apply loc e es =
	match es with
	| [] -> failwith "precondition violated"
  | [e'] -> ExprApp (loc, e, e')
	| h :: ((_::_) as t) -> apply loc (ExprApp (loc, e, h)) t
%}

%token <string> ID
%token <float> NUMBER
%token LPAREN RPAREN ARROW NOT AND OR LT GT EQ NOT_EQ LTE GTE PLUS TIMES EQUALS TRUE FALSE FUN LET IN IF THEN ELSE EOF

%nonassoc IN
%nonassoc ELSE

%right AND
%right OR

%left LT
%left GT
%left EQ
%left NOT_EQ
%left LTE
%left GTE
%left PLUS
%left TIMES

%start <Ast.expr> ast

%%

ast:
	| e = expr; EOF { e }
	;

expr:
	| basic_expr = basic_expr { basic_expr }
	| NOT; e = expr; { ExprUnaryOp ($startpos, Not, e) }
	| e1 = expr; AND; e2 = expr { ExprBinaryOp ($startpos, And, e1, e2) }
	| e1 = expr; OR; e2 = expr { ExprBinaryOp ($startpos, Or, e1, e2) }
	| e1 = expr; LT; e2 = expr { ExprBinaryOp ($startpos, Lt, e1, e2) }
	| e1 = expr; GT; e2 = expr { ExprBinaryOp ($startpos, Gt, e1, e2) }
	| e1 = expr; EQ; e2 = expr { ExprBinaryOp ($startpos, Eq, e1, e2) }
	| e1 = expr; NOT_EQ; e2 = expr { ExprBinaryOp ($startpos, NotEq, e1, e2) }
	| e1 = expr; LTE; e2 = expr { ExprBinaryOp ($startpos, Lte, e1, e2) }
	| e1 = expr; GTE; e2 = expr { ExprBinaryOp ($startpos, Gte, e1, e2) }
  | e1 = expr; TIMES; e2 = expr { ExprBinaryOp ($startpos, Mult, e1, e2) }
	| e1 = expr; PLUS; e2 = expr { ExprBinaryOp ($startpos, Add, e1, e2) }
	| FUN; param = ID; ARROW; e = expr; { ExprFun ($startpos, param, e) }
	| LET; id = ID; EQUALS; e1 = expr; IN; e2 = expr { ExprLet ($startpos, id, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { ExprIf ($startpos, e1, e2, e3) }
	| e = basic_expr; es = basic_expr+ { apply $startpos e es }
	;

basic_expr:
	| literal = literal { literal }
	| id = ID { ExprVar ($startpos, id) }
	| LPAREN; e = expr; RPAREN { e }
	;

literal:
	| n = NUMBER { ExprLiteral ($startpos, Number n) }
	| TRUE { ExprLiteral ($startpos, Boolean true) }
	| FALSE { ExprLiteral ($startpos, Boolean false) }
	;
