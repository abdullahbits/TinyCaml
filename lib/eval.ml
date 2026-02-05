open Core
open Ast
open Errors
module Environment = Map.Make_tree (String)

let empty_env = Environment.empty

type env = value Environment.t
and value = VBool of bool | VNumber of float | VClosure of string * expr * env

let string_of_value value =
  match value with
  | VBool b -> Fmt.str "%b" b
  | VNumber i -> Fmt.str "%f" i
  | VClosure (param, _, _) -> Fmt.str "fun %s" param

let rec eval (env : env) (expr : expr) : value =
  match expr with
  | ExprLiteral (_, literal) -> (
      match literal with Number f -> VNumber f | Boolean b -> VBool b)
  | ExprVar (loc, id) -> eval_var env loc id
  | ExprBinaryOp (loc, op, expr1, expr2) ->
      eval_binary_op env loc op expr1 expr2
  | ExprUnaryOp (loc, op, expr) -> eval_unary_op env loc op expr
  | ExprLet (_, id, expr1, expr2) -> eval_let env id expr1 expr2
  | ExprIf (loc, guard, expr1, expr2) -> eval_if env loc guard expr1 expr2
  | ExprFun (_, param, e) -> VClosure (param, e, env)
  | ExprApp (loc, e1, e2) -> eval_app env loc e1 e2

and eval_var (env : env) (loc : loc) (id : id) : value =
  try Environment.find_exn env id
  with Not_found_s _ -> raise_unbound_variable_error loc id

and eval_binary_op (env : env) (loc : loc) (op : binary_op) (expr1 : expr)
    (expr2 : expr) : value =
  match (op, eval env expr1, eval env expr2) with
  | Add, VNumber a, VNumber b -> VNumber (a +. b)
  | Mult, VNumber a, VNumber b -> VNumber (a *. b)
  | And, VBool a, VBool b -> VBool (a && b)
  | Or, VBool a, VBool b -> VBool (a || b)
  | Lt, VNumber a, VNumber b -> VBool (Core.Float.( < ) a b)
  | Gt, VNumber a, VNumber b -> VBool (Core.Float.( > ) a b)
  | Eq, VNumber a, VNumber b -> VBool (Core.Float.( = ) a b)
  | NotEq, VNumber a, VNumber b -> VBool (Core.Float.( <> ) a b)
  | Lte, VNumber a, VNumber b -> VBool (Core.Float.( <= ) a b)
  | Gte, VNumber a, VNumber b -> VBool (Core.Float.( >= ) a b)
  | _ -> raise_type_error loc

and eval_unary_op (env : env) (loc : loc) (op : unary_op) (expr : expr) : value
    =
  match (op, eval env expr) with
  | Not, VBool b -> VBool (not b)
  | _ -> raise_type_error loc

and eval_let (env : env) (id : id) (expr1 : expr) (expr2 : expr) : value =
  let v = eval env expr1 in
  let env' = Environment.set ~key:id ~data:v env in
  eval env' expr2

and eval_if (env : env) (loc : loc) (guard : expr) (expr1 : expr) (expr2 : expr)
    : value =
  match eval env guard with
  | VBool true -> eval env expr1
  | VBool false -> eval env expr2
  | _ -> raise_type_error loc

and eval_app (env : env) (loc : loc) (expr1 : expr) (expr2 : expr) : value =
  match eval env expr1 with
  | VClosure (param, e, defenv) ->
      let v2 = eval env expr2 in
      let env_for_body = Environment.set ~key:param ~data:v2 defenv in
      eval env_for_body e
  | _ -> raise_type_error loc
