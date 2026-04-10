open Utils
module Error_msg = Error_msg

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Interp2.ty =
    | TUnit
    | TBool
    | TInt
    | TInt_list
    | TFun of ty * ty
    | TTuple of ty list

let rec pp_ty ppf ty =
  let open Fmt in
  let pp_parens ppf ty =
    match ty with
    | TFun (_, _)
    | TTuple _
    | _ -> pp_ty ppf ty
  in
  match ty with
  | TUnit -> pf ppf "unit"
  | TBool -> pf ppf "bool"
  | TInt -> pf ppf "int"
  | TFun (t1, t2) -> pf ppf "%a -> %a" pp_parens t1 pp_ty t2
  | TTuple ts -> list ~sep:(Fmt.any " * ") pp_ty ppf ts
  | TInt_list -> pf ppf "int list"

type _pattern = Ast.Interp2._pattern =
  | PUnit
  | PBool of bool
  | PInt of int
  | PNil
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PVar of string
and pattern = Ast.Interp2.pattern =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Interp2.bop =
  | Add | Sub | Mul | Div | Mod
  | Eq | Neq | Lt | Lte | Gt | Gte
  | And | Or | Cons

type _expr = Ast.Interp2._expr =
  | Unit
  | Bool of bool
  | Int of int
  | Var of string
  | Nil
  | Assert of expr
  | Negate of expr
  | Tuple of expr list
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Fun of (string * ty) list * expr
  | App of expr * expr list
  | Let of
      {
        is_rec : bool;
        name : string;
        args : (string * ty) list;
        annot : ty option;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Interp2.expr =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Interp2._stmt =
  | SLet of {
      is_rec : bool;
      name : string;
      args : (string * ty) list;
      annot : ty option;
      binding : expr;
    }
and stmt = Ast.Interp2.stmt =
  {
    pos : pos;
    stmt : _stmt;
  }

type prog = stmt list

module Env = Map.Make(String)

(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      pp_ty t1 pp_ty t2
  in Error_msg.mk pos msg

let exp_tuple_pat pos t =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of type %a"
      pp_ty t
  in Error_msg.mk pos msg

let exp_diff_tuple_pat pos ty =
  let msg =
    Format.asprintf
      "This pattern matches values of a tuple type but a pattern was expected which matches values of a different tuple type %a"
      pp_ty ty
  in Error_msg.mk pos msg

let not_func pos ty =
  let msg =
    Format.asprintf
      "This expression has type %a. This is not a function; it cannot be applied"
      pp_ty ty
  in Error_msg.mk pos msg

let too_many_args pos ty =
  let msg =
    Format.asprintf
      "This function has type %a. It is applied to to many arguments"
      pp_ty ty
  in Error_msg.mk pos msg

let missing_rec_annot pos =
  Error_msg.mk pos "Must provide output type annotation for recursive function"

let missing_rec_arg pos =
  Error_msg.mk pos "Must provide argument for recursive function"

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg


(* TYPING
   ----------------------------------------------------------------------
*)

(* Contexts *)

type ctxt = ty Env.t

(* Type Checking *)

let rec type_of_expr (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
  match e.expr with
  | Unit -> Ok TUnit
  | Bool _ -> Ok TBool
  | Int _ -> Ok TInt
  | Var x -> Ok (Env.find x ctxt)
  | Assert e1 ->
    (match type_of_expr ctxt e1 with
    | Ok TBool -> Ok TUnit
    | Ok _ -> assert false
    | Error _ -> assert false)
  | Negate e1 ->
    (match type_of_expr ctxt e1 with
    | Ok TInt -> Ok TInt
    | Ok _ -> assert false
    | Error _ -> assert false)
  | Bop (op, e1, e2) ->
    (match op, type_of_expr ctxt e1, type_of_expr ctxt e2 with
    | (Add | Sub | Mul | Div | Mod), Ok TInt, Ok TInt -> Ok TInt
    | (Eq | Neq | Lt | Lte | Gt | Gte), Ok t1, Ok t2 when t1 = t2 -> Ok TBool
    | (And | Or), Ok TBool, Ok TBool -> Ok TBool
    | _ -> assert false)
  | If (e1, e2, e3) ->
    (match type_of_expr ctxt e1, type_of_expr ctxt e2, type_of_expr ctxt e3 with
    | Ok TBool, Ok t2, Ok t3 when t2 = t3 -> Ok t2
    | _ -> assert false)
  | Let {is_rec = false; name; args = []; annot = _; binding; body} ->
    (match type_of_expr ctxt binding with
    | Ok t_binding ->
      let ctxt' = Env.add name t_binding ctxt in
        type_of_expr ctxt' body
    | Error _ -> assert false)
  | Let _ -> assert false
  | Nil -> assert false
  | Tuple _ -> assert false
  | Fun _ -> assert false
  | App _ -> assert false
  | Match _ -> assert false



let type_of (p : prog) : (ty, Error_msg.t) result =
  let rec go ctxt ty p =
    match p with
    | [] -> Ok (Option.value ~default:TUnit ty)
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps -> (
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      match type_of_expr ctxt e with
      | Ok ty ->
        let ctxt = Env.add name ty ctxt in
        go ctxt (Some ty) ps
      | Error err -> Error err
    )
  in go Env.empty None p


(* EVALUATION
   ----------------------------------------------------------------------
*)

(* Values *)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      args : string list;
      body : expr;
    }
  | VInt_list of int list

(* Dynamic Environments *)

type dyn_env = value Env.t

(* Evaluation *)

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos

let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e.expr with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int n -> VInt n
  | Var x -> Env.find x env
  | Assert e1 ->
    (match eval_expr env e1 with
    | VBool true -> VUnit
    | VBool false -> raise (Assert_fail e.pos)
    | _ -> assert false)
  | Negate e1 ->
    (match eval_expr env e1 with
    | VInt n -> VInt (-n)
    | _ -> assert false)
  | Bop (Add, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VInt (n1 + n2)
    | _ -> assert false)
  | Bop (Sub, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VInt (n1 - n2)
    | _ -> assert false)
  | Bop (Mul, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VInt (n1 * n2)
    | _ -> assert false)
  | Bop (Div, e1, e2) ->
    (match eval_expr env e2 with
    | VInt 0 -> raise (Div_by_zero e.pos)
    | VInt n2 ->
      (match eval_expr env e1 with
      | VInt n1 -> VInt (n1 / n2)
      | _ -> assert false)
    | _ -> assert false)
  | Bop (Mod, e1, e2) ->
    (match eval_expr env e2 with
    | VInt 0 -> raise (Div_by_zero e.pos)
    | VInt n2 ->
      (match eval_expr env e1 with
      | VInt n1 -> VInt (n1 mod n2)
      | _ -> assert false)
    | _ -> assert false)
  | Bop (Eq, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VBool (n1 = n2)
    | VBool b1, VBool b2 -> VBool (b1 = b2)
    | VUnit, VUnit -> VBool true
    | _ -> assert false)
  | Bop (Neq, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VBool (n1 <> n2)
    | VBool b1, VBool b2 -> VBool (b1 <> b2)
    | VUnit, VUnit -> VBool false
    | _ -> assert false)
  | Bop (Lt, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VBool (n1 < n2)
    | VBool b1, VBool b2 -> VBool (b1 < b2)
    | VUnit, VUnit -> VBool false
    | _ -> assert false)
  | Bop (Lte, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VBool (n1 <= n2)
    | VBool b1, VBool b2 -> VBool (b1 <= b2)
    | VUnit, VUnit -> VBool true
    | _ -> assert false)
  | Bop (Gt, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VBool (n1 > n2)
    | VBool b1, VBool b2 -> VBool (b1 > b2)
    | VUnit, VUnit -> VBool false
    | _ -> assert false)
  | Bop (Gte, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n1, VInt n2 -> VBool (n1 >= n2)
    | VBool b1, VBool b2 -> VBool (b1 >= b2)
    | VUnit, VUnit -> VBool true
    | _ -> assert false)
  | Bop (And, e1, e2) ->
    (match eval_expr env e1 with
    | VBool true ->
      (match eval_expr env e2 with
      | VBool b -> VBool b
      | _ -> assert false)
    | VBool false -> VBool false
    | _ -> assert false)
  | Bop (Or, e1, e2) ->
    (match eval_expr env e1 with
    | VBool true -> VBool true
    | VBool false ->
      (match eval_expr env e2 with
        | VBool b -> VBool b
        | _ -> assert false)
    | _ -> assert false)
  | Bop (Cons, _, _) -> assert false
  | If (e1, e2, e3) ->
    (match eval_expr env e1 with
    | VBool true -> eval_expr env e2
    | VBool false -> eval_expr env e3
    | _ -> assert false)
  | Let {is_rec = false; name; args = []; annot = _; binding; body} ->
    let v_binding = eval_expr env binding in
    let env' = Env.add name v_binding env in
      eval_expr env' body
  | Let _ -> assert false
  | Nil -> assert false
  | Tuple _ -> assert false
  | Fun _ -> assert false
  | App _ -> assert false
  | Match _ -> assert false

let eval (p : prog) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; args; annot; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = {pos; expr=Let {is_rec; name; args; annot; binding; body}} in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
  in go Env.empty None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value * ty, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* prog = Ast.Interp2.prog_of_prog prog in
  let* ty = type_of prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
  in
  Ok (v, ty)


(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _ = " ^ s in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false

let parse_ty s =
  let s = "let _ : " ^ s ^ " = assert false" in
  let p = Parser.prog Lexer.read (Lexing.from_string s) in
  match Ast.Interp2.prog_of_prog p with
  | Ok [{pos=_;stmt=SLet {annot=Some ty;_}}] -> ty
  | _ -> assert false
