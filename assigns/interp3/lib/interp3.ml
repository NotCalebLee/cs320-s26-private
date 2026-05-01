open Utils
module Error_msg = Error_msg
module Ast = Ast

(* SYNTAX
   ----------------------------------------------------------------------
*)

type ty = Ast.Type.t =
  | TUnit
  | TBool
  | TInt
  | TString
  | TTuple of ty list
  | TAdt of ty list * string
  | TFun of ty * ty
  | TParam of string

type _pattern = Ast.Pattern.pattern =
  | PWild
  | PVar of string
  | PUnit
  | PBool of bool
  | PInt of int
  | PString of string
  | PTuple of pattern list
  | PCons of string * pattern option
and pattern = Ast.Pattern.t =
  {
    pos : pos;
    pattern : _pattern;
  }

type bop = Ast.Expr.bop =
  | Add | Sub | Mul
  | Div | Mod
  | And | Or
  | Concat
  | Eq | Neq | Lt | Lte | Gt | Gte

type _expr = Ast.Expr.expr =
  | Unit
  | Bool of bool
  | Int of int
  | String of string
  | Negate of expr
  | Bop of bop * expr * expr
  | If of expr * expr * expr
  | Annot of expr * ty
  | Tuple of expr list
  | Assert of expr
  | Var of string
  | Cons of string * expr option
  | Fun of (string * ty option) * expr
  | App of expr * expr
  | Let of
      {
        is_rec : bool;
        name : string;
        binding : expr;
        body : expr;
      }
  | Match of expr * (pattern * expr) list
and expr = Ast.Expr.t =
  {
    pos : pos;
    expr : _expr;
  }

type _stmt = Ast.Stmt.stmt =
  | SLet of
      {
        is_rec : bool;
        name : string;
        binding : expr;
      }

  | SAdt of
      {
        tpars : string list;
        name : string;
        constrs : (string * ty option) list
      }
and stmt = Ast.Stmt.t =
  {
    pos : pos;
    stmt : _stmt;
  }

module Env = Map.Make(String)


(* TYPE ERRORS
   ----------------------------------------------------------------------
*)

let dummy_error = Error_msg.mk dummy_pos "Dummy error"

let unknown_var pos x = Error_msg.mk pos (Format.asprintf "Unbound value %s" x)

let exp_ty pos t1 t2 =
  let msg =
    Format.asprintf
      "This expression has type %a but an expression was expected of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let invalid_app pos = Error_msg.mk pos "Invalid application"

let invalid_tuple pos = Error_msg.mk pos "Invalid tuple"

let unknown_cons pos x = Error_msg.mk pos (Format.asprintf "Unbound constructor %s" x)

let cons_exp_no_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects 0 arguments" x)

let cons_exp_args pos x =
  Error_msg.mk
    pos
    (Format.asprintf "The constructor %s expects arguments" x)

let exp_pat pos t1 t2 =
  let msg =
    Format.asprintf
      "This pattern matches values of type %a but a pattern was expected which matches values of type %a"
      Ast.Type.pp t1 Ast.Type.pp t2
  in Error_msg.mk pos msg

let bound_several_times pos x =
  let msg =
    Format.asprintf
      "Variable %s is bound several times in this matching"
      x
  in Error_msg.mk pos msg

let dup_ty_name pos x =
  let msg =
    Format.asprintf
      "Type using name %s is already defined"
      x
  in Error_msg.mk pos msg

let unbound_ty_var pos n =
  Error_msg.mk
    pos
    (Format.asprintf "The type variable %s is unbound in this type declaration" n)

let ty_param_several_times pos =
  Error_msg.mk
    pos
    "A type parameter occurs several times"

(* TYPING
   ----------------------------------------------------------------------
*)

type ty_scheme = string list * ty
type ctxt = ty_scheme Env.t
type constr = ty * ty

let fresh () = TParam (_gensym ())

(* Helper *)
let rec subst_ty (x : string) (r : ty) (t : ty) : ty =
  match t with 
  | TUnit | TBool | TInt | TString -> t
  | TParam y -> if x = y then r 
    else t
  | TTuple ts -> TTuple (List.map (subst_ty x r) ts)
  | TAdt (ts, name) -> TAdt (List.map (subst_ty x r) ts, name)
  | TFun (t1, t2) -> TFun(subst_ty x r t1, subst_ty x r t2)

let instantiate (vars, t : ty_scheme) : ty =
  let rec go vars t = 
    match vars with
    | [] -> t
    | x :: xs -> 
      let fresh_ty = fresh() in 
        go xs (subst_ty x fresh_ty t)
    in 
      go vars t

let type_of_expr (ctxt : ctxt) (e : expr) : (ty_scheme, Error_msg.t) result =
  let rec type_pattern ctxt (p : pattern) : (ty * ctxt, Error_msg.t) result =
  match p.pattern with
  | PWild -> Ok (fresh (), Env.empty)
  | PVar x ->
    let t = fresh () in
      Ok (t, Env.add x ([], t) Env.empty)
  | PUnit -> Ok (TUnit, Env.empty)
  | PBool _ -> Ok (TBool, Env.empty)
  | PInt _ -> Ok (TInt, Env.empty)
  | PString _ -> Ok (TString, Env.empty)
  
  let rec go ctxt e = 
    match e.expr with 
    | Unit -> Ok ([], TUnit)
    | Bool _ -> Ok ([], TBool)
    | Int _ -> Ok ([], TInt)
    | String _ -> Ok ([], TString)

    | Negate e1 -> 
      begin match go ctxt e1 with
      | Ok (_, TInt) -> Ok ([], TInt)
      | Ok (_, t) -> Error (exp_ty e.pos t TInt)
      | Error e -> Error e
      end
    | Bop (op, e1, e2) ->
      begin match op with
      | Add | Sub | Mul | Div | Mod -> 
        begin match go ctxt e1, go ctxt e2 with
        | Ok (_, TInt), Ok (_, TInt) -> Ok ([], TInt)
        | Ok (_, t), Ok (_, _) -> Error (exp_ty e1.pos t TInt)
        | Error e, _ -> Error e
        | _, Error e -> Error e
        end
      | And | Or -> 
        begin match go ctxt e1, go ctxt e2 with
        | Ok (_, TBool), Ok (_, TBool) -> Ok ([], TBool)
        | Ok (_, t1), Ok (_, t2) ->
          if t1 <> TBool then Error (exp_ty e1.pos t1 TBool)
          else if t2 <> TBool then Error (exp_ty e2.pos t2 TBool)
          else Ok ([], TBool)        
        | Error e, _ -> Error e
        | _, Error e -> Error e
        end
      | Concat -> 
        begin match go ctxt e1, go ctxt e2 with
        | Ok (_, TString), Ok (_, TString) -> Ok ([], TString)
        | Ok (_, t1), Ok (_, t2) -> 
          if t1 <> TString then Error (exp_ty e1.pos t1 TString)
          else if t2 <> TString then Error (exp_ty e2.pos t2 TString)
          else Ok ([], TString)
        | Error e, _ -> Error e
        | _, Error e -> Error e
        end
      | Eq | Neq | Lt | Lte | Gt | Gte -> 
        begin match go ctxt e1, go ctxt e2 with
        | Ok (_, t1), Ok (_, t2) -> 
          if t1 <> t2 then Error (exp_ty e2.pos t2 t1)
          else Ok ([], TBool)
        | Error e, _ -> Error e
        | _, Error e -> Error e
        end
      end

    | If (e1, e2, e3) -> 
      begin match go ctxt e1, go ctxt e2, go ctxt e3 with
      | Ok (_, TBool), Ok(_, t2), Ok(_, t3) ->
        if t2 = t3 then Ok([], t2)
        else Error (exp_ty e3.pos t3 t2)
      | Ok (_, t1), Ok _, Ok _ -> Error (exp_ty e1.pos t1 TBool)
      | Error e, _, _ -> Error e
      | _, Error e, _ -> Error e
      | _, _, Error e -> Error e
      end

    | Var x -> 
      begin match Env.find_opt x ctxt with 
      | Some scheme -> Ok([], instantiate scheme)
      | None -> Error (unknown_var e.pos x)
      end

    | Fun ((x, ty_opt), body) -> 
      let arg_ty = 
        match ty_opt with
        | Some t -> t
        | None -> fresh ()
      in
      let ctxt' = Env.add x ([], arg_ty) ctxt in 
        begin match go ctxt' body with 
        | Ok (_, body_ty) -> Ok ([], TFun (arg_ty, body_ty))
        | Error e -> Error e
        end
    
    | App (e1, e2) ->
      begin match go ctxt e1, go ctxt e2 with
      | Ok (_, TFun (arg_ty, ret_ty)), Ok (_, body_ty) -> 
        if body_ty = arg_ty then Ok([], ret_ty)
        else Error (exp_ty e2.pos body_ty arg_ty)
      | Ok _, Ok _ -> Error (invalid_app e1.pos)
      | Error e, _ -> Error e
      |_, Error e -> Error e
      end

    | Let {is_rec; name; binding; body} ->
      if not is_rec then 
        begin match go ctxt binding with 
        | Ok (_, binding_ty) ->
          let ctxt' = Env.add name ([], binding_ty) ctxt in 
            go ctxt' body
        | Error e -> Error e
        end
      else
        let fresh_ty = fresh () in 
        let ctxt_with_name = Env.add name ([], fresh_ty) ctxt in 
          begin match go ctxt_with_name binding with 
          | Ok (_, binding_ty) -> 
              if fresh_ty = binding_ty then
                let ctxt' = Env.add name ([], binding_ty) ctxt in 
                  go ctxt' body
              else 
                Error (exp_ty binding.pos binding_ty fresh_ty)
          | Error e -> Error e 
          end

    | Tuple l -> 
      let rec type_all l = 
        match l with
        | [] -> Ok []
        | e1 :: rest -> 
          begin match go ctxt e1, type_all rest with
          | Ok (_, t1), Ok ts -> Ok (t1 :: ts)
          | Error e, _ -> Error e
          | _, Error e -> Error e 
          end
      in 
      begin match type_all l with
      | Ok ts -> Ok ([], TTuple ts)
      | Error e -> Error e 
      end 
    
    | Assert e1 -> 
      begin match e1.expr with
      | Bool false -> Ok ([], fresh ())
      | _-> 
        begin match go ctxt e1 with
        | Ok (_, TBool) -> Ok ([], TUnit)
        | Ok (_, t) -> Error (exp_ty e1.pos t TBool)
        | Error e -> Error e 
        end
      end
    
    
    | Cons (name, arg_opt) -> 
      begin match Env.find_opt name ctxt with 
      | None -> Error (unknown_cons e.pos name)
      | Some scheme -> 
        let cons_ty = instantiate scheme in 
          begin match cons_ty, arg_opt with
          | TFun (expected_arg_ty, ret_ty), Some arg ->
            begin match go ctxt arg with 
            | Ok (_, actual_arg_ty) -> 
              if actual_arg_ty = expected_arg_ty then Ok([], ret_ty)
              else Error (exp_ty arg.pos actual_arg_ty expected_arg_ty)
            | Error e -> Error e 
            end
          | TFun _, None -> Error (cons_exp_args e.pos name)
          | ty, None -> Ok ([], ty)
          | _, Some _ -> Error (cons_exp_no_args e.pos name)
          end
      end


      
    | _ -> assert false
  in go ctxt e

let rec nub l =
  match l with
  | [] -> []
  | x :: xs -> x :: List.filter ((<>) x) (nub xs)

let free_vars ty =
  let rec go = function
    | TTuple ts | TAdt (ts, _) -> List.concat_map go ts
    | TFun (t1, t2) -> go t1 @ go t2
    | TParam a -> [a]
    | _ -> []
  in nub (go ty)

let well_typed (p : stmt list) : (unit, Error_msg.t) result =
  let rec go (used_ty_names : string list) (ctxt : ctxt) p =
    match p with
    | [] -> Ok ()
    | {pos; stmt=SLet {is_rec;name;binding}} :: ps ->
      let body = Ast.Expr.var dummy_pos name in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      begin
        match type_of_expr ctxt e with
        | Ok ty -> go used_ty_names (Env.add name ty ctxt) ps
        | Error e -> Error e
      end
    | {pos; stmt=SAdt {tpars; name; constrs}} :: ps ->
      if nub tpars = tpars
      then
        if List.mem name used_ty_names
        then Error (dup_ty_name pos name)
        else
          let rec process ctxt cs =
            match cs with
            | [] -> Ok ctxt
            | (cons_name, None) :: cs ->
              let tparams = List.map (fun x -> TParam x) tpars in
              process (Env.add cons_name (tpars, TAdt(tparams, name)) ctxt) cs
            | (cons_name, Some ty) :: cs ->
              begin
                match List.(find_opt (fun x -> not (mem x tpars)) (free_vars ty)) with
                | None ->
                  let tparams = List.map (fun x -> TParam x) tpars in
                  let ctxt = Env.add cons_name (tpars, TFun (ty, TAdt(tparams, name))) ctxt in
                  process ctxt cs
                | Some a -> Error (unbound_ty_var pos a)
              end
          in
          match process ctxt constrs with
          | Ok ctxt -> go (name :: used_ty_names) ctxt ps
          | Error e-> Error e
      else Error (ty_param_several_times pos)
  in
  let ctxt =
    Env.(
      empty
      |> add "print_endline" ([], TFun (TString, TUnit))
      |> add "Nil" (["a"], TAdt ([TParam "a"], "list"))
      |> add "Cons" (["a"], TFun (TTuple [TParam "a"; TAdt ([TParam "a"], "list")], TAdt ([TParam "a"], "list")))
    )
  in go [] ctxt p

(* EVALUATION
   ----------------------------------------------------------------------
*)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VString of string
  | VCons of string * value option
  | VTuple of value list
  | VClos of {
      env : value Env.t;
      name : string option;
      arg : string;
      body : expr;
    }

type dyn_env = value Env.t

exception Div_by_zero of pos
exception Assert_fail of pos
exception Match_fail of pos
exception Compare_fun_val of pos


let rec pattern_match v p : dyn_env option = 
  begin match p.pattern, v with 
  | PWild, _ -> Some Env.empty
  | PVar x, _ -> Some (Env.singleton x v)
  | PUnit, VUnit -> Some Env.empty
  | PBool b1, VBool b2 when b1 = b2 -> Some Env.empty
  | PInt n1, VInt n2 when n1 = n2 -> Some Env.empty
  | PString s1, VString s2 when s1 = s2 -> Some Env.empty
  | PCons (c1, Some p), VCons (c2, Some v) when c1 = c2 -> pattern_match v p 
  | _ -> None
  end

let rec eval_expr (env : dyn_env) (e : Ast.Expr.t) : value =
  match e.expr with
  | Unit -> VUnit
  | Bool b -> VBool b
  | Int n -> VInt n
  | String s -> VString s
  | Var x -> Env.find x env 
  | Negate e1 -> 
    begin match eval_expr env e1 with 
    | VInt n -> VInt (-n)
    | _ -> assert false
    end
  | Bop (op, e1, e2) -> 
    begin match op with
    | Add -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VInt (n1 + n2)
      | _ -> assert false
      end
    | Sub -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VInt (n1 - n2)
      | _ -> assert false
      end
    | Mul -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VInt (n1 * n2)
      | _ -> assert false
      end
    | Div -> 
      begin match eval_expr env e2 with
      | VInt 0 -> raise (Div_by_zero e.pos)
      | VInt n2 -> 
        begin match eval_expr env e1 with
        | VInt n1 -> VInt (n1 / n2)
        | _ -> assert false
        end
      | _ -> assert false
      end
    | Mod -> 
      begin match eval_expr env e2 with
      | VInt 0 -> raise (Div_by_zero e.pos)
      | VInt n2 -> 
        begin match eval_expr env e1 with
        | VInt n1 -> VInt (n1 mod n2)
        | _ -> assert false
        end
      | _ -> assert false
      end
    | And -> 
      begin match eval_expr env e1 with 
      | VBool false -> VBool false
      | VBool true -> eval_expr env e2
      | _ -> assert false
      end
    | Or -> 
      begin match eval_expr env e1 with 
      | VBool true -> VBool true
      | VBool false -> eval_expr env e2
      | _ -> assert false
      end
    | Concat -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VString s1, VString s2 -> VString (s1 ^ s2)
      | _ -> assert false
      end
    | Eq -> VBool (eval_expr env e1 = eval_expr env e2)
    | Neq -> VBool (eval_expr env e1 <> eval_expr env e2)
    | Lt -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VBool (n1 < n2)
      | VString s1, VString s2 -> VBool (s1 < s2)
      | VBool b1, VBool b2 -> VBool (b1 < b2)
      | _ -> assert false
      end
    | Lte -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VBool (n1 <= n2)
      | VString s1, VString s2 -> VBool (s1 <= s2)
      | VBool b1, VBool b2 -> VBool (b1 <= b2)
      | _ -> assert false
      end
    | Gt -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VBool (n1 > n2)
      | VString s1, VString s2 -> VBool (s1 > s2)
      | VBool b1, VBool b2 -> VBool (b1 > b2)
      | _ -> assert false
      end
    | Gte -> 
      begin match eval_expr env e1, eval_expr env e2 with
      | VInt n1, VInt n2 -> VBool (n1 >= n2)
      | VString s1, VString s2 -> VBool (s1 >= s2)
      | VBool b1, VBool b2 -> VBool (b1 >= b2)
      | _ -> assert false
      end
    end

    | If (e1, e2, e3) -> 
      begin match eval_expr env e1 with 
      | VBool true -> eval_expr env e2
      | VBool false -> eval_expr env e3
      | _ -> assert false
      end
    
    | Fun ((arg, _), body) -> 
      VClos {env = env; name = None; arg = arg; body = body;}
    
    | App (e1, e2) -> 
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in 
      begin match v1 with 
      | VClos {env = clos_env; name = None; arg; body} -> 
        let new_env = Env.add arg v2 clos_env in
          eval_expr new_env body
      | VClos {env = clos_env; name = Some f; arg; body} -> 
        let new_env = Env.add arg v2 (Env.add f v1 clos_env)
        in eval_expr new_env body
      | _ -> assert false
      end
    | Annot (e1, _) -> eval_expr env e1

    | Tuple l -> VTuple (List.map (eval_expr env) l)

    | Assert e1 -> 
      begin match eval_expr env e1 with
      | VBool true -> VUnit
      | VBool false -> raise (Assert_fail e.pos)
      | _ -> assert false
      end

    | Cons (name, None) -> VCons (name, None)

    | Cons (name, Some arg) -> 
      let v = eval_expr env arg 
      in VCons (name, Some v)
      
    | Let {is_rec; name; binding; body} -> 
      if is_rec then
        begin match binding.expr with 
        | Fun((arg, _), fun_body) -> 
          let clos = VClos{env = env; name = Some name; arg = arg; body = fun_body;}
          in 
          let env' = Env.add name clos env 
          in eval_expr env' body
        | _ -> assert false
        end 
      else
        let v = eval_expr env binding in 
        let env' = Env.add name v env 
        in eval_expr env' body  


    | _ -> assert false
    

let eval (p : stmt list) : value =
  let rec go env v p =
    match p with
    | [] -> Option.value ~default:VUnit v
    | {pos; stmt=SLet {is_rec; name; binding}} :: ps ->
      let body = {pos=dummy_pos; expr=Var name} in
      let e = Ast.Expr.let_ pos is_rec name [] None binding body in
      let v = eval_expr env e in
      go (Env.add name v env) (Some v) ps
    | _ :: ps -> go env v ps
  in
  let env =
    Env.(
      empty
      |> add "print_endline"
        (VClos
           {
             env = empty;
             name = None;
             arg = "$print_endline";
             body = Ast.Expr.mk dummy_pos Unit;
           })
    )
  in go env None p


(* INTERPRETER
   ----------------------------------------------------------------------
*)

let interp ~(filename : string) : (value, Error_msg.t) result =
  let ( let* ) = Result.bind in
  let* prog = Syntax.parse ~filename in
  let* () = well_typed prog in
  let* v =
    match eval prog with
    | v -> Ok v
    | exception Assert_fail pos -> Error (Error_msg.mk pos "(Exception) Assert_fail")
    | exception Div_by_zero pos -> Error (Error_msg.mk pos "(Exception) Div_by_zero")
    | exception Match_fail pos -> Error (Error_msg.mk pos "(Exception) Match_fail")
    | exception Compare_fun_val pos -> Error (Error_msg.mk pos "(Exception) Compare_fun_val")
  in
  Ok v

(* TESTING STUFF
   ----------------------------------------------------------------------
*)

let parse_expr s =
  let s = "let _x = " ^ s in
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | [{pos=_;stmt=SLet {binding=e;_}}] -> e
  | _ -> assert false
