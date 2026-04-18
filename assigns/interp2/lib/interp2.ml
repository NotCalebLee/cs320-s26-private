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

let ( let* ) = Result.bind

let rec type_of_pattern (p : pattern) (expected : ty) : (ctxt, Error_msg.t) result =
  match p.pattern with
  | PUnit ->
    if expected = TUnit then Ok Env.empty
    else Error (exp_pat p.pos TUnit expected)
  | PBool _ ->
    if expected = TBool then Ok Env.empty
    else Error (exp_pat p.pos TBool expected)
  | PInt _ ->
    if expected = TInt then Ok Env.empty
    else Error (exp_pat p.pos TInt expected)
  | PNil ->
    if expected = TInt_list then Ok Env.empty
    else Error (exp_pat p.pos TInt_list expected)
  | PVar x ->
    Ok (Env.singleton x expected)
  | PCons (p1, p2) ->
    if expected <> TInt_list then
      Error (exp_pat p.pos TInt_list expected)
    else
      let* c1 = type_of_pattern p1 TInt in
      let* c2 = type_of_pattern p2 TInt_list in
      Env.fold
        (fun x t acc ->
          let* acc = acc in
          if x <> "_" && Env.mem x acc then Error (bound_several_times p2.pos x)
          else Ok (Env.add x t acc))
        c2
        (Ok c1)
| PTuple ps ->
    (match expected with
    | TTuple ts ->
        if List.length ps <> List.length ts then
          Error (exp_diff_tuple_pat p.pos expected)
        else
          let rec go ps ts acc =
            match ps, ts with
            | [], [] -> Ok acc
            | p :: ps', t :: ts' ->
                let* c = type_of_pattern p t in
                let* acc =
                  Env.fold
                    (fun x t acc ->
                      let* acc = acc in
                      if x <> "_" && Env.mem x acc then Error (bound_several_times p.pos x)
                      else Ok (Env.add x t acc))
                    c
                    (Ok acc)
                in
                go ps' ts' acc
            | _ -> assert false
          in
          go ps ts Env.empty
    | _ -> Error (exp_tuple_pat p.pos expected))

(* Type Checking *)

let rec type_of_expr (ctxt : ctxt) (e : expr) : (ty, Error_msg.t) result =
  match e.expr with
  | Unit -> Ok TUnit
  | Bool _ -> Ok TBool
  | Int _ -> Ok TInt
  | Var x ->
    (match Env.find_opt x ctxt with
    | Some t -> Ok t
    | None -> Error (unknown_var e.pos x))
  | Assert e1 ->
    let* t1 = type_of_expr ctxt e1 in
    if t1 = TBool then Ok TUnit
    else Error (exp_ty e1.pos t1 TBool)
  | Negate e1 ->
    let* t1 = type_of_expr ctxt e1 in
    if t1 = TInt then Ok TInt
    else Error (exp_ty e1.pos t1 TInt)
  | Bop (op, e1, e2) ->
    let* t1 = type_of_expr ctxt e1 in
    let* t2 = type_of_expr ctxt e2 in
    (match op with
    | Add | Sub | Mul | Div | Mod ->
      if t1 <> TInt then Error (exp_ty e1.pos t1 TInt)
      else if t2 <> TInt then Error (exp_ty e2.pos t2 TInt)
      else Ok TInt
    | Eq | Neq | Lt | Lte | Gt | Gte ->
      if t1 <> t2 then Error (exp_ty e2.pos t2 t1)
      else Ok TBool
    | And | Or ->
      if t1 <> TBool then Error (exp_ty e1.pos t1 TBool)
      else if t2 <> TBool then Error (exp_ty e2.pos t2 TBool)
      else Ok TBool
    | Cons ->
      if t1 <> TInt then Error (exp_ty e1.pos t1 TInt)
      else if t2 <> TInt_list then Error (exp_ty e2.pos t2 TInt_list)
      else Ok TInt_list)
  | If (e1, e2, e3) ->
    let* t1 = type_of_expr ctxt e1 in
    if t1 <> TBool then Error (exp_ty e1.pos t1 TBool)
    else
      let* t2 = type_of_expr ctxt e2 in
      let* t3 = type_of_expr ctxt e3 in
      if t2 = t3 then Ok t2
      else Error (exp_ty e3.pos t3 t2)
  | Let {is_rec = false; name; args = []; annot; binding; body} ->
    let* t_binding = type_of_expr ctxt binding in
    let* t_binding =
      match annot with
      | None -> Ok t_binding
      | Some t_annot -> if t_binding = t_annot then Ok t_binding
        else Error (exp_ty binding.pos t_binding t_annot)
    in
      let ctxt' = Env.add name t_binding ctxt in
        type_of_expr ctxt' body
  | Let {is_rec = false; name; args; annot; binding; body} ->
    let ctxt_binding =
      List.fold_left (fun acc (x, t) -> Env.add x t acc) ctxt args
    in
    let* t_body = type_of_expr ctxt_binding binding in
    let t_fun =
      let rec build args ret =
        match args with
        | [] -> ret
        | (_, t) :: rest -> TFun (t, build rest ret)
      in
      build args t_body
    in
    let* t_fun =
      match annot with
      | None -> Ok t_fun
      | Some t_ret ->
        if t_body = t_ret then
          let rec build args ret =
            match args with
            | [] -> ret
            | (_, t) :: rest -> TFun (t, build rest ret)
          in
            Ok (build args t_ret)
        else
          Error (exp_ty binding.pos t_body t_ret)
    in
    type_of_expr (Env.add name t_fun ctxt) body
  | Let {is_rec = true; name = _; args = []; annot = _; binding = _; body = _} ->
    Error (missing_rec_arg e.pos)
  | Let {is_rec = true; name = _; args = _; annot = None; binding = _; body = _} ->
    Error (missing_rec_annot e.pos)
  | Let {is_rec = true; name; args; annot = Some t_ret; binding; body} ->
    let t_fun =
      let rec build args ret =
        match args with
        | [] -> ret
        | (_, t) :: rest -> TFun (t, build rest ret)
      in
      build args t_ret
    in
    let ctxt_binding =
      List.fold_left (fun acc (x, t) -> Env.add x t acc) (Env.add name t_fun ctxt) args
    in
    let* t_binding = type_of_expr ctxt_binding binding in
    if t_binding <> t_ret then
      Error (exp_ty binding.pos t_binding t_ret)
    else
      type_of_expr (Env.add name t_fun ctxt) body
  | Nil -> Ok TInt_list
  | Tuple es ->
    let rec go acc = function
      | [] -> Ok (TTuple (List.rev acc))
      | e :: rest ->
        let* t = type_of_expr ctxt e in
          go (t :: acc) rest
      in go [] es
  | Fun (args, body) ->
    let ctxt' =
      List.fold_left (fun acc (x, t) -> Env.add x t acc) ctxt args
    in
    let* tbody = type_of_expr ctxt' body in
    let rec build_fun_ty args ret =
      match args with
      | [] -> ret
      | (_, t) :: rest -> TFun (t, build_fun_ty rest ret)
    in
    Ok (build_fun_ty args tbody)
| App (fn, args) ->
    let* tf = type_of_expr ctxt fn in
    let rec apply_ty current_ty remaining_args =
      match remaining_args with
      | [] -> Ok current_ty
      | arg :: rest ->
          (match current_ty with
          | TFun (tparam, tret) ->
              let* targ = type_of_expr ctxt arg in
              if targ = tparam then apply_ty tret rest
              else Error (exp_ty arg.pos targ tparam)
          | _ ->
              Error (too_many_args fn.pos tf))
    in
    (match args with
    | [] -> Ok tf
    | _ ->
        (match tf with
        | TFun _ -> apply_ty tf args
        | _ -> Error (not_func fn.pos tf)))
  | Match (e0, branches) ->
    let* t_scrut = type_of_expr ctxt e0 in
    (match branches with
      | [] -> Error (Error_msg.mk e.pos "empty match expression")
      | (p1, e1) :: rest ->
        let* c1 = type_of_pattern p1 t_scrut in
        let* t_branch = type_of_expr (Env.union (fun _ _ t2 -> Some t2) ctxt c1) e1 in
        let rec check = function
          | [] -> Ok t_branch
          | (p, e_branch) :: bs ->
            let* cp = type_of_pattern p t_scrut in
            let* t = type_of_expr (Env.union (fun _ _ t2 -> Some t2) ctxt cp) e_branch in
              if t = t_branch then check bs
              else Error (exp_ty e_branch.pos t t_branch)
          in
          check rest)


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

let rec match_pattern (v : value) (p : pattern) : dyn_env option =
  match p.pattern, v with
  | PUnit, VUnit -> Some Env.empty
  | PBool b1, VBool b2 when b1 = b2 -> Some Env.empty
  | PInt n1, VInt n2 when n1 = n2 -> Some Env.empty
  | PNil, VInt_list [] -> Some Env.empty
  | PVar x, v -> Some (Env.singleton x v)
  | PCons (p1, p2), VInt_list (n :: ns) ->
    (match match_pattern (VInt n) p1, match_pattern (VInt_list ns) p2 with
    | Some e1, Some e2 ->
        Some (Env.union (fun _ _ v2 -> Some v2) e1 e2)
    | _ -> None)
  | PTuple ps, VTuple vs when List.length ps = List.length vs ->
    let rec go ps vs acc =
      match ps, vs with
      | [], [] -> Some acc
      | p :: ps', v :: vs' ->
        (match match_pattern v p with
        | None -> None
        | Some env' ->
          go ps' vs' (Env.union (fun _ _ v2 -> Some v2) acc env'))
      | _ -> None
    in
      go ps vs Env.empty
  | _ -> None

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
  | Nil -> VInt_list []
  | Assert e1 ->
    (match eval_expr env e1 with
    | VBool true -> VUnit
    | VBool false -> raise (Assert_fail e.pos)
    | _ -> assert false)
  | Negate e1 ->
    (match eval_expr env e1 with
    | VInt n -> VInt (-n)
    | _ -> assert false)
  | Tuple es ->
    VTuple (List.map (eval_expr env) es)
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
    VBool (eval_expr env e1 = eval_expr env e2)
  | Bop (Neq, e1, e2) ->
    VBool (eval_expr env e1 <> eval_expr env e2)
| Bop (Lt, e1, e2) ->
    VBool (eval_expr env e1 < eval_expr env e2)

| Bop (Lte, e1, e2) ->
    VBool (eval_expr env e1 <= eval_expr env e2)

| Bop (Gt, e1, e2) ->
    VBool (eval_expr env e1 > eval_expr env e2)

| Bop (Gte, e1, e2) ->
    VBool (eval_expr env e1 >= eval_expr env e2)
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
  | Bop (Cons, e1, e2) ->
    (match eval_expr env e1, eval_expr env e2 with
    | VInt n, VInt_list ns -> VInt_list (n :: ns)
    | _ -> assert false)
  | If (e1, e2, e3) ->
    (match eval_expr env e1 with
    | VBool true -> eval_expr env e2
    | VBool false -> eval_expr env e3
    | _ -> assert false)
  | Fun (args, body) ->
    VClos {env; name = None; args = List.map fst args; body;}
  | App (fn, args) ->
    let rec apply vf args =
      match args with
      | [] -> vf
      | arg :: rest ->
        (match vf with
        | VClos {env = clos_env; name; args = x :: xs; body} ->
          let v_arg = eval_expr env arg in
            let self =
              match name with
              | None -> None
              | Some f ->
                (match Env.find_opt f clos_env with
                | Some v -> Some (f, v)
                | None -> Some (f, vf))
            in
            let env' = Env.add x v_arg clos_env in
              if xs = [] then
                let env'' =
                  match self with
                  | None -> env'
                  | Some (f, vself) -> Env.add f vself env'
                in
                  apply (eval_expr env'' body) rest
              else
                let env'' =
                  match self with
                  | None -> env'
                  | Some (f, vself) -> Env.add f vself env'
                in
                  let vf' = VClos {env = env''; name; args = xs; body;}
                  in
                    apply vf' rest
          | _ -> assert false)
    in
      apply (eval_expr env fn) args
  | Let {is_rec = false; name; args = []; annot = _; binding; body} ->
    let v_binding = eval_expr env binding in
    let env' = Env.add name v_binding env in
      eval_expr env' body
  | Let {is_rec = false; name; args; annot = _; binding; body} ->
    let v_binding =
      VClos {env; name = None; args = List.map fst args; body = binding;}
      in
        let env' = Env.add name v_binding env in
          eval_expr env' body
  | Let {is_rec = true; name; args; annot = _; binding; body} ->
    let v_binding =
      VClos {env; name = Some name; args = List.map fst args; body = binding;}
      in
        let env' = Env.add name v_binding env in
          eval_expr env' body
  | Match (e0, branches) ->
    let v0 = eval_expr env e0 in
      let rec go = function
      | [] -> raise (Match_fail e.pos)
      | (p, e_branch) :: rest ->
        (match match_pattern v0 p with
        | Some penv -> eval_expr (Env.union (fun _ v1 _ -> Some v1) env penv) e_branch
        | None -> go rest)
      in
        go branches

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
