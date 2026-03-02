
let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

type token = Lpar | Rpar | Word of string

let tokens_of_string (s : string) : token list =
  let rec go acc i =
    if i >= String.length s
    then acc
    else
      match s.[i] with
      | '(' -> go (Lpar :: acc) (i + 1)
      | ')' -> go (Rpar :: acc) (i + 1)
      | c ->
        if is_ws c
        then go acc (i + 1)
        else
          let rec go' j =
            if i + j >= String.length s
            then Word (String.sub s i j) :: acc
            else
              let c = s.[i + j] in
              if List.mem c ['('; ')'] || is_ws c
              then go (Word (String.sub s i j) :: acc) (i + j)
              else go' (j + 1)
          in go' 1
  in List.rev (go [] 0)

type sexpr =
  | Atom of string
  | List of sexpr list

let sexpr_of_tokens_opt (ts : token list) : sexpr option =
  let rec go (ts : token list) : (sexpr * token list) option =
    match ts with
    | [] -> None
    | t :: ts -> (
        match t with
        | Word a -> Some (Atom a, ts)
        | Lpar -> (
            match go' ts with
            | es, Rpar :: ts -> Some (List es, ts)
            | _ -> None
          )
        | Rpar -> None
      )
  and go' (ts : token list) : sexpr list * token list =
    match go ts with
    | Some (e, ts) ->
      let (es, ts) = go' ts in
      e :: es, ts
    | None -> [], ts
  in
  match go ts with
  | Some (e, []) -> Some e
  | _ -> None

let sexpr_of_string_opt (s : string) : sexpr option =
  sexpr_of_tokens_opt (tokens_of_string s)

let rec string_of_sexpr (e : sexpr) : string =
  match e with
  | Atom s -> s
  | List ss -> "(" ^ String.concat " " (List.map string_of_sexpr ss) ^ ")"

type op = Add | Mul | Eq

let string_of_op (op : op) : string =
  match op with
  | Add -> "+"
  | Mul -> "*"
  | Eq -> "="

let op_of_sexpr_opt (s : sexpr) : op option =
  match s with
  | Atom "+" -> Some Add
  | Atom "*" -> Some Mul
  | Atom "=" -> Some Eq
  | _ -> None

type expr =
  | Int of int
  | Bop of op * expr * expr
  | If of expr * expr * expr

let rec expr_of_sexpr_opt (s : sexpr) : expr option =
  match s with
  | Atom str -> Some (Int (int_of_string str))
  | List (Atom "if" :: e1 :: e2 :: e3 :: []) -> 
    (match expr_of_sexpr_opt e1, expr_of_sexpr_opt e2, expr_of_sexpr_opt e3 with
    | Some e1, Some e2, Some e3 -> Some (If (e1, e2, e3))
    | _ -> None)
  | List (op :: e1 :: e2 :: []) -> 
    (match op_of_sexpr_opt op, expr_of_sexpr_opt e1, expr_of_sexpr_opt e2 with
      | Some op, Some e1, Some e2 -> Some (Bop (op, e1, e2))
      | _ -> None)
  | _ -> None 

let expr_of_string_opt (s : string) : expr option =
  match sexpr_of_string_opt s with
  | Some sexpr -> expr_of_sexpr_opt sexpr
  | None -> None 


let rec sexpr_of_expr (e : expr) : sexpr =
  match e with 
  | Int n -> Atom (string_of_int n)
  | Bop (op, e1, e2) -> List [Atom (string_of_op op); sexpr_of_expr e1; sexpr_of_expr e2]
  | If (e1, e2, e3) -> List [Atom "if"; sexpr_of_expr e1; sexpr_of_expr e2; sexpr_of_expr e3]

let string_of_expr (e : expr) : string =
  string_of_sexpr (sexpr_of_expr e)

type ty = BoolT | IntT

let ty_of_sexpr_opt (e : sexpr) : ty option =
  match e with
  | Atom s -> (
      match s with
      | "bool" -> Some BoolT
      | "int" -> Some IntT
      | _ -> None
    )
  | _ -> None

let string_of_ty (ty : ty) : string =
  match ty with
  | BoolT -> "bool"
  | IntT -> "int"

type ty_jmt =
  {
    expr : expr;
    ty : ty;
  }

let string_of_ty_jmt (j : ty_jmt) : string =
  string_of_expr j.expr ^ " : " ^ string_of_ty j.ty

type ty_rule =
  | Int_lit
  | Add_int
  | Mul_int
  | Eq_rule
  | If_rule

let string_of_ty_rule (r : ty_rule) =
  match r with
  | Int_lit -> "intLit"
  | Add_int -> "addInt"
  | Mul_int -> "mulInt"
  | Eq_rule -> "eq"
  | If_rule -> "if"

let ty_rule_of_sexpr_opt (e : sexpr) : ty_rule option =
  match e with
  | Atom s -> (
      match s with
      | "INTLIT" -> Some Int_lit
      | "ADDINT" -> Some Add_int
      | "MULINT" -> Some Mul_int
      | "EQ" -> Some Eq_rule
      | "IF" -> Some If_rule
      | _ -> None
    )
  | _ -> None

type ty_deriv =
  | Rule_app of {
      prem_derivs : ty_deriv list;
      concl : ty_jmt;
      rname : ty_rule;
    }
  | Hole

let rec ty_deriv_of_sexpr_opt (s : sexpr) : ty_deriv option =
  match s with
  | Atom "???" -> Some Hole
  | List (expr :: ty :: rname :: prem) -> 
    (match expr_of_sexpr_opt expr, ty_of_sexpr_opt ty, ty_rule_of_sexpr_opt rname with
    | Some expr, Some ty, Some rname -> 
      let rec prem_list l = 
        match l with 
        | [] -> Some []
        | prem :: rest -> (match ty_deriv_of_sexpr_opt prem, prem_list rest with 
          | Some x, Some xs -> Some (x :: xs)
          | _ -> Some [Hole])
      in (match prem_list prem with
        | Some prem_derivs -> Some (Rule_app { prem_derivs; concl = {expr; ty}; rname})
        | None -> None)
    | _ -> None)
  | _ -> None

let ty_deriv_of_string_opt (s : string) : ty_deriv option =
  match sexpr_of_string_opt s with 
  | Some sexpr -> ty_deriv_of_sexpr_opt sexpr
  | None -> None

let string_of_ty_deriv (d : ty_deriv) : string =
  let rec go d =
    match d with
    | Hole -> [("???", "hole")]
    | Rule_app d ->
      (string_of_ty_jmt d.concl, string_of_ty_rule d.rname) :: go' [] d.prem_derivs
  and go' has_line ds =
    let lines =
      List.fold_left
        (fun acc b -> (if b then "│  " else "   ") ^ acc)
        ""
        has_line
    in
    match ds with
    | [] -> []
    | [Hole] ->
      [lines ^ "└──???" , "hole"]
    | [Rule_app d] ->
      let next_line =
        ( lines ^ "└──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in next_line :: go' (false :: has_line) d.prem_derivs
    | Hole :: ds -> (lines ^ "├──???" , "hole") :: go' has_line ds
    | Rule_app d :: ds ->
      let next_line =
        ( lines ^ "├──" ^ string_of_ty_jmt d.concl
        , string_of_ty_rule d.rname
        )
      in
      next_line
      :: go' (true :: has_line) d.prem_derivs
      @ go' has_line ds
  in
  let lines = go d in
  let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0 in
  let width =
    List.fold_left
      (fun acc (line, _) -> max acc (length line))
      0
      lines
  in
  let lines =
    List.map
      (fun (line, rname) ->
         String.concat ""
           [
             line;
             String.init (width - length line + 2) (fun _ -> ' ');
             "("; rname; ")";
           ])
      lines
  in
  String.concat "\n" lines

let check_rule (r : ty_rule) (prems : ty_jmt option list) (concl : ty_jmt) : bool =
  let check_prem (exp_expr : expr) (exp_ty : ty) (prem_opt : ty_jmt option) : bool = 
    match prem_opt with 
    | None -> true
    | Some j -> j.expr = exp_expr && j.ty = exp_ty
  in 
  match r with 
  | Int_lit -> (
    match concl.expr, prems with
    | Int _, [] -> concl.ty = IntT
    | _ -> false)
  | Add_int -> (
    match concl.expr, prems with 
    | Bop (Add, e1, e2), [p1; p2] -> concl.ty = IntT && check_prem e1 IntT p1 && check_prem e2 IntT p2
    | _ -> false)
  | Mul_int -> (
    match concl.expr, prems with 
    | Bop (Mul, e1, e2), [p1; p2] -> concl.ty = IntT && check_prem e1 IntT p1 && check_prem e2 IntT p2
    | _ -> false)
  | Eq_rule -> (
    match concl.expr, prems with
    | Bop (Eq, e1, e2), [p1; p2] -> concl.ty = BoolT && check_prem e1 IntT p1 && check_prem e2 IntT p2
    | _ -> false)
  |If_rule -> (
    match concl.expr, prems with
    | If (e1, e2, e3), [p1; p2; p3] -> check_prem e1 BoolT p1 && check_prem e2 concl.ty p2 && check_prem e3 concl.ty p3
    | _ -> false)
  
  

type status =
  | Complete
  | Invalid
  | Partial

let rec check_deriv (d : ty_deriv) : status =
  match d with
  | Hole -> Partial
  | Rule_app {prem_derivs; concl; rname} -> 
    let prem_status = List.map check_deriv prem_derivs in 
      let rec prem_invalid l = 
        match l with
        | [] -> false
        | x :: xs -> (x = Invalid) || prem_invalid xs
      in 
      if prem_invalid prem_status then Invalid
      else let (prem_jmts : ty_jmt option list) =
        List.map (fun x -> match x with
          | Hole -> None
          | Rule_app d -> Some d.concl) prem_derivs
      in 
      if not(check_rule rname prem_jmts concl) then Invalid
      else 
        let rec prem_complete l = 
          match l with 
          | [] -> true
          | x :: xs -> (x = Complete) && prem_complete xs
        in 
        if prem_complete prem_status then Complete
        else Partial
        

type value = BoolV of bool | IntV of int

let string_of_value (v : value) : string =
  match v with
  | BoolV b -> string_of_bool b
  | IntV n -> string_of_int n

let rec value_of_expr (e : expr) : value =
  match e with 
  | Int n -> IntV n
  | Bop (op, e1, e2) -> 
    let v1 = value_of_expr e1 in
    let v2 = value_of_expr e2 in 
    (match op, v1, v2 with
    | Add, IntV n1, IntV n2 -> IntV (n1 + n2)
    | Mul, IntV n1, IntV n2 -> IntV (n1 * n2)
    | Eq, IntV n1, IntV n2 -> BoolV (n1 = n2)
    | _ -> failwith "undefined")
  | If (e1, e2, e3) -> 
    let v1 = value_of_expr e1 in 
      (match v1 with 
      | BoolV true -> value_of_expr e2
      | BoolV false -> value_of_expr e3
      | _ -> failwith "undefined")


type error = Parse_error | Invalid_deriv of ty_deriv

let interp (s : string) : (ty_deriv * value option, error) result  =
  match ty_deriv_of_string_opt s with
  | Some deriv -> (
    match check_deriv deriv with
    | Complete -> (
      match deriv with
      | Rule_app d -> Ok (deriv, Some (value_of_expr d.concl.expr))
      | _ -> assert false
    )
    | Partial -> Ok (deriv, None)
    | Invalid -> Error (Invalid_deriv deriv)
  )
  | None -> Error Parse_error

let example_deriv : string = "" (* TODO *)
