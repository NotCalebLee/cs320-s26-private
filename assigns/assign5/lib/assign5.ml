module Tensor = Tensor
type tensor = Tensor.t

type 'a sexpr = 'a Sexpr.t =
  | Atom of 'a
  | List of 'a sexpr list

type op = Syntax.op = Add | Mul

type expr = Syntax.expr =
  | Ident of string * string list
  | Map of op * expr * expr
  | Fold of op * string * expr

type stmt = Syntax.stmt =
  | Init of string * int list * float sexpr
  | Set of string * string list * expr

let rec distinct = function
  | [] -> true
  | x :: xs -> not (List.mem x xs) && distinct xs

let remove_assoc key xs =
  List.filter (fun (k, _) -> k <> key) xs

let consistent (i1 : (string * int) list) (i2 : (string * int) list) : bool =
  List.for_all
    (fun (x, n) ->
      match List.assoc_opt x i2 with
      | None -> true
      | Some m -> n = m)
    i1

let union_idx_space (i1 : (string * int) list) (i2 : (string * int) list)
    : (string * int) list =
  i1 @ List.filter (fun (x, _) -> not (List.mem_assoc x i1)) i2

let dim_check (env : (string * tensor) list) (e : expr) : ((string * int) list) option =
  let rec go = function
    | Ident (name, labels) -> (
        match List.assoc_opt name env with
        | None -> None
        | Some t ->
            let old_space = Tensor.idx_space t in
            if List.length labels <> List.length old_space || not (distinct labels) then
              None
            else
              Some (List.map2 (fun lbl (_, n) -> (lbl, n)) labels old_space)
      )
    | Map (_, e1, e2) -> (
        match go e1, go e2 with
        | Some i1, Some i2 when consistent i1 i2 && consistent i2 i1 ->
            Some (union_idx_space i1 i2)
        | _ -> None
      )
    | Fold (_, x, e) -> (
        match go e with
        | Some idx_space when List.mem_assoc x idx_space ->
            Some (remove_assoc x idx_space)
        | _ -> None
      )
  in
  go e

let eval (env : (string * tensor) list) (e : expr) : tensor =
  let apply_op op a b =
    match op with
    | Add -> a +. b
    | Mul -> a *. b
  in
  let rec fold_axis op t axis idx axis_size k acc =
    if k = axis_size then acc
    else
      let v = Tensor.get t ((axis, k) :: idx) in
      let acc' =
        match acc with
        | None -> Some v
        | Some a -> Some (apply_op op a v)
      in
      fold_axis op t axis idx axis_size (k + 1) acc'
  in
  let rec go = function
    | Ident (name, labels) ->
        let t = List.assoc name env in
        Tensor.relabel_axes t labels
    | Map (op, e1, e2) ->
        let t1 = go e1 in
        let t2 = go e2 in
        let idx_space = union_idx_space (Tensor.idx_space t1) (Tensor.idx_space t2) in
        Tensor.init idx_space (fun idx ->
            apply_op op (Tensor.get t1 idx) (Tensor.get t2 idx))
    | Fold (op, axis, e) ->
        let t = go e in
        let full_space = Tensor.idx_space t in
        let axis_size = List.assoc axis full_space in
        let out_space = remove_assoc axis full_space in
        Tensor.init out_space (fun idx ->
            match fold_axis op t axis idx axis_size 0 None with
            | Some v -> v
            | None -> assert false)
  in
  go e

type error =
  | Parse_error
  | Dim_error
  | Init_error

let interp (env : (string * tensor) list) (s : string) : ((string * tensor) list, error) result =
  let interp_stmt env stmt =
    match Syntax.stmt_of_sexpr_opt stmt with
    | Some (Init (a, shape, expr)) -> (
        match Tensor.of_sexpr_opt shape expr with
        | Some t -> Ok ((a, t) :: env)
        | _ -> Error Init_error
      )
    | Some (Set (a, idx, e)) -> (
        let sort = List.sort String.compare in
        match dim_check env e with
        | Some idx_space when sort idx = sort (List.map fst idx_space) ->
            let idx_space = List.map (fun x -> (x, List.assoc x idx_space)) idx in
            let t = Tensor.init idx_space (Tensor.get (eval env e)) in
            Ok ((a, t) :: env)
        | _ -> Error Dim_error
      )
    | _ -> Error Parse_error
  in
  let rec interp_prog env = function
    | [] -> Ok env
    | e :: es -> (
        match interp_stmt env e with
        | Ok env -> interp_prog env es
        | Error e -> Error e
      )
  in
  match Sexpr.list_of_string_opt s with
  | Some ss -> interp_prog env ss
  | None -> Error Parse_error