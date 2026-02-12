
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

let is_upper c =
  let c = int_of_char c in
  65 <= c && c <= 90

let all_upper x =
  let rec loop i =
    if i >= String.length x
    then true
    else if not (is_upper x.[i])
    then false
    else loop (i + 1)
  in loop 0

let rec findvar (k : string) (env : (string * int) list) : int =
  match env with
  | [] -> failwith("not a var")
  | (x, y) :: rest ->
      if x = k then y
      else findvar k rest

let lex s =
  let rec go acc i =
    let rec go_digits acc i j =
      if i + j >= String.length s
      then List.rev (String.sub s i j :: acc)
      else if is_digit (String.get s (i + j))
      then go_digits acc i (j + 1)
      else go (String.sub s i j :: acc) (i + j)
    in
    if i >= String.length s
    then List.rev acc
    else
      match String.get s i with
      | '+' -> go ("+" :: acc) (i + 1)
      | '-' -> go ("-" :: acc) (i + 1)
      | '*' -> go ("*" :: acc) (i + 1)
      | '/' -> go ("/" :: acc) (i + 1)
      | '(' -> go ("(" :: acc) (i + 1)
      | ')' -> go (")" :: acc) (i + 1)
      | c ->
        if is_digit c
        then go_digits acc i 1
        else if is_ws c
        then go acc (i + 1)
        else assert false
  in go [] 0


let rec eval (env : (string * int) list) (expr : string list) : int =
  let (e, rest) = eval_add_sub env expr in
  match rest with
  | [] -> e
  | _ -> assert false

and eval_add_sub env expr =
  let (e, rest) = eval_mul_div env expr in
    let rec loop acc xs =
      match xs with
      | "+" :: rest -> let (x, rest) = eval_mul_div env rest in
        loop (acc + x) rest
      | "-" :: rest -> let (x, rest) = eval_mul_div env rest in
        loop (acc - x) rest
      | _ -> (acc, xs) in
    loop e rest

and eval_mul_div env expr =
  let (e, rest) = eval_num_paren env expr in
    let rec loop acc xs =
      match xs with
      | "*" :: rest -> let (x, rest) = eval_num_paren env rest in
        loop (acc * x) rest
      | "/" :: rest -> let (x, rest) = eval_num_paren env rest in
        loop (acc / x) rest
      | _ -> (acc, xs) in
    loop e rest

and eval_num_paren env expr =
  match expr with
  | "(" :: rest -> let (e, rest) = eval_add_sub env rest in
    (match rest with
      | ")" :: rest -> (e, rest)
      | _ -> assert false)
  | n :: rest -> 
    if all_upper n
    then (findvar n env, rest)
    else (int_of_string n, rest)
  | [] -> assert false

let insert_uniq (k : 'k) (v : 'v) (r : ('k * 'v) list) : ('k * 'v) list =
  let rec loop acc xs = 
    match xs with
    | [] -> (k, v) :: acc
    | (x, y) :: rest -> 
      if k = x then 
        loop acc rest
      else
        loop ((x, y) :: acc) rest
  in loop [] r

let interp (input : string) (env : (string * int) list) : int * (string * int) list =
  match lex input with
  | var :: "=" :: expr -> (
    match eval env expr with
    | ouput -> ouput, insert_uniq var ouput env
    | exception _ -> failwith "whoops!"
  )
  | _
  | exception _ -> failwith "whoops!"
