
let is_ws c =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let is_digit c =
  let c = int_of_char c in
  48 <= c && c <= 57

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


let rec eval expr =
  let (e, rest) = eval_add_sub expr in
  match rest with
  | [] -> e

and eval_add_sub expr =
  let (e, rest) = eval_mul_div expr in
    let rec loop acc xs =
      match xs with
      | "+" :: rest -> let (x, rest) = eval_mul_div rest in
        loop (acc + x) rest
      | "-" :: rest -> let (x, rest) = eval_mul_div rest in
        loop (acc - x) rest
      | _ -> (acc, xs) in
    loop e rest

and eval_mul_div expr =
  let (e, rest) = eval_num_paren expr in
    let rec loop acc xs =
      match xs with
      | "*" :: rest -> let (x, rest) = eval_num_paren rest in
        loop (acc * x) rest
      | "/" :: rest -> let (x, rest) = eval_num_paren rest in
        loop (acc / x) rest
      | _ -> (acc, xs) in
    loop e rest

and eval_num_paren expr =
  match expr with
  | "(" :: rest -> let (e, rest) = eval_add_sub rest in
    match rest with
      | ")" :: rest -> (e, rest)
  | n :: rest -> (int_of_string n, rest)
  | [] -> assert false


let interp (input : string) : int =
  match eval (lex input) with
  | output -> output
  | exception _ -> failwith "whoops!"
