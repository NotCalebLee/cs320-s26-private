let sqrt (n : int) : int = 
  let rec loop x = 
    if(x * x) >= n then x
    else loop (x + 1)
  in loop 0

let rec pow (n : int) (k : int) : int = 
  if k = 0  then 1
  else n * pow n (k - 1)


let is_ws = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let string_of_char (c : char) : string =
  String.init 1 (fun _ -> c)

let explode (s : string) : char list =
  let rec loop acc i =
    if i = String.length s
    then List.rev acc
    else loop (String.get s i :: acc) (i + 1)
  in loop [] 0

let implode (cs : char list) : string =
  String.init
    (List.length cs)
    (fun i -> List.nth cs i)

let implode_all (css : char list list) : string list =
  let rec loop acc css =
    match css with
    | [] -> List.rev acc
    | cs :: rest -> loop (implode cs :: acc) rest
  in loop [] css

let split_on_ws_helper (cs : char list) : char list list =
  let rec loop c acc cs = 
    match cs with
    | [] -> if c = [] then acc
      else acc @ [c]
    | d :: ds -> if is_ws d then
        if c = [] then loop [] acc ds
        else loop [] (acc @ [c]) ds
      else loop (c @ [d]) acc ds
  in loop [] [] cs

let split_on_ws (s : string) : string list =
  implode_all (split_on_ws_helper (explode s))


let rec eval stack prog =
  match prog with
  | [] -> stack
  | "+" :: xs -> (
      match stack with 
      | a :: b :: rest -> eval ((b + a) :: rest) xs
      | _ -> stack
    )
  | "-" :: xs -> (
      match stack with
      | a :: b :: rest -> eval ((b - a) :: rest) xs
      | _ -> stack
    )
  | "*" :: xs -> (
      match stack with
      | a :: b :: rest -> eval ((b * a) :: rest) xs
      | _ -> stack
    )
  | "/" :: xs -> (
      match stack with
      | a :: b :: rest -> eval ((b / a) :: rest) xs
      | _ -> stack
    )
  | "mod" :: xs -> (
      match stack with
      | a :: b :: rest -> eval ((b mod a) :: rest) xs
      | _ -> stack
    )
  | "sqrt" :: xs -> (
      match stack with
      | a :: b -> eval ((sqrt a) :: b) xs
      | _ -> stack
    )
  | "^" :: xs -> (
      match stack with
      | a :: b :: rest -> eval ((pow b a) :: rest) xs
      | _ -> stack
    )
  | n :: xs -> eval ((int_of_string n) :: stack) xs


let interp (input : string) : int =
  match eval [] (split_on_ws input) with
  | [output] -> output
  | _ -> failwith "whoops!"
