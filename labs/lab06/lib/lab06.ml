
let map (f : 'a -> 'b) (l : 'a list) : 'a list =
  let rec loop acc l = 
    match l with
    | [] -> List.rev acc
    | x :: xs -> loop (f x :: acc) xs
  in loop [] l

let filter (f : 'a -> bool) (l : 'a list) : 'a list =
  let rec loop acc l = 
    match l with
    | [] -> List.rev acc
    | x :: xs -> 
      if f x then
        loop (x :: acc) xs
      else loop acc xs
    in loop [] l

let fold_left
    (f : 'acc -> 'a -> 'acc)
    (base : 'acc)
    (l : 'a list) : 'acc =
  let rec loop acc l = 
    match l with
    | [] -> acc
    | x :: xs -> loop (f acc x) xs
  in loop base l

let fold_right
    (f : 'a -> 'acc -> 'acc)
    (l : 'a list)
    (base : 'acc) : 'acc =
  let rec loop l acc =
    match l with 
    | [] -> acc
    | x :: xs -> loop (f x acc) xs
  in loop l base


let init n (f: int -> 'a) : 'a list =
  let rec loop acc n = 
    if n < 0 then acc
    else loop (f n :: acc) (n - 1)
  in loop [] (n - 1)


module Matrix = struct
  type 'a t = 'a list list

  let init (dim : int * int) (f : int -> int -> 'a) : 'a t =
    let (r, c) = dim in
    let rows = init r (fun r -> init c (fun c -> f r c))

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    map (fun r: 'a list -> map f r) m

  let fold_left
      (f : 'acc -> 'a -> 'acc)
      (base : 'acc)
      (m : 'a t) : 'acc =
    
    fold_left (fun r_acc r -> fold_left f r_acc r) base m

end
