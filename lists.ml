(* Problem 1 *)
let rec last x =
  match x with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last rest


(* Problem 2 *)
let rec last_2 = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: rest -> last_2 rest


(* Problem 3 *)
let rec at (i: int) = function
  | [] -> None 
  | h::t -> if i = 0 then Some h else at (i - 1) t 


(* Problem 4 *)
let rec length x = 
  let rec aux n = function
    | [] -> n 
    | _::t -> aux (n + 1) t
  in 
  aux 0 x


(* Problem 5 *)
let rev list = 
  let rec aux acc = function 
    | [] -> acc
    | h :: t -> aux (h :: acc) t 
  in aux [] list


(* Problem 6 *)
let is_palindrome list = 
  list = rev list
