let int_to_list x =
  let rec sub_int_to_list x l =
    if x < 10 then x :: l
    else sub_int_to_list (x/10) ((x mod 10)::l)
  in sub_int_to_list x []

let sum_of_list list =
  let rec sub list ans =
    match list with
      | [] -> ans
      | hd::rest -> sub rest (ans+hd)
  in sub list 0

let fact n =
  let rec fact' n m =
    if n > 1 then fact' (n-1) (m*n) else m in
    fact' n 1

let list_to_int list =
  let rec loop list ans =
    match list with
      | [] -> ans
      | x::rest -> loop rest (ans*10 + x)
  in loop list 0

(*
let list_to_bigint list =
  let rec loop list ans =	
	match list with
	  | [] -> ans
      | x::rest -> loop rest (ans ^ (string_of_big_int x))
  in big_int_of_string (loop list "")
*)

let largechar_to_int char = (Char.code char) - 64

let smallchar_to_int char = (Char.code char) - 96

let string_to_char_list string =
  let len = String.length string in
  let rec loop n m string list =
    if n < m then loop (n+1) m string ((String.get string n)::list) else List.rev list in
  loop 0 len string []

(*
let make_array_to n =
  Array.init n (fun x -> x+1);;

let del_mul_of_n a i max =
  if a.(i) > 0 then
    let rec loop j =
      if j > max then ()
      else (a.(j) <- 0; loop (j+i))
    in loop (i*2);;

let rec sub_seive_array a i max =
  if i > max then ()
  else (del_mul_of_n a i max; sub_seive_array a (i+1) max);;

let seive_array max =
  let a = Array.init max (fun x -> x) in
    sub_seive_array a 2 (sqrt_int max) ; a;;
*)
let get_nonzero_list_from_array array =
  let make_nonzero_list l n = if (n != 0) then (n::l) else l in
    List.rev (Array.fold_left make_nonzero_list [] array);;


let seive_array range =
  let seive a i upper =
    if a.(i) > 0 then
      let rec loop j =
	if j > upper then ()
	else (a.(j) <- 0; loop (j+i))
      in loop (i*2) in
  let rec loop a i upper limit =
    if i > upper then ()
    else (seive a i limit; loop a (i+1) upper limit) in
  let sqrt_int x = int_of_float (sqrt (float_of_int x) +. 0.5) in
  let a = Array.init range (fun x -> x) in
    loop a 2 (sqrt_int range) (range-1); a

let seive_list n =
 (List.tl (get_nonzero_list_from_array (seive_array n)))

let factorization n =
  let rec loop n i l =
    if n <= i then n::l else 
      if n mod i = 0 then loop (n/i) i (i::l) else loop n (i+1) l
  in loop n 2 []
