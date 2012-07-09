(*
open Int64;;

let big_eratosthenes int_number =
  let rec del_list x list list'=
    match list with
      | [] -> list'
      | hd::rest -> if (rem hd x) = zero then del_list x rest list' else del_list x rest (list'@[hd]) in
  let makelist num =
    let rec sub_makelist int64_num l =
      if int64_num = succ(one) then succ(one)::l else sub_makelist (sub int64_num one) (int64_num::l) in
      sub_makelist num [] in
  let rec sub_eratosthenes numlist primelist =
    match numlist with
      | [] -> primelist
      | x::rest -> let numlist' = (del_list x numlist []) in
	let primelist' = (primelist@[x]) in
	  if ((mul x x) > (List.hd (List.rev numlist))) then (primelist @ numlist)
	  else sub_eratosthenes numlist' primelist' in
    sub_eratosthenes (makelist int_number) [];;

let mynum = 600851475143L;;
let myprime = big_eratosthenes 1000L;;

exception Not_found;;

let rec find_max_prime num primelist =
  match primelist with
    | [] -> num
    | x::rest -> if (rem num x) = zero then find_max_prime (div num x) primelist else find_max_prime num rest;;

let rec sqrt n num =
  if (mul n n) > num then n else sqrt (succ n) num;;

let sqrtmynum = sqrt 20000L 600851475143L;;

let myprimelist = big_eratosthenes 40000L;;

let isprime num =
  let rec sub_isprime n num =
    if (rem num n) = zero then 1 else sub_isprime (succ n) num in
    sub_isprime one num;;

let rec div_list num list =
  match list with
    | [] -> 0
    | hd::rest -> if (rem num hd) = zero then 1 else div_list num rest;;


let  biggestprime num numlist =
  let rev_numlist = List.rev numlist in
  let rec sub_biggestprime num numlist =
    match numlist with
      | [] -> raise Not_found
      | hd::rest -> if ((rem num hd) = zero) then hd else sub_biggestprime num rest in
  sub_biggestprime num rev_numlist;;

biggestprime mynum myprimelist;;
big_eratosthenes 13195L;;

*)

(*2011/8/4 re_solve*)

(*2011/8/25 解き直し*)
open Int64;;

(*素数判定*)
let is_prime num =
  match num with
    | 1L -> false
    | 2L -> true
    | num-> if rem num 2L = 0L then false else
	let rec loop n =
	  if mul n n > num then true else 
	    if rem num n = 0L then false else loop (add n 2L)
	in loop 3L
	     
(*素因数分解*)
let prime_list =
  let rec loop m n l =
    if is_prime m then m::l else
      if is_prime n then
	if rem m n = 0L then loop (div m n) n (n::l)
	else loop m (add n 1L) l
      else loop m (add n 1L) l
  in loop 600851475143L 2L []

let prime_list2 =
  let rec loop m n l =
    if m < mul n n then m::l else
      if rem m n = 0L then loop (div m n) n (n::l)
      else loop m (add n 1L) l
  in loop 600851475143L 2L []

let _ = Printf.printf "%Ld\n" (List.hd prime_list2)


(*
let _ =
  let rec loop num sum =
    if num < 2000000L then
      if is_prime num then loop (add num 2L) (add sum num) else loop (add num 2L) sum
    else sum
  in loop 3L 2L;;
*)
