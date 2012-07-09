(*
let isprime odd_n = 
  let rec sub_isprime n m = 
    if m mod 2 = 0 then sub_isprime n (m-1) else 
      match (n, m) with
	| (n, 1) -> true 
	| (n, m) -> if (n mod m) = 0 then false else sub_isprime n (m-2) in 
    sub_isprime odd_n (int_of_float (sqrt (float_of_int odd_n)));;

let rec sum_of_primes n m max=
  if n < max then 
    if isprime n then sum_of_primes (n+2) (Int64.add (Int64.of_int n) m) max else sum_of_primes (n+2) m max
  else m;;
(*
let rec make_primelist n l max =
  if n < max then
    if isprime n then make_primelist (n+2) (n::l) max else make_primelist (n+2) l max
  else l;;

let rec makeoddlist n m l =
  if n <= m then makeoddlist (n+2) m (n::l) else l;;

List.filter isprime (makeoddlist 3 1999999 []);;
*)

let _ = Printf.printf "%Ld\n" (sum_of_primes 3 2L 2000000);;
*)

(*2011/8/5 re_solve*)
open Int64;;

(*素数判定*)
let is_prime num =
  let rec loop n =
    if mul n n > num then true else 
      if rem num n = 0L then false else loop (add n 1L)
  in loop 2L

let _ =
  let rec loop num sum =
    if num < 2000000L then
      if is_prime num then loop (add num 2L) (add sum num) else loop (add num 2L) sum
    else Printf.printf "%Ld\n" sum
  in loop 3L 2L;;
