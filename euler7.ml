(*
(*エラトステネスの篩*)
let eratosthenes number =
  let rec del_list x list list'=
    match list with
      | [] -> list'
      | hd::rest -> if (hd mod x) = 0 then del_list x rest list' else del_list x rest (list'@[hd]) in
  let makelist odd_num =
    let rec sub_makelist odd_num l = 
      match odd_num with
	| 1 -> 2::l
	| odd_num -> sub_makelist (odd_num-2) (odd_num::l) in
    sub_makelist odd_num [] in
  let rec sub_eratosthenes numlist primelist =
    match numlist with
      | [] -> primelist
      | x::rest -> let numlist' = (del_list x numlist []) in
		   let primelist' = (primelist@[x]) in
		   if x * x > (List.hd (List.rev numlist)) then (primelist @ numlist)
		   else sub_eratosthenes numlist' primelist' in
  sub_eratosthenes (makelist number) [];;

(*素数判定*)
let isprime odd_n = 
  let rec sub_isprime n m =
    match (n, m) with
      | (n, 1) -> true 
      | (n, m) -> if (n mod m) = 0 then false else sub_isprime n (m-2) in 
    sub_isprime odd_n (odd_n-2) ;;

(*30001までの素数のリストを生成*)
let primelist = eratosthenes 30001;;
(*リストの長さ番目の素数を得る*)
let primelistlength = List.length primelist;;
List.hd (List.rev primelist);;

(*素数を見つけてカウントする*)
let rec findprime count max num =
  if isprime num then 
    if (count = max) then num else findprime (count+1) max (num+2)
  else findprime count max (num+2);;

(*2番目の素数が3なので、そこから始めて10001番目の素数を見つける*)
findprime 2 10001 2;;

*)

(*8/4 re_solve*)
(*素数判定*)
let is_prime num =
  let rec loop n =
    if n*n > num then true else 
      if num mod n = 0 then false else loop (n+1)
  in loop 2

(*奇数を素数判定して、素数の数をカウント*)
let _ =
  let rec loop n counter =
    if counter = 10000 then 
      if is_prime n then Printf.printf "%d\n" n else loop (n+2) counter 
    else
      if is_prime n then loop (n+2) (counter+1) else loop (n+2) counter
  in loop 3 1;;

