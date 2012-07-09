(*intをint_listに　123 -> [1;2;3]*)
let rec int_to_list x l =
  if x < 10 then x :: l
  else int_to_list (x/10) ((x mod 10)::l)

(*nが回文数かどうか判定*)
let is_parindromic n = 
  let nlist = int_to_list n [] in
    (nlist = List.rev nlist)

(*999*999~901*901までのリストを作成し、ソート*)
let mul_list  =
  let rec sub_makelist n m l =
    if n>900 then (if  m>900 then sub_makelist n (m-1) ((n*m)::l) else sub_makelist (n-1) (n-1) l) else l in
    List.rev (List.sort Pervasives.compare (sub_makelist 999 999 []))

(*リスト内で最大の回文数を探索*)
let _ =
  let rec get_palindromic list =
    match list with
      | [] -> 0
      | nm :: rest -> if is_parindromic nm then nm else get_palindromic rest
  in print_int (get_palindromic mul_list)
