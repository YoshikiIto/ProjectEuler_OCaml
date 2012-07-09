(*nがlistの全ての要素で割れるかチェック*)
let rec candivlist n list =
  match list with
    | [] -> true
    | hd::rest -> if n mod hd = 0 then candivlist n rest else false;;

(*20から11までのリスト*)
let twenty_to_eleven = [20;19;18;17;16;15;14;13;12;11];;

(*nが上記リストで割れるかチェックし、割れなかったらn+20をチェック。最初に見つかった答えを返す*)
let rec find_answer n =
  if candivlist n twenty_to_eleven then n else find_answer (n+20);;
