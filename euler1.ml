(*3か5の倍数を1000まで足していく*)
let _ =
  let rec loop n ans =
    if n < 1000 then
      if n mod 3 = 0 || n mod 5 = 0 then loop (n+1) (ans+n) else loop (n+1) ans
    else print_int ans
  in loop 2 0
