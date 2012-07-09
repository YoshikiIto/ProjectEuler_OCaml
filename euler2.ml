(*n番目のフィボナッチ数*)
let fib n =
  let rec loop n x y =
    if n = 1 then x+y else loop (n-1) (x+y) x
  in loop n 1 0

(*3n-1番目のフィボナッチ数が偶数なので、合計を求める*)
let _ =
  let rec loop n sum =
    let fibn = fib n in
      if fibn > 4000000 then sum
      else loop (n+3) (fibn+sum)
  in print_int (loop 2 0)
