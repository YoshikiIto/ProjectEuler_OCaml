let sum_of_square num max =
  let rec in_sum_of_square num max ans =
    if num <= max then in_sum_of_square (num+1) max ((num*num) + ans) else ans in
    in_sum_of_square num max 0;;

let square_of_sum start max = 
  let sum = (start + max) * (max/2) in
    sum * sum;;

let sum = sum_of_square 1 100;;
let square = square_of_sum 1 100;;

let answer = square - sum;;
