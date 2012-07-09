(*Pythagorean tripletならtrue*)
let ispy a b c = if a*a + b*b = c*c then true else false

(*(1,2,9997)から順にチェックしていく*)
let _ =
  let rec loop a b =
    if a>333 then (0,0,0) else
      if b>(1000-a-b) then loop (a+1) (a+2) else 
	if ispy a b (1000-a-b) then (a,b,(1000-a-b)) else loop a (b+1)
  in (fun (x,y,z) -> print_int (x*y*z)) (loop 1 2)
