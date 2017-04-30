open Printf

exception ShouldHaveSameLength
let iter2 f a1 a2 =
  if Array.length a1 = Array.length a2
  then
    for i=0 to (Array.length a1)-1 do f a1.(i) a2.(i) done
  else raise ShouldHaveSameLength

let printSeparatedArray file p l =
  try
  let n = Array.length l
  in if n=0 then ()
     else if n=1 then fprintf file "%a\n" p l.(0)
     else
       begin
	 for i=0 to n-2 do fprintf file "%a," p l.(i) done;
	 fprintf file "%a\n" p l.(n-1)
       end
  with Invalid_argument _ -> raise (Invalid_argument "printSeparatedArray")
