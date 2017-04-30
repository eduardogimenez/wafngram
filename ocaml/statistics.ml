let mean l =
  (List.fold_right (fun n m -> n+.m) l 0.0) /. (float_of_int (List.length l))

let stdDev mu l =
  sqrt ((List.fold_right (fun n m -> (n -. mu)**2.0 +. m) l 0.0)/.((float_of_int (List.length l))-. 1.0))

(* let compute a =
  let n = ref 0 in
  let mean = ref 0.0 in
  let m2 = ref 0.0 in
  begin
  for i=0 to (Array.length a)-1 do
    n:= !n+1;
    let delta = a.(i) -. !mean in
    begin
    mean := !mean +. (delta /. (float_of_int !n));
    m2 := !m2 +. delta *. (a.(i) -. !mean)
    end
  done;
  (!mean,sqrt (!m2 /. (float_of_int (!n-1))))
  end;;
let x = [1;2;3;4;4;4;4;4;5;6;7];;
let xa = Array.map float_of_int x;;
let xl = [ 1.;2.;3.;4.;4.;4.;4.;4.;5.;6.];;
let mean2 l r = List.fold_right (fun d f -> sample f d) l (singleton r);;
 *)
