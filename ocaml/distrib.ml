type t = {
  (* Mean *)
  mean  : float;
  (* sigmanum = sigma^2/(num-2) *)
  sigmanum : float;
  (* minimum value sampled *)
  minval : float;
  (* maximum value sampled *)
  maxval : float;
  (* Amount of sampled values *)
  num   : int;
  (* Actually sampled values *)
  histogram : (float,int) Hashtbl.t 
}

let singleton v = {
  mean     = v;
  sigmanum = 0.0;
  minval   = v;
  maxval   = v;
  num      = 1;
  histogram = begin let tbl = (Hashtbl.create ~random:false 10) in Hashtbl.add tbl v 1; tbl end
};;
(*
let sample d sample =
  let delta  = sample -. d.mean in
  let newmean = d.mean +. (delta /. (float_of_int d.num))
  in {
    mean     = newmean;
    sigmanum = d.sigmanum +. delta *. (sample -. newmean);
    num      = d.num+1
  };;
 *)

(* Wlford's algorithm for computing the mean and std dev online 
   See https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm *)
let sample d sample =
  let n       = d.num+1 in
  let delta   = sample -. d.mean in
  let newmean = d.mean +. (delta /. (float_of_int n)) in
  let m2      = d.sigmanum +. delta *. (sample -. newmean) in
  begin
    let i = try Hashtbl.find d.histogram sample with Not_found -> 0 in Hashtbl.replace d.histogram sample (i+1); 
    {
    mean     = newmean;
    sigmanum = m2;
    minval   = if sample < d.minval then sample else d.minval;
    maxval   = if sample > d.maxval then sample else d.maxval;
    num      = n;
    histogram = d.histogram
  }
  end
    
(* Un invento porque no tenemos la varianza de los conteos en las prior. 
   Está hecho para que no quede varianza cero, que fuerza un error fatal: es decir que esa tiene que ser la media y no otra. *)
let computeFrequency v n =
  let r = (v  /. n) in  {
  mean = r;
  sigmanum = (n -. 1.0) *. ((r /. 3.0) ** 2.0);
  minval = 0.0;
  maxval = 2.0 *. r;
  num = truncate n;
  histogram = Hashtbl.create ~random:false 1
  }
       
let stdDev d =
  if d.num <= 1 then 0.0 else sqrt (d.sigmanum /. (float_of_int (d.num-1)))

let meanDistance d1 d2 = d1.mean -. d2.mean

let outlier d1 d2 =
  not (d1.minval <= d2.mean && d2.mean <= d1.maxval)
  
(*  (abs_float (meanDistance d1 d2)) > 3.0 *. (stdDev d1) *)
   

let oneStandardDevAway d1 d2 =
  abs_float (meanDistance d1 d2) < (stdDev d1)
					      
let kurtosis m dev = (m ** 4.0 +. 6.0 *. (m ** 2.0) *. (dev ** 2.0) +. 3.0 *. (dev ** 4.0))

let round x n =
  let s = (10.0 ** (float_of_int (n-1))) in
  let y = x *. s in
  let (r1,r2) = modf y in
  let r3 = if r1 < 0.5 then r2 else r2 +. 1.0
  in r3 /. s

let printHistogram out d =
  Hashtbl.iter (fun r n -> Printf.fprintf out "%.3f\t" (round r 3)) d.histogram
		 

