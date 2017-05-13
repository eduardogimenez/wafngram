let empty l =
  match l with
  | [] -> true
  | _ ->  false

let tl l =
  match l with
  | [] -> []
  | _::l1 -> l1

let splitAtNth n l1 =
  let rec loop n l1 l2 =
    if n=0
    then (List.rev (tl l2),l1)
    else 
      match l1 with
    | []    -> (List.rev l2,l1)
    |  h::t -> loop (n-1) t (h::l2)
  in loop n l1 []
	  
let removeSegment n m l =
  let (pre,mid) = splitAtNth (n+1) l in
  let (_, post) = splitAtNth (m-n) mid
  in (pre, post)

let getSegment n m l =
  let (pre,mid) = splitAtNth  n l in
  let (seg,_)   = splitAtNth (m-n+2) mid
  in seg

       
exception IndexNotFound of string
let index x l =
  let rec loop l n = 
    match l with
    | [] -> raise (IndexNotFound x)
    | y::l1 -> if x=y then n else loop l1 (n+1) 
  in loop l 0

let depth x l =
  try index x l
  with IndexNotFound _ -> List.length l

let order x y = if x>y then 1 else if x=y then 0 else -1
let sort l = List.sort order l

let rank f g l =
  match l with
  | [] -> []
  | x::l1 ->
     let rec loop n x l =
       match l with
       | [] -> []
       | y::l1 -> if (f x)=(f y) then (g y n)::(loop n x l1) else (g y (n+1))::(loop (n+1) y l1)
     in (g x 0)::(loop 0 x l1)

let rec witness p l =
  match l with
  | [] -> None
  | x::l1 -> if p x then Some x else witness p l1

(* p1 means fatal error, p2 is a counting predicate *)
let atLeast n p1 p2 l =
  let rec loop m l =
    if m=n then true
    else
      match l with
      | [] -> false
      | x::l1 -> if p1 then true else if p2 x then loop (m+1) l1 else loop m l1
  in loop 0 l

(* Elige al azar aproximadamente 1/(n+1) % de los elementos de la lista que verifican p. *)
let randomPartition n p l =
  if n=0 then (List.filter p l, [])
  else
    begin
      Random.self_init ();
      List.partition (fun x -> (Random.int n)> 0) (List.filter p l)
    end

let listDistance l1 l2 =
  let rec loop l n = 
    match l with
    | [] -> 0
    | ngram::l3 -> ((depth ngram l2)-n)+(loop l3 (n+1)) 
  in loop l1 0
	  

      
