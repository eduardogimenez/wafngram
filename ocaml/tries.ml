module type Token = sig
    type t
    type symbol
    val length : t -> int
    val get : t -> int -> symbol
    val append : t -> symbol -> t
    val toString : t -> string
    val empty : t
  end
		      
module type Value = sig
    type d
    val sample    : d -> float -> d
    val singleton : float -> d
    val project   : d -> float
    val frequency : float -> float -> d
    val print     : out_channel -> d -> unit
  end

(*
module type Tries = sig
    type t
    val empty      : unit -> t
    val countToken : t -> Token.t -> Value.d
    val replace    : t -> Token.t -> Value.d -> unit
    val print      : out_channel -> (out_channel -> Value.d) -> string -> t -> unit
    val toList     : t -> string list
    val iter       : (string -> 'a -> unit) -> t -> unit
    val map        : (string -> Value.d -> Value.d) -> t -> t			 
    end
 *)
		      
module MakeTries (Value:Value)(Token:Token) = struct
		      
type t =
  | Node of (Token.symbol,Value.d*t) Hashtbl.t

let createTable () = ((Hashtbl.create ~random:false 50):(Token.symbol,Value.d*t) Hashtbl.t)
let create () = Node (createTable ())
let rec size (Node tbl) =
  Hashtbl.fold (fun _ (_,subnd) n -> 1+(size subnd)+n) tbl 0


let find (Node tbl) tk =
  let n = (Token. length tk) in
  let rec loop i tbl =
    if i=n-1
    then
      let (v,_) = Hashtbl.find tbl (Token.get tk i) in v
    else
      let (_,Node subtbl) = Hashtbl.find tbl (Token.get tk i)
      in loop (i+1) subtbl
  in loop 0 tbl

let mem t tk =
  try let _ = find t tk in true with Not_found -> false 
				      
(* Updates values asociated to a string in the tries *)
let countFrequency (Node tbl) str len v =
  let n = (Token.length str) in
  let rec loop i tbl =
    if i=n
    then ()
    else
      let nbngrms = (float_of_int len) -. (float_of_int i) in
      try
	let (v1,Node subtbl) = Hashtbl.find tbl (Token.get str i) in
	begin
	  Hashtbl.replace tbl (Token.get str i) ((Value.sample v1 (v /. nbngrms)),Node subtbl);
	  loop (i+1) subtbl
	end
      with Not_found ->
	let subtbl = createTable () in
	begin
	  Hashtbl.add tbl (Token.get str i) (Value.singleton (v /. nbngrms),Node subtbl);
	  loop (i+1) subtbl
	end
  in loop 0 tbl

(* Updates values asociated to a string in the tries *)
let count (Node tbl) str v =
  let n = (Token.length str) in
  let rec loop i tbl =
    if i=n
    then ()
    else
      try
	let (v1,Node subtbl) = Hashtbl.find tbl (Token.get str i) in
	begin
	  Hashtbl.replace tbl (Token.get str i) ((Value.sample v1 v),Node subtbl);
	  loop (i+1) subtbl
	end
      with Not_found ->
	let subtbl = createTable () in
	begin
	  Hashtbl.add tbl (Token.get str i) (Value.singleton v,Node subtbl);
	  loop (i+1) subtbl
	end
  in loop 0 tbl

let countExact (Node tbl) str v =
  let n = (Token.length str) in
  let rec loop i tbl =
    if i=n-1
    then
      let (v1,subn) = try Hashtbl.find tbl (Token.get str i) with Not_found -> (Value.singleton v,create ())
      in Hashtbl.replace tbl (Token.get str i) (Value.sample v1 v,subn)
    else
      try
	let (_,Node subtbl) = Hashtbl.find tbl (Token.get str i)
	in loop (i+1) subtbl
      with Not_found ->
	let subtbl = createTable () in
	begin
	  Hashtbl.add tbl (Token.get str i) (Value.singleton v,Node subtbl);
	  loop (i+1) subtbl
	end
  in loop 0 tbl
	  
let replace (Node tbl) str v =
  let n = (Token.length str) in
  let rec loop i tbl =
    if i=n-1
    then
      let (_,subn) = try Hashtbl.find tbl (Token.get str i) with Not_found -> (v,create ())
      in Hashtbl.replace tbl (Token.get str i) (v,subn)
    else
      try
	let (_,Node subtbl) = Hashtbl.find tbl (Token.get str i)
	in loop (i+1) subtbl
      with Not_found ->
	let subtbl = createTable () in
	begin
	  Hashtbl.add tbl (Token.get str i) (v,Node subtbl);
	  loop (i+1) subtbl
	end
  in loop 0 tbl

let print out sep (Node tbl) =
  let rec loop prefix tbl =
    Hashtbl.iter
      (fun c (v,Node subtbl) ->
       let nprefix = (Token.append prefix c) in
       begin
	 Printf.fprintf out "%s %s %a\n" (Token.toString nprefix) sep Value.print v;
	 loop nprefix subtbl
       end)
      tbl
  in loop Token.empty tbl
	  
let toList f (Node tbl) =
  let rec loop prefix tbl =
    Hashtbl.fold
      (fun c (v,Node subtbl) l ->
       let nprefix = (Token.append prefix c)
       in List.concat[(f nprefix,v)::(loop nprefix subtbl);l])
      tbl
      []
  in loop Token.empty tbl
	 
let iter f (Node tbl) = 
  let rec loop prefix tbl =
    Hashtbl.iter
      (fun c (v,Node subtbl) ->
       let nprefix = (Token.append prefix c) in
       begin
	 f nprefix v;
	 loop nprefix subtbl
       end)
      tbl
  in loop Token.empty tbl

let map f (Node tbl) =
  let rec loop prefix tbl =
    Hashtbl.iter
      (fun c (v,Node subtbl) ->
       let nprefix = (Token.append prefix c) in
       begin
	 Hashtbl.replace tbl c ((f nprefix v),Node subtbl);
	 loop nprefix subtbl;
       end)
      tbl
  in
  begin
    loop Token.empty tbl;
    Node tbl
  end

(* f1 composes horizontally the deeper computations, f2 computes in depth *)
let fold (f1:'a->'a->'a) (f2:Token.t -> Value.d -> 'a -> 'a) (Node tbl) x =
  let rec loop prefix tbl =
    Hashtbl.fold
      (fun c (subv,Node subtbl) r ->
       let nprefix = (Token.append prefix c)
       in f1 r (f2 nprefix subv (loop nprefix subtbl)))
      tbl x
  in loop Token.empty tbl

(* A fresh tries structure where all the tokens occurring in a given tries 
   are associated to the default real 0.0 *)
let setToZero t =
  let nt = create () in
  begin
    iter (fun tk _ -> replace nt tk (Value.singleton 0.0)) t;
    nt
  end

let depth n (Node tbl) =
  let rec loop m tbl =
    if m=n
    then Hashtbl.fold (fun _ (d,Node subtbl) r ->  (Value.project d) +. r) tbl 0.0
    else Hashtbl.fold (fun _ (_,Node subtbl) r ->  (loop (m+1) subtbl) +. r) tbl 0.0
  in loop 0 tbl


let ratio (Node tbl) =
  let len = Hashtbl.fold (fun _ (d,Node subtbl) r ->  (Value.project d) +. r) tbl 0.0 in
  let rec loop m tbl =
    let nbngrms = len -. m
    in  Hashtbl.iter
	  (fun tk (d,Node subtbl) ->
	   begin
	     Hashtbl.replace tbl tk ((Value.frequency (Value.project d) nbngrms),Node subtbl);
	     loop (m +. 1.0) subtbl
	   end) tbl
  in loop 0.0 tbl

end

						(*
type token =
  | String of string
  | Number
		     
module Tries =
  MakeTries
    (struct
      type d = float
      let sample = (fun x y -> x +. y)
      let singleton x = x
      let project  x = x
      let frequency = fun x y -> x /. y
      let print out d = Printf.fprintf out "%f" d
    end)
    (struct 
      type t = token
      type symbol = char
      let length tk =
	match tk with
	| String str -> String.length str
	| Number -> 1
      let get tk i = 
	match tk with
	| String str -> (String.get str i)
	| Number -> 'N' (* Números representados con N mayúscula, los strings son en minúscula. *)
      let append tk c = 
	match tk with
	| String str -> String (str^(String.make 1 c))
	| Number -> raise (Invalid_argument "append:Number")
      let toString tk = 
	match tk with
	| String str -> str
	| Number -> "N"
      let empty = String ""
    end);;

open Tries;;

let t = (create ());;
  
  count t (String "abc") 1.0;; (* abcab *)
    count t (String "bca") 1.0;;
      count t (String "cab") 1.0;;
	count t (String  "ab") 1.0;;
	  count t (String "b") 1.0;;
	      print stdout "->" t;;
	      print stdout "->" t;;
		toList (fun x -> x) t;;
		  size t;;
		    depth 3 t;;
		      ratio t;;
			print stdout "->" t;;

let t1 = (create ());;
  
  countFrequency t1 (String "abc") 5 1.0;; (* abcab *)
    countFrequency t1 (String "bca") 5 1.0;;
      countFrequency t1  (String "cab") 5 1.0;;
	countFrequency t1  (String  "ab") 5 1.0;;
	  countFrequency t1  (String "b") 5 1.0;;
	      print stdout "->" t1;;
		*)
	     
	  
						 
