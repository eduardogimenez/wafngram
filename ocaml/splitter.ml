open Arff
open Listlib

exception Unsplittable of int*string
exception IllTyped of int*Arff.value
exception MoreAttributesThanExpected of string
       
let getIdName defsym s =
  match Str.bounded_split (Str.regexp defsym) s 2 with
  | [def;den] -> def
  | _         -> raise (Unsplittable (2,("in getIdName"^s)))

let rec fillList a n = if n=0 then [] else a::(fillList a (n-1))
		       
let rec getIdValues defsep defsym attrl strl =
  match (attrl,strl) with
  | [], [] -> [String ""]
  | attr::attrl1, s::strl1 -> 
     (match Str.bounded_split (Str.regexp defsym) s 2 with
      | [def;den] ->
	 if def=attr
	 then (String den)::(getIdValues defsep defsym attrl1 strl1)
	 else (String  "")::(getIdValues defsep defsym attrl1 strl)
      | _         -> raise (Unsplittable (2,("in geIdValues: "^s))))
  | _::_, [] -> fillList (String "") ((List.length attrl)+1) 
  | [], _::_ -> [String (String.concat defsep strl)]
		      	       
let rec postProcessing defsep strl =
  match strl with
  | [] -> []
  | [x] -> [x]
  | x1::(x2::l) ->
     if (String.get x1 ((String.length x1)-1))='\\'
     then postProcessing defsep ((String.concat defsep [x1;x2])::l)
     else x1::(postProcessing defsep (x2::l))
		       
let splitAndReplace defsep defsym attrl s =
  let strl1 = Str.split (Str.regexp defsep) s in
  let strl2 =  postProcessing defsep strl1
  in  getIdValues defsep defsym attrl strl2

  (*
  let len  = List.length attrl in
  let strl =
    if (List.length strl)=len
    then strl
    else let strl2 = postProcessing defsep strl in 
	 if (List.length strl2)=len
	 then strl2
	 else raise (Unsplittable (len,("in splitAndReplace: "^s)))
  in getIdValues defsep defsym attrl strl
   *)
  
(* Modifying the string fields of the HTTP request specified in lpos by their relative positions. *)
let splitStringField i defsep defsym attrl inst =
  let len = Array.length inst in
  if i<=0 || i>=len
  then inst
  else 
    match inst.(i) with
    | String str ->
	 (Array.concat
	    [if i>0 then (Array.sub inst 0 i) else [||];
	     (Array.of_list (splitAndReplace defsep defsym attrl str));
	     if i+1<len then (Array.sub inst (i+1) (len-i-1)) else [||]
	    ])
    | x -> raise (IllTyped (i,x))
	 

(* Splits a string field and adds the resulting values as aditional attributes *)
let split file defsep defsym pos attrl =
  try
    {header =
       {relname=file.header.relname;
	attributes=
	  (let completedattrs = List.concat [attrl;["Remaining"]] in
	   let newattrbs = List.map (fun id -> (id,StringType)) completedattrs in 
	   let (prev,post) = splitAtNth (pos+1) file.header.attributes []
	   in List.concat [prev;newattrbs;post]);
	constants=file.header.constants};
     data   =
       match file.data with
       | Values l -> Values (List.map (splitStringField pos defsep defsym attrl) l)
       | x -> x}
  with Unsplittable (n,x) ->
       begin
	 Printf.fprintf stderr "This is unsplittable in %d parts: %s\n" n x;
	 file
       end
      |IllTyped (i,v) ->
	begin
	  Printf.fprintf stderr "Field %i should be of type String, but has a value of %a" i Arff.valuePrintf v;
	  file
	end
