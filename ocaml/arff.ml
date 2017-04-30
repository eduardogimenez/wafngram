open Printf
open Str
       
type attributeType =
    NumberType
  | IntegerType
  | FloatType
  | DateType
  | StringType
  | EnumeratedType of string list

type value =
    Missing
  | Number of int
  | Integer of int
  | Float of float
  | Date of string
  | String of string
  | Enumerated of string

type instanceType =
    Ocurrences of ((int*value) list) list
  | Values of (value array) list


type headerType = {
  relname: string;
  attributes: (string*attributeType) list;
  constants: (string, string) Hashtbl.t 
}

let emptyHeader = {
  relname= "";
  attributes=[];
  constants=Hashtbl.create ~random:false 0;
}


type t = {
  header: headerType;
  data : instanceType
}
	   
let typePrintf file t =
  match t with
    NumberType -> fprintf file "numeric" 
  | IntegerType -> fprintf file "integer"
  | FloatType   -> fprintf file "float"
  | DateType    -> fprintf file "date"
  | StringType  -> fprintf file "string"
  | EnumeratedType l -> fprintf file "{%s}" (String.concat "," l)

let uudecoded = ref false
let escape str =
  if !uudecoded
  then (Str.global_replace (Str.regexp "\\(\n\\|\r\\)") "\\n"
       (Str.global_replace (Str.regexp "'") "\\'" str)) else str
				
let valuePrintf file v =
  match v with
    Missing        -> fprintf file "?"
  | Number d       -> fprintf file "%d" d
  | Integer i      -> fprintf file "%i" i
  | Float r        -> fprintf file "%f" r
  | Date s         -> fprintf file "%s"   (escape s)
  | String s       -> fprintf file "'%s'" (escape s)
  | Enumerated s   -> fprintf file "%s"   (escape s)

let rec printSeparatedList file p l =
  match l with
    [] -> ()
  | [x] -> fprintf file "%a" p x
  | x::y::l1 ->
     begin
       fprintf file "%a," p x;
       printSeparatedList file p (y::l1)
     end
	       
let printOcurrenceInstance file p l =
  begin
    fprintf file "{";
    printSeparatedList file p l;
    fprintf file "}//%d\n" (List.length l);
    flush file;
  end
       
let printValuesInstance file inst =
  Arraylib.printSeparatedArray file valuePrintf inst

	 
let printf file a =
  begin
    fprintf file "@relation '%s'\n" a.header.relname;
    (List.iter (fun (n,t) -> fprintf file "@attribute '%s' %a\n" (escape n) typePrintf t) a.header.attributes);
    (fprintf file "@data\n");
    match a.data with
      Ocurrences l ->
      List.iter (printOcurrenceInstance file (fun file (at,oc) -> fprintf file "%i %a" at valuePrintf oc)) l
    | Values l ->
       List.iter (Arraylib.printSeparatedArray file valuePrintf) l
  end

exception BadLength of int

let numberOfInstances file =
  match file.data with
  | Values l -> List.length l
  | Ocurrences l -> List.length l

let wellFormed file =
  match file.data with
  | Values []  -> false
  | Values (x::l) ->
     let n = Array.length x in
     begin
       List.iteri (fun i inst -> if (Array.length inst)<>n then raise (BadLength i)) (x::l);
       true
     end
  | _ -> true
    
(*
let typeOfConstants atrs =
  List.fold_right
    (fun (atr,ty) l ->
     match ty with
     | Enumerated lv -> List.concat [List.map (fun c -> (c,ty)) lv;l]
     | _ -> l
     end)
    atrs

let tableOfTypes lcty =
  let tbl = Hastbl.create (List.length lcty)
  in  List.iter (fun (c,ty) l -> Hashtbl.add tbl c ty) lcty
 *)

let splitData n p file = 
  match file.data with
  | Ocurrences l ->
     let (l1,l2) = Listlib.randomPartition n (fun _ -> true) l
     in  ({header = file.header;data = Ocurrences l1},
	  {header = file.header;data = Ocurrences l2})
  | Values l ->
     let (l1,l2) = Listlib.randomPartition n p l
     in  ({header = file.header;data = Values l1},
	  {header = file.header;data = Values l2})

let empty hd = {
  header =hd;
  data   = Values []
}

exception ShouldBeValuesData
let getValuesAsArray file =  
  match file.data with
  | Values l -> Array.of_list l
  | _ -> raise ShouldBeValuesData


let printDistribution file out n =
  begin
    List.iter (fun r -> Printf.fprintf out "%f\n" r) [0.1;0.2;0.3;0.4;0.5;0.6;0.7;0.8;0.9;1.0];
    Printf.fprintf out "\n";
    (match file.data with
    | Values l -> raise (Invalid_argument "printDistribution: shall be in ocurrences format")
    | Ocurrences l ->
       List.iter
	 (fun l -> try Printf.fprintf out "%a\n" valuePrintf (List.assoc n l) with Not_found -> ())
	 l)
  end
