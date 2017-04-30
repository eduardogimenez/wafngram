open Arff
open Distrib
open Listlib

let length = ref 3

(* The type of ngrams. *)
type t =
  | String of string
  | Equal  of char
  | Number

exception StopIter
let forAllChar p str =
  try
    String.iter (fun c -> if p c then raise StopIter) str;
    true
  with StopIter -> false

let isNumber str =
  forAllChar (fun c -> let n = (Char.code c) in 48>n || n>57) str

let isEqual str =
  let x = String.get str 0 in forAllChar (fun c -> c <> x) str
	     
let abstract str =
  if isNumber str then Number
  else if isEqual str then Equal (String.get str 0)
  else String str

let escape str = str (* Str.global_replace (Str.regexp "'") "\\'" str*)
let labelOfCategory ord fld cat =
  let ord = string_of_int ord in
  match cat with
  | String str -> String.concat "-" [(*ord;*)fld;"S";escape str]
  | Equal  c   -> String.concat "-" [(*ord;*)fld;"E";String.make 1 c]
  | Number     -> String.concat "-" [(*ord;*)fld;"N"]
let stringOfCategory cat =
  match cat with
  | String str -> String.concat "-" ["S";escape str]
  | Equal  c   -> String.concat "-" ["E";String.make 1 c]
  | Number     -> String.concat "-" ["N"]
	      
type localEntry = int
		   
(* Un ordinal que sirve como identificador global del atributo y su valor asociado. *)
type 'a attribute = int*'a
let max = ref 0
let makeAttribute v =
  begin
    max:=!max+1;
    (!max,v)
  end

type fieldModel = (t,(Distrib.t attribute)) Hashtbl. t
		      
type fieldAttributes = {
  length      : Distrib.t attribute;
  occurrences : fieldModel
}

type field = {
  (* El ordinal del campo en el HTTP request *)
  position : int;
  (* Eventualmente el nombre del registro en el formulario dentro de un campo. *)
  subfield : string option
}
let getSubfield fld =
  match fld with
  | None -> ""
  | Some s -> s
let printField out fld =
  Printf.fprintf out "%d-%s" fld.position (getSubfield fld.subfield)

type model = (field, fieldAttributes) Hashtbl.t

let emptySmallModel () = (Hashtbl.create ~random:false 500)
let emptyBigModel   () = (Hashtbl.create ~random:false 50000)			      
	      
let countNgram fldmdl ngrm v =
  try
    let (ord,occ) = Hashtbl.find fldmdl ngrm
    in Hashtbl.replace fldmdl ngrm (ord,Distrib.sample occ v)
  with Not_found -> Hashtbl.add fldmdl ngrm (makeAttribute (Distrib.singleton v))

let countOcurrence fldmdl ngrm =
  try
    let occ = Hashtbl.find fldmdl ngrm
    in Hashtbl.replace fldmdl ngrm (occ+1)
  with Not_found -> Hashtbl.add fldmdl ngrm 1


(* Transforms a temporal field model associating ocurrences into a field model
   associating the place in the ngram rank of most used. *) 
let buildFieldModel fldmdl =
  let sortedl = 
    List.sort
      (fun (_,n) (_,m) -> if n<m then 1 else if n=m then 0 else -1)
      (Hashtbl.fold (fun ngram occ l -> (ngram,occ)::l) fldmdl []) in
  let rankedl = Listlib.rank (fun (_,n) -> n) (fun (ngrm,_) i -> (ngrm,i)) sortedl
  in List.iter (fun (ngrm,i) -> Hashtbl.replace fldmdl ngrm i) rankedl
 
				
let singleFieldExtraction fld str mdl =
  let len    = String.length str in
  let {length=(ord,d);occurrences=fldmdl} =
    try  Hashtbl.find mdl fld
    with Not_found -> {
      length     = makeAttribute (Distrib.singleton (float_of_int len));
      occurrences= Hashtbl.create ~random:false (len - !length)}
  in
  begin
    let count = Hashtbl.create ~random:false (len - !length) in
    for i = 0 to (len - !length) do
      let ngram = try String.sub str i !length with Invalid_argument _ -> str
      in countOcurrence count (abstract ngram)  
    done;
    buildFieldModel count;
    Hashtbl.iter (fun ngram occ -> countNgram fldmdl ngram (float_of_int occ)) count;
    Hashtbl.replace mdl fld {length=(ord,Distrib.sample d (float_of_int len));occurrences=fldmdl}
  end
    
(********************************************************************************************)
(* Replacement algorithm.                                                                   *)
(********************************************************************************************)
	      
(********************************************************************************************)
(* Side effects.                                                                                 *)
(* An array specifying for each index attribute whether it is a monolitic field 
   or whether it is made of subfields separated by defSep string and asigned by 
   defSym string. *)
let subfields : ((string*string) option) array = Array.make 100 None
(* Abstracts away subfield identifiers when set. *)
(********************************************************************************************)
		    
(* Extracts the attributes from a string which is an id=value subfield.*)
exception CouldNotSplitField of string
let subfieldDefExtraction defsym i s mdl =
  match Str.bounded_split (Str.regexp defsym) s 2 with
  | [def;den] -> singleFieldExtraction {position=i;subfield=Some def} den mdl 
  | [den]      -> singleFieldExtraction {position=i;subfield=None}     den mdl 
  | []         -> raise (CouldNotSplitField s)

(* Extracts attributes from a string made of a list of subfield definitions delimited by defsep. *)
let fieldExtraction (defsep,defsym) i s mdl =
  let strl = Str.split (Str.regexp defsep) s
  in  List.iter (fun s -> subfieldDefExtraction defsym i s mdl) strl
	    
(* Counts ngrams in the fields inside an instance inst.
   If the field has subfields, counting is performed only in the values.
   Raises BoundsDoesNotFormARank if n => m. *)
exception BoundsDoesNotFormARank of int*int
let instanceExtraction inst mdl =
  let len = Array.length inst in
  begin
    for i=0 to len-1 do  
      match inst.(i) with
      | Arff.String str ->
	 (match subfields.(i) with
	  | None      -> singleFieldExtraction {position=i;subfield=None} str mdl 
	  | Some defs -> fieldExtraction defs i str mdl)
      | _ -> ()
    done;
    mdl
  end

let fileExtraction data mdl =
  List.fold_right instanceExtraction data mdl

(**********************************************************)

let buildModel mdl = mdl
	    
(**********************************************************)

let printModel file mdl =
  let l  = Hashtbllib.toList mdl in
  let sl  = List.sort (fun (fld1,_) (fld2,_) -> if fld1>fld2 then 1 else if fld1=fld2 then 0 else -1) l in
  List.iter
    (fun (fld,att) ->
     let (ordl,len) = att.length in
     let fldname   = String.concat "-" [string_of_int fld.position;
					Http.fieldNamesArray.(fld.position);
					getSubfield fld.subfield] in
     let fldlngth  = String.concat "-" [(*string_of_int ordl;*)fldname;"length"] in
     begin
       Printf.fprintf file "@attribute %s string // Length mean: %f, Length dev: %f \n" fldlngth len.mean (Distrib.stdDev len);
       let l1  = List.sort (fun (_,(o1,r1)) (_,(o2,r2)) -> compare r2 r1) (Hashtbllib.toList att.occurrences) in
       List.iter
	 (fun (ngram,(ordn,occ)) ->
	  let ngrmname = labelOfCategory ordn fldname ngram
	  in  Printf.fprintf file "@attribute %s string // Frecuency Mean: %f , Variance: %f\n" ngrmname occ.mean (Distrib.stdDev occ))
	l1
     end)
    sl

let train file =
  match file.data with
  | Values l ->
     let size = 10*54*(List.length l) in
     let mdl = Hashtbl.create ~random:false size
     in buildModel (fileExtraction l mdl)
  | _ -> Hashtbl.create ~random:false 1
    
let countNgrams mdl =
  Hashtbl.fold (fun _ entry n -> (Hashtbl.length entry.occurrences)+1+n) mdl 0

(***********************************************************)

let listDistance l1 l2 =
  let rec loop l n = 
    match l with
    | [] -> 0
    | ngram::l3 -> ((Listlib.depth ngram l2)-n)+(loop l3 (n+1)) 
  in loop l1 0

type abnormal =
  | UnknownModelField of field
  | BadLength    of float*float
  | BadFrecuency of string*float*float

let printReason out abn =
  match abn with
  | UnknownModelField fld ->
     Printf.fprintf out "Unknown Model Field: %a" printField fld
  | BadLength (d,sdv) ->
     Printf.fprintf out "Bad Length: %f , %f" d sdv
  | BadFrecuency (str,d,sdv) ->
     Printf.fprintf out "Bad Frecuency %s: %f , %f" str d sdv
				   
exception UnknownTestField of field
exception UnknownNgram of t
let fieldDistance mdl test fld =
  try
    let {length=(_,dlen1);occurrences=fldmdl1} =
      Hashtbl.find mdl fld in
    let {length=(_,dlen2);occurrences=fldmdl2} =
      try  Hashtbl.find test fld
      with Not_found -> raise (UnknownTestField fld) in
    let lendist = if outlier dlen1 dlen2 then [BadLength ((meanDistance dlen1 dlen2),Distrib.stdDev dlen1)] else [] in
    let ngramdist =
      Hashtbl.fold
	(fun ngrm (_,r2) l ->
	 let  r1 = try snd(Hashtbl.find fldmdl1 ngrm) with Not_found -> Distrib.singleton 0.0
	 in  if outlier r1 r2 then (BadFrecuency (stringOfCategory ngrm,(meanDistance r1 r2),Distrib.stdDev r1))::l else l)
	fldmdl2 []
    in List.concat [lendist;ngramdist]
  with Not_found -> [UnknownModelField fld]
		      
(* El rank y las ocurrencias también deben ser distribuciones con media y desviación. O sino con un treashold que sea seteado por cada ngram. *)
       
let distance mdl test =
  List.sort
    (fun a b -> if a>b then 1 else if a=b then 0 else -1)
    (Hashtbl.fold
       (fun fld fldatt l -> (fld,fieldDistance mdl test fld)::l) test [])
    
exception ShouldBeValueModel
let testFileDistances mdl testfile =
  match testfile.data with
  | Values l -> List.map (fun inst -> distance mdl (buildModel (instanceExtraction inst (emptySmallModel ())))) l
  | _        -> raise ShouldBeValueModel

let printTestFileDistances mdl testfile out =
  let ll = testFileDistances mdl testfile
  in List.iteri
       (fun i instl ->
	begin
	  Printf.fprintf out "Instance %2d :: " i;
	  List.iter
	    (fun (fld,lscore) ->
	     if empty lscore
	     then ()
	     else
	       begin
		 Printf.fprintf out "Field %2d-%s : " fld.position (getSubfield fld.subfield);
		 List.iter (fun r -> Printf.fprintf out "%a, " printReason r) lscore
	       end)
	    instl;
	  Printf.fprintf out "\n"
	end)
       ll
(*
let printTestFileWekaModel mdl testfile out =
  let ll = testFileDistances mdl testfile
  in List.map
       (fun instl ->
	begin
	  List.mapi
	    (fun i (fld,(len,dev,score)) ->
	     Printf.fprintf
	       out "%d %2.2f, %d %2.2f, %d %5d; " fld.position (getSubfield fld.subfield) len dev score) instl;
	  Printf.fprintf out "\n"
	end)
       ll
 *)
