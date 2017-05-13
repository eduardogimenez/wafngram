open Config
open Distrib
open Stringlib

(* The type of abstracted ngrams. 
   Strings of the same symbol and numbers are collapsed. *)
type token =
  | String of string
  | Url    of string list

(* Abstracts away a string into a token. Tokens are normalized to lowercase. *)
let abstract str =
  if isNumber str then String "N"
  else String (String.lowercase (Stringlib.removeAccents str))
	      
(* Printable token representation. *)
let stringOfCategory cat =
  match cat with
  | String str -> (Stringlib.removeAccents (escape str))
  | Url      l -> String.concat "/" (List.map escape l)

(* Printable http field representation. *)
let labelOfCategory fld cat =
  match cat with
  | String str -> String.concat "-" [fld;"S";escape str]
  | Url      l -> String.concat "-" [fld;"U";String.concat "/" l]

	
module MakeDistribTries =
  Tries.MakeTries
    (struct
      type d = Distrib.t
      let sample = Distrib.sample
      let singleton = Distrib.singleton
      let project d = d.Distrib.mean
      let frequency = Distrib.computeFrequency
      let print out d = Printf.fprintf out "%f" d.mean
    end)
	
module StringTries =
  MakeDistribTries
    (struct 
      type t = string
      type symbol = char
      let length str = String.length str
      let get str i =  String.get str i
      let append str c = (str^(String.make 1 c))
      let toString str = str
      let empty = ""
    end)

module UrlTries =
  MakeDistribTries
    (struct 
      type t = string list
      type symbol = string
      let length  l = List.length l
      let get l i =  List.nth l i
      let append l str = List.append l [str]
      let toString l = String.concat "/" l
      let empty = []
    end)

type t =
  | StringTries of StringTries.t
  | UrlTries    of UrlTries.t
       
let create mthd =
  match mthd with
  | Ngram _     -> StringTries (StringTries.create ())
  | Delimiter _ -> UrlTries (UrlTries.create ())
  | Exclude     -> StringTries (StringTries.create ()) (* never used*)

let elim1 f1 f2 t = 
  match t with
  | (StringTries t1) -> f1 t1
  | (UrlTries    t2) -> f2 t2

let inj f1 f2 t =
  match t with
  | (StringTries t1) -> StringTries (f1 t1)
  | (UrlTries    t2) -> UrlTries    (f2 t2)
			   
let elim2 f1 f2 t tk = 
  match t,tk with
  | (StringTries t1),(String str) -> f1 t1 str
  | (UrlTries    t2),(Url l)      -> f2 t2 l
  | _, (String str)               -> raise (Invalid_argument str)
  | _, (Url l)                    -> raise (Invalid_argument (String.concat "/" l))

			   
(* Samples a real value associated to an ngram. *)
let countNgram             = elim2 StringTries.count UrlTries.count
let countOnlyThisNgram     = elim2 StringTries.countExact UrlTries.countExact
let countNgramFrequency    = elim2 StringTries.countFrequency UrlTries.countFrequency
let find          = elim2 StringTries.find UrlTries.find
let mem           = elim2 StringTries.mem  UrlTries.mem
let size          = elim1 StringTries.size UrlTries.size
let replace       = elim2 StringTries.replace UrlTries.replace
let ratio         = elim1 StringTries.ratio   UrlTries.ratio
let print out sep t =
  elim1 (StringTries.print out sep) (UrlTries.print out sep) t
let unknownTokens = inj StringTries.setToZero UrlTries.setToZero
let iter f t      =
  match t with
  | (StringTries t1) -> StringTries.iter (fun str -> f (String str)) t1
  | (UrlTries    t2) -> UrlTries.iter    (fun l   -> f (Url l)) t2
let fold f1 f2 t x    =
  match t with
  | (StringTries t1) -> StringTries.fold f1 (fun str d r -> f2 (String str) d r) t1 x
  | (UrlTries    t2) -> UrlTries.fold    f1 (fun   l d r -> f2 (Url      l) d r) t2 x
let toStringList fld =
  elim1 (StringTries.toList (fun str -> String.concat "-" [fld;"S";escape str]))
        (UrlTries.toList    (fun l   -> String.concat "-" [fld;"U";String.concat "/" l]))
let toList =
  elim1 (StringTries.toList (fun str -> String str))
        (UrlTries.toList    (fun l   -> Url l))

(* Transforms the collected data into the field model to be used for testing. *) 
let buildFieldModel countmthd fldmdl =
  match countmthd with
(*
  | GlobalCount ->
     let len = float_of_int (Hashtbl.fold (fun ngrm d n -> d.num + n) fldmdl 0)
     in Tries.map 
	  (fun ngrm d ->
	   {mean=d.mean/. len;
	    minval=d.minval /. len;
	    maxval = d.maxval /. len;
	    sigmanum=d.sigmanum /. len;num=d.num})
	  fldmdl
 *)
  | Rank prior    ->
     let l = toList fldmdl in
     let sortedl = 
       List.sort
	 (fun (_,d1) (_,d2) ->
	  let n = d1.mean in
	  let m = d2.mean 
	  in if n<m then 1 else if n=m then 0 else -1)
	  l in
     let rankedl = Listlib.rank (fun n -> n) (fun (ngrm,d) n -> (ngrm,n)) sortedl
     in List.iter (fun (ngrm,n) -> replace fldmdl ngrm (Distrib.singleton (float_of_int n))) rankedl
  | _ -> ()
	   

