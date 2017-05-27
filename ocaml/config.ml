let length = ref 3
		 
type header = {
  ngramlength : int;
  (* Detail why an instance is an outlier. *)
  verbose     : bool;
  (* Print only the outlier ID, and nothing else *)
  idOnly      : bool;
  (* Invert the output, printing instances that are NOT outliers. *)
  inversion   : bool;
(*
  input       : string;
  model       : string;
  test        : string;
  output      : string
 *)
}

let defaultHeader () = {
  ngramlength = !length;
  verbose = false;
  idOnly  = false;
  inversion = false
(*
  input = "input.arff";
  model = "ngram-model.arff";
  test  = "test.arff";
  output = "output-counter.arff"
 *)
}
    
type tokenization =
  (* Count ngrams of the specified lenth *)
  | Ngram of int
  (* Split the string with respect to the specified separator *)
  | Delimiter of (string*int)
  (* Do not perform any operation on this field. *)
  | Exclude
let printTokenization out tk =
  match tk with
  | Ngram n -> Printf.fprintf out "Ngram %d" n
  | Delimiter (str,n) -> Printf.fprintf out "Delimiter %s %d" str n
  | Exclude -> Printf.fprintf out "Exclude"

      
(* Split the field into equations using the first string as 
   separator and the second string as equality symbol *)
type subfields = string*string

type language =
  | Spanish
  | English
  | Catalonian
			  
type countMethod =
  (* Count the number of ocurrences of each ngram in the field, computes 
     normal distribution of the number of ocurrences in the instances *)
  | NumberInField
  (* Compute the string frecuency of the string in this field, 
     then average frecuency on all instances *)
  | FrequencyInField of language option
  (* Computes the distance with respect to a rank of the most used ngrams in the field 
     all along the instances. *)
  | Rank of language option
  (* Only considers the aplhabet used, without considering frequencies *)
  | Alphabet of language option
(* Computes the simplified Mahalanobis distance respecto to each n-gram. *)
  | Mahalanobis of language option

let defaultMethod    = ref (Rank None)
let minimumNgramMean = ref (Array.make 3 0.0)
let minimumDistance  = ref (Array.make 3 0.0)
			
let printCountMethod out mthd =
  match mthd with
  | NumberInField         -> Printf.fprintf out "NumberInField"
  | FrequencyInField lang -> Printf.fprintf out "FrequencyInField"
  | Rank  lang            -> Printf.fprintf out "Rank"
  | Mahalanobis  lang     -> Printf.fprintf out "Mahablanobis"
  | Alphabet     lang     -> Printf.fprintf out "Alphabet"


let printHeader out hd =
  begin
    Printf.fprintf out "Configuration parameters:\n Ngramlength=%d \n MinimumNgramMean = %a \n MinimumDistance = %a \n Verbose=%b \n idOnly = %b \n Inversion=%b\n"
		   hd.ngramlength
		   (fun out -> Arraylib.printSeparatedArray out (fun out r -> Printf.fprintf out "%f" r)) !minimumNgramMean
		   (fun out -> Arraylib.printSeparatedArray out (fun out r -> Printf.fprintf out "%f" r)) !minimumDistance
		   hd.verbose
		   hd.idOnly
		   hd.inversion;
    flush out
  end

					    
module Field = struct

  (* Options for a single field *)
type t = {
  tokenization    : tokenization;
  subfields       : subfields option;
  countmthd       : countMethod;
  minimumDistance : float array;
  minimumMean     : float array
}

(* Default configuration values for a field *)
let default () = {
  tokenization    = Ngram !length;
  subfields       = None;
  countmthd       = !defaultMethod;
  minimumDistance = !minimumDistance;
  minimumMean     = !minimumNgramMean
}

type config = (Http.field,t) Hashtbl.t

end
		 
(* The fields with a particular configuration *)
type t = {
  header: header;
  fields: Field.config
}

let get (conf:Field.config) (fld:Http.field) =
  try Hashtbl.find conf fld
  with Not_found -> Field.default ()

let create () = (Hashtbl.create 53:Field.config)
		      
let add conf fld fldc = Hashtbl.replace conf fld fldc

let print out conf =
  begin
    printHeader out conf.header;
    Hashtbl.iter
      (fun fld confld ->
       Printf.fprintf out "Field %a Tokenization %a Method %a\n" Http.printField fld printTokenization confld.Field.tokenization printCountMethod confld.Field.countmthd) conf.fields;
    flush out;
    close_out out
  end
