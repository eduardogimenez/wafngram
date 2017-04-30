let length = ref 3
		 
type header = {
  ngramlength : int;
  (* Minimum frequency for an ngram to be considered in the outlier selection, depending on the ngram length *)
  minimumNgramMean : float array;
  (* Minimum frequency for an URL directory *)
  minimumUrlMean   : float;
  (* Minimum mean distance for an ngram to be considered as an outlier *)
  minimumDistance  : float array;
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
  minimumNgramMean = Array.make !length 0.0001;
  minimumUrlMean   = 0.001;
  minimumDistance  = Array.make !length 0.0001;
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
let printHeader out hd =
  begin
    Printf.fprintf out "Configuration parameters:\n Ngramlength=%d \n MinimumNgramMean = %a MinimumUrlMean = %f \n MinimumDistance = %a \n Verbose=%b \n idOnly = %b \n Inversion=%b\n"
		   hd.ngramlength
		   (fun out -> Arraylib.printSeparatedArray out (fun out r -> Printf.fprintf out "%f" r)) hd.minimumNgramMean
		   hd.minimumUrlMean
		   (fun out -> Arraylib.printSeparatedArray out (fun out r -> Printf.fprintf out "%f" r)) hd.minimumDistance
		   hd.verbose
		   hd.idOnly
		   hd.inversion;
    flush out
  end
    
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
  (* Computes the string frecuency with respect to all the field contents
     in all the instances. *)
  | GlobalCount
  (* Computes the distance with respect to a rank of the most used ngrams in the field 
     all along the instances. *)
  | Rank
      
(* Options for a single field *)
type fieldSpec = {
  tokenization : tokenization;
  subfields    : subfields option;
  countmthd    : countMethod
}

(* Default configuration values for a field *)
let default () = {
  tokenization = Ngram !length;
  subfields    = None;
  countmthd    = FrequencyInField None
}

type fieldConfig = (Http.field,fieldSpec) Hashtbl.t
		   
(* The fields with a particular configuration *)
type t = {
  header: header;
  fields: fieldConfig
}

let get (conf:fieldConfig) (fld:Http.field) =
  try Hashtbl.find conf fld
  with Not_found -> default ()

let create () = (Hashtbl.create 53:fieldConfig)
		      
let add conf fld fldc = Hashtbl.replace conf fld fldc

let print out conf =
  Hashtbl.iter
    (fun fld confld ->
     Printf.fprintf out "Field %a Tokenization %a\n" Http.printField fld printTokenization confld.tokenization) conf 
