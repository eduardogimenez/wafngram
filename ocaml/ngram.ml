open Arff
open Http
open Config
open Distrib
open Listlib
open Stringlib
open FieldModel


type fieldAttributes = {
  length      : Distrib.t;
  occurrences : FieldModel.t
}

type model = (field, fieldAttributes) Hashtbl.t

(*
let attributeMap fld tk f1 f2 lmdl =
  List.iter
    (fun mdl ->
     let  att = Hashtbl.find fld mdl in
     let natt = {length=(f1 att.length);
		 occurrences=
		   try Hashtbl.replace att.occurrences tk (f2 (FieldModel.find att.ocurrences tk))
		   with Not_found -> att.ocurrences}
     in Hashtbl.replace mdl fld natt)
     lmdl
 *)
				      
let emptyTestModel () = ((Hashtbl.create ~random:false 500):model)

(* Counts one occurrence of a given ngram. *)
let countOcurrence count ngrm =
  try
    let occ = Hashtbl.find count ngrm
    in Hashtbl.replace count ngrm (occ+1)
  with Not_found -> Hashtbl.add count ngrm 1
  
exception NotSupported
let singleFieldExtraction confld fld str mdl =
  let len    = String.length str in
  let {length=d;occurrences=fldmdl} =
    try  Hashtbl.find mdl fld
    with Not_found -> {
      length     = Distrib.singleton (float_of_int len);
      occurrences= FieldModel.create confld.tokenization}
  in
  match confld.tokenization with
  | Exclude -> ()
  | Delimiter (sep,n) -> (*
     let lstr  = Str.split (Str.regexp sep) str in
     begin
       countNgram fldmdl (FieldModel.Url lstr) 1.0;
       Hashtbl.replace mdl fld {length=Distrib.sample d (float_of_int len);occurrences=fldmdl}
     end*)
     let lstr  = Str.split (Str.regexp sep) str in
     let astr  = Array.of_list lstr in
     if len = 0 then ()
     else
       (* len => 1 and it is assumed that n => 1 *)
       let n = min n len in (* We cannot extrat ngrams larger than the field length: redefine n if necessary *)
       let nbngrams = len-n +1 in (* nbngram > 0 *)
       (match confld.countmthd with 
	| FrequencyInField _ -> 
	   let count = Hashtbl.create ~random:false nbngrams in   
	   begin
	     for i = 0 to len-1 do 
	      let ngram = Array.to_list (try Array.sub astr i (min n (len-i)) with Invalid_argument _ -> astr)
	      in countOcurrence count (FieldModel.Url ngram)  
	     done;
	     Hashtbl.iter
	       (fun ngram occ -> FieldModel.countNgramFrequency fldmdl ngram len (float_of_int occ)) count;
	     FieldModel.iter
	       (fun ngram v ->
		if not(Hashtbl.mem count ngram) then FieldModel.countOnlyThisNgram fldmdl ngram 0.0) fldmdl;
	    Hashtbl.replace mdl fld {length=Distrib.sample d (float_of_int len);occurrences=fldmdl}
	   end
	| GlobalCount -> raise NotSupported
	| _ -> 
	   let count = Hashtbl.create ~random:false nbngrams in   
	   begin
	     for i = 0 to len-1 do 
	       let ngram = Array.to_list (try Array.sub astr i (min n (len-i)) with Invalid_argument _ -> astr)
	      in countOcurrence count (FieldModel.Url ngram)  
	     done;
	     Hashtbl.iter (fun ngram occ -> countNgram fldmdl ngram (float_of_int occ)) count;
	     Hashtbl.replace mdl fld {length=Distrib.sample d (float_of_int len);occurrences=fldmdl}
	   end)
       | Ngram n ->
	  if len = 0 then ()
	  else
	    (* len => 1 and it is assumed that n => 1 *)
	    let n = min n len in (* We cannot extrat ngrams larger than the field length: redefine n if necessary *)
	    let nbngrams = len-n +1 in (* nbngram > 0 *)
	    match confld.countmthd with 
	    | FrequencyInField _ -> 
	       let count = Hashtbl.create ~random:false nbngrams in   
	       begin
		 for i = 0 to len-1 do 
		   let ngram = try String.sub str i (min n (len-i)) with Invalid_argument _ -> str
		   in countOcurrence count (abstract ngram)  
		 done;
		 Hashtbl.iter
		   (fun ngram occ -> FieldModel.countNgramFrequency fldmdl ngram len (float_of_int occ)) count;
		 FieldModel.iter
		   (fun ngram v ->
		    if not(Hashtbl.mem count ngram) then FieldModel.countOnlyThisNgram fldmdl ngram 0.0) fldmdl;
		 Hashtbl.replace mdl fld {length=Distrib.sample d (float_of_int len);occurrences=fldmdl}
	       end
	    | GlobalCount -> raise NotSupported
	    | _ -> 
	       let count = Hashtbl.create ~random:false nbngrams in   
	       begin
		 for i = 0 to len-1 do 
		   let ngram = try String.sub str i (min n (len-i)) with Invalid_argument _ -> str
		   in countOcurrence count (abstract ngram)  
		 done;
		 Hashtbl.iter (fun ngram occ -> countNgram fldmdl ngram (float_of_int occ)) count;
		 Hashtbl.replace mdl fld {length=Distrib.sample d (float_of_int len);occurrences=fldmdl}
	       end
		 
(********************************************************************************************)
(* Extraction algorithm.                                                                   *)
(********************************************************************************************)
	      
(* Extracts the attributes from a string which is an id=value subfield.*)
exception CouldNotSplitField of string
let subfieldDefExtraction conf defsym i s mdl =
  match Str.bounded_split (Str.regexp defsym) s 2 with
  | [def;den]  -> let subfld     = {position=i;subfield=Some def} in
		  let subfldconf = Config.get conf subfld
		  in  singleFieldExtraction subfldconf subfld den mdl 
  | [den]      -> let fld    = {position=i;subfield=None} in
		  let confld = Config.get conf fld
		  in  singleFieldExtraction confld fld den mdl 
  | []         -> ()
  | _          -> raise (Invalid_argument "subfieldDefExtraction: list length should be less than 2")

(* Extracts attributes from a string made of a list of subfield definitions delimited by defsep. *)
let fieldExtraction conf (defsep,defsym) i s mdl =
  let strl = Str.split (Str.regexp defsep) s
  in  List.iter (fun s -> subfieldDefExtraction conf defsym i s mdl) strl
	    
(* Counts ngrams in the fields inside an instance inst.
   If the field has subfields, counting is performed only in the values.
   Raises BoundsDoesNotFormARank if n => m. *)
exception BoundsDoesNotFormARank of int*int
let instNumbProcessing = ref 0
let instanceExtraction conf inst mdl =
  let len = Array.length inst in
  begin
    instNumbProcessing := !instNumbProcessing+1;
    for i=0 to len-1 do  
      match inst.(i) with
      | Arff.String str ->
	 let fld    = {position=i;subfield=None} in
	 let confld = Config.get conf fld in
	 (match confld.subfields with
	  | None      -> singleFieldExtraction confld fld str mdl 
	  | Some defs -> fieldExtraction       conf   defs i str mdl)
      | _ -> ()
    done;
    mdl
  end

let fileExtraction conf data mdl =
  begin
    instNumbProcessing := 0;
    List.fold_right (instanceExtraction conf) data mdl
  end
(**********************************************************)

let extractInstances conf l =
  List.map (fun inst -> instanceExtraction conf inst (emptyTestModel ())) l
	   
let extractTestFileModel conf testfile =
  match testfile.data with
  | Values l -> extractInstances conf l
  | _        -> raise (Invalid_argument "extractTestFileModel: should be a Values model")

		  
let trainRanking mdl fldmdl = ()
  
exception MethodIncompatibleWithTokenization of Config.tokenization	  
let buildModel conf isTestMdl mdl =
  begin
    Hashtbl.iter
      (fun fld entry ->
       let confld = Config.get conf fld
       in  match confld.countmthd with
           | Rank ->
	      begin
		trainRanking mdl entry.occurrences;
		buildFieldModel confld entry.occurrences
	      end
	   | FrequencyInField lang ->
	      (match confld.tokenization with
	       | Exclude -> ()
	       | Ngram n -> 
		  begin
		    (if not isTestMdl then Prior.load lang entry.occurrences);
		    buildFieldModel confld entry.occurrences
		  end
	       | x -> raise (MethodIncompatibleWithTokenization x))
	   | _ -> ())
      mdl;
    mdl
  end

(*********************************************************************)
    
module Rank = struct
  
  type t = (Http.field, Distrib.t) Hashtbl.t
  let create () = ((Hashtbl.create ~random:false 200):t)



  let score fldmdl fldmdltest =
    FieldModel.fold
      (fun n1 n2 -> n1 +. n2)
      (fun ngrm d2 n ->
       let  r1 = try (FieldModel.find fldmdl ngrm).mean
		 with Not_found -> float_of_int (FieldModel.size fldmdl)
       in  (abs_float (r1 -. d2.mean)) +. n)
      fldmdltest 0.0 
		     
  let goals conf mdl ltest =
    let smdl = create () in
    begin
      List.iter
	(fun test ->
	 Hashtbl.iter
	   (fun fld att ->
	    let sc = try score (Hashtbl.find mdl fld).occurrences att.occurrences with Not_found -> infinity in
	    let d  = try (Distrib.sample (Hashtbl.find smdl fld) sc) with Not_found -> Distrib.singleton sc
	    in  Hashtbl.replace smdl fld d)
	    test)
	ltest;
      smdl
    end
      
  let outlier confld fld fldmdl fldmdltest smdl =
    let sc = Distrib.singleton (score fldmdl fldmdltest) in
    let gl = Hashtbl.find smdl fld
    in [Distrib.outlier gl sc]
	 
  let printScore out smdl =
    begin
      List.iter (fun r -> Printf.fprintf out "%f\n" r) [10.0;20.0;30.0;40.0;50.0;60.0;70.0;80.0;90.0;100.0];
      Printf.fprintf out "\n";
      Hashtbl.iter
	(fun fld d -> Printf.fprintf out "%a\t%a\n" fld Http.printField Distrib.printHistogram d)
	smdl
    end
      
	 end

(**********************************************************)

(* Assigns an ordinal to each attribute, represented as a string. *)
type ordinals = (string,int) Hashtbl.t
let ord   = ref 0
let addOrdinal ordtbl name =
  begin
    Hashtbl.add ordtbl name !ord;
    ord:=!ord+1
  end
    
let computeOrdinals l =
  let ordtbl = Hashtbl.create ~random:false (List.length l) in
  begin
    List.iter (fun (name,_) -> addOrdinal ordtbl name) l;
    addOrdinal ordtbl "Class";
    ordtbl
  end

let fieldLengthName fld = String.concat "-" [(Http.fieldName fld);"length"]
let ngramFieldName  fld ngram = String.concat "-" [(Http.fieldName fld);stringOfCategory ngram]
    
let modelToList mdl =
  Hashtbl.fold
    (fun fld att lr ->
     let fldname  = Http.fieldName  fld in
     let fldlngth = fieldLengthName fld in
     (fldlngth,att.length)::(List.concat [FieldModel.toStringList fldname att.occurrences;lr]))
    mdl
    []

let modelToValueList mdl =
  List.map (fun (name,d) -> (name,Arff.Float d.mean)) (modelToList mdl)

let addUnknownAttributes refmdl mdl =
  Hashtbl.iter
    (fun fld att ->
     try let {length=_;occurrences=reffldmdl} = Hashtbl.find refmdl fld
         in 
	 FieldModel.iter
	   (fun ngram occ ->
	    if FieldModel.mem reffldmdl ngram
	    then ()
	    else FieldModel.replace reffldmdl ngram (Distrib.singleton 0.0))
	   att.occurrences
     with  Not_found ->
       Hashtbl.add refmdl fld
		   {length=(Distrib.singleton 0.0);
		    occurrences=FieldModel.unknownTokens att.occurrences})
    mdl
	   
let sortModel mdl = 
  List.sort
    (fun (name1,_) (name2,_) -> compare name1 name2)
    (modelToList mdl)

let makeHeader conf refmdl linst =
  begin
    List.iter (fun inst -> addUnknownAttributes refmdl (instanceExtraction conf.fields inst (emptyTestModel ()))) linst;
    sortModel refmdl
  end
    
let printModel file hdr =
  begin
    Printf.fprintf file "@relation NgramModel \n";
    List.iter
      (fun (name,d) ->
       Printf.fprintf
	   file
	   "@attribute '%s' numeric // Mean: %a, Deviation: %a \n"
	   (Arff.escape name)
	   Arff.valuePrintf (Arff.Float d.mean)
	   Arff.valuePrintf (Arff.Float (Distrib.stdDev d)))
      hdr;
    Printf.fprintf file "@attribute Class {Normal, Abnormal}\n";
    Printf.fprintf file "@data\n";
    flush file
  end

(*
let printModelInstance file category ordtbl lval =
  let lvalandclass = List.concat [lval;[("Class",Arff.Enumerated category)]] in
  let lordvalandclass = List.map (fun (name,v) -> try (Hashtbl.find ordtbl name,v) with Not_found -> raise (Invalid_argument name)) lvalandclass in
  let slordvalandclass = List.sort (fun (o1,_) (o2,_) -> compare o1 o2) lordvalandclass in
  Arff.printOcurrenceInstance
    file
    (fun file (ord,v) -> Printf.fprintf file "%d %a" ord Arff.valuePrintf v) slordvalandclass
 *)      

let nameToOrdinal ordtbl l =
  List.map
    (fun (name,v) ->
     try (Hashtbl.find ordtbl name,v)
     with Not_found -> raise (Invalid_argument name) (* begin addOrdinal ordtbl name;(Hashtbl.find ordtbl name,v) end *) )
    l
    
let printModelInstance file category ordtbl lval =
  let lvalandclass     = List.concat [lval;[("Class",Arff.Enumerated category)]] in
  let lordvalandclass  = nameToOrdinal ordtbl lvalandclass in
  let slordvalandclass = List.sort (fun (o1,_) (o2,_) -> compare o1 o2) lordvalandclass in
  Arff.printOcurrenceInstance
    file
    (fun file (ord,d) -> Printf.fprintf file "%d %a" ord Arff.valuePrintf d) slordvalandclass
    
let extractAndPrintModelInstance file category ordtbl conf inst =
  printModelInstance file category ordtbl (modelToValueList (instanceExtraction conf inst (emptyTestModel ())))

let extractAndPrintModel file category ordtbl conf absarff =
    match absarff.data with
  | Values l ->   List.iter (extractAndPrintModelInstance file category ordtbl conf) l
  | _        -> raise (Invalid_argument "extractAndPrintModel")
		      

(*
let printData file category ordtbl l =
  begin
    List.iter (printModelInstance file category ordtbl) l;
    flush file
  end
 *)
		      
let train conf file =
  match file.data with
  | Values l ->
     let size = 10*54*(List.length l) in
     let mdl = Hashtbl.create ~random:false size
     in buildModel conf false (fileExtraction conf l mdl)
  | _ ->  Hashtbl.create ~random:false 1
    
let countNgrams mdl =
  Hashtbl.fold (fun _ entry n -> (FieldModel.size entry.occurrences)+1+n) mdl 0

(***********************************************************)

type abnormal =
  | UnknownModelField of Http.field
  | BadLength    of float*float
  | BadFrequency of token*float*float*float

let printReason conf out abn =
  match abn with
  | UnknownModelField fld ->
     Printf.fprintf out "Unknown Model Field: %a" printField fld
  | BadLength (d,sdv) ->
     Printf.fprintf out "Bad Length: %f , %f" d sdv
  | BadFrequency (str,d,m,sdv) ->
     if   conf.header.verbose
     then Printf.fprintf out "Bad Frequency %s: d=%f , m=%f, s=%f" (stringOfCategory str) d m sdv
     else Printf.fprintf out "%s " (stringOfCategory str)

(*
let outlierFrequency dlen2 r1 r2 =
  let expectedOccMin = (Pervasives.max 0.0 (r1.mean -. 3.0 *. (stdDev r1)))*. dlen2.mean in
  let expectedOccMax = ceil  ((r1.mean +. 3.0 *. (stdDev r1))*. dlen2.mean ) in
  let actualOcc      = r2.mean *. dlen2.mean
  in  not (expectedOccMin <= actualOcc && actualOcc <= expectedOccMax)
*)

let outlierFrequency r1 r2 = outlier r1 r2
					   (*
  let expectedOccMin = ceil  (r1.minval *. dlen2.mean)  in
  let expectedOccMax = ceil  (r1.maxval *. dlen2.mean ) in
  let actualOcc      = r2.mean *. dlen2.mean
  in  not (expectedOccMin <= actualOcc && actualOcc <= expectedOccMax)
					    *)

let frequencyDistance confld fldmdl1 fldmdl2 gmdl = 
  FieldModel.fold
    (fun l1 l2 -> List.concat[l1;l2])
    (fun ngrm r2 l ->
     let  r1 = try FieldModel.find fldmdl1 ngrm
	       with Not_found ->
		 (try (match confld.countmthd with
		       | FrequencyInField (Some lan) -> FieldModel.find (Prior.getDictionnary lan) ngrm
		       | _ -> Distrib.singleton 0.0)
		  with Not_found -> Distrib.singleton 0.0)
     in  if (outlierFrequency r1 r2) then
	   begin
	     (BadFrequency (ngrm,(meanDistance r1 r2),r1.mean,Distrib.stdDev r1))::l
	   end
	     else l)
    fldmdl2 []
    
exception UnknownTestField of Http.field
exception UnknownNgram of t
let fieldDistance mdl confld fld fldatt gmdl =
  try
    let {length=dlen1;occurrences=fldmdl1} = Hashtbl.find mdl fld in
    let fldmdl2 = fldatt.occurrences in
    let dlen2   = fldatt.length in
    let lendist = if outlier dlen1 dlen2 then [BadLength ((meanDistance dlen1 dlen2),Distrib.stdDev dlen1)] else [] in
    let ngramdist = frequencyDistance confld fldmdl1 fldmdl2 gmdl
    in List.concat [lendist;ngramdist]
  with Not_found -> [UnknownModelField fld]
		      
(* El rank y las ocurrencias también deben ser distribuciones con media y desviación. O sino con un treashold que sea seteado por cada ngram. *)
       
let distance conf mdl test gmdl =
  List.sort
    (fun a b -> if a>b then 1 else if a=b then 0 else -1)
    (Hashtbl.fold
       (fun fld fldatt l ->
	let confld = Config.get conf fld in
	if confld.tokenization = Config.Exclude then l else (fld,fieldDistance mdl test confld fld attfld gmdl)::l) test [])

let getInstances testfile =
  match testfile.data with
  | Values l -> l
  | _        -> raise (Invalid_argument "getInstances: should take a Values argument")
		      
		      
let fatalCriteria abn =
  match abn with
  | UnknownModelField _ -> true
  | BadFrequency (ngrm,d,m,dev) -> d<>0.0 && m = 0.0 (* Una ocurrencia de un ngram no visto en el entrenamiento *)
  | BadLength (_,dev) -> dev = 0.0 (* Diferencias en un largo que es constante *)
				 
let countCriteria confhd abn =
  match abn with
  | BadFrequency (ngrm,d,m,dev) ->
       (match ngrm with
	|String str ->
	  (abs_float d) > confhd.minimumDistance.((String.length str)-1) &&
	              m > confhd.minimumNgramMean.((String.length str)-1)
	|Url      l ->
	  (abs_float d) > confhd.minimumDistance.(0) &&
	              m > confhd.minimumUrlMean)
  | _ -> false

		  
let spot conf fld lscore =
  let fatal = List.filter fatalCriteria lscore in
  let count = List.filter (countCriteria conf.header) lscore in
(*  let (exceeded,warning) = if (List.length count) > 2 then (count,[]) else ([],count) in
  let status = fatal<>[] || (List.length count > 2)
  in  (status,fatal,exceeded,warning) *)
  if fatal<>[]
  then (true,fatal,[],[])
  else if (List.length count) > 2
  then (true,[],count,[])
  else (false, [],[],count)

let printReasonList conf out str lscore =
  if lscore <> [] then
    begin
      Printf.fprintf out "%s: " str;
      List.iter (fun r -> Printf.fprintf out "%a, " (printReason conf) r) lscore;
    end


      
let nbOutliers = ref 0
let printTestFileDistances conf abs mdl ltestinst gmdl out =
  let outlierInstance = ref false in
  let adata = Arff.getValuesAsArray abs in
  let printTestFileDistanceListInverted i instl =
      if List.exists
	   (fun (fld,lscore) ->
	    let (status,fatal,count,warning) = spot conf fld lscore
	    in status) instl
      then outlierInstance := true
      else
	if conf.header.idOnly
	then Printf.fprintf out "ID %a\n" Arff.valuePrintf (adata.(i).(Http.FieldNames.idField))
	else Arff.printValuesInstance out adata.(i) in
  let printTestFileDistanceListNormal i instl =
    begin
      if conf.header.idOnly
      then ()
      else Arff.printValuesInstance out adata.(i);
      List.iter
	(fun (fld,lscore) ->
	 let (status,fatal,count,warning) = spot conf fld lscore
	 in if status then
	      begin
		outlierInstance := true;
		if conf.header.idOnly
		then Printf.fprintf out "ID %a\n" Arff.valuePrintf (adata.(i).(Http.FieldNames.idField))
		else
		  begin
		    Printf.fprintf out "\t\t";
		    Printf.fprintf out "Field %2d-%s : " fld.position (getSubfield fld.subfield);
		    printReasonList conf out "FATAL" fatal;
		    printReasonList conf out "EXCEEDS FREQUENCY" count;
		    printReasonList conf out "WARNING" warning;
		    Printf.fprintf out "\n"
		  end
	      end
	    else ())
	instl
    end in
  let printList print l =
    	List.iteri
	  (fun i inst -> 
	   begin
	     Printf.fprintf out "Instance %2d :: " i;
	     let testmdl = instanceExtraction conf.fields inst (emptyTestModel ())
	     in print i (distance conf.fields mdl (buildModel conf.fields true testmdl) );
	     Printf.fprintf out "\n";
	     if !outlierInstance then begin nbOutliers := !nbOutliers+1; outlierInstance:=false end;		 
	   end)
	  l
  in  if conf.header.inversion
      then printList printTestFileDistanceListInverted ltestinst
      else printList printTestFileDistanceListNormal   ltestinst


let printHistograms out mdl =
  begin
    List.iter (fun r -> Printf.fprintf out "%f\n" r) [0.1;0.2;0.3;0.4;0.5;0.6;0.7;0.8;0.9;1.0];
    Printf.fprintf out "\n";
    Hashtbl.iter
    (fun fld att ->
     (FieldModel.iter (fun tk d -> Printf.fprintf out "%s\t%a\n" (ngramFieldName fld tk) Distrib.printHistogram d) att.occurrences))
    mdl
  end
