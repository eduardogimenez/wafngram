open Config
open FieldModel

let maxlang = 3
let langIndex n =
  match n with
    Spanish -> 0
  | Catalonian -> 1
  | English -> 2

exception UndefinedLanguage
let langOfString str =
  match str with
    "Spanish" -> Spanish
  | "Catalonian" -> Catalonian
  | "English" -> English
  | _ -> raise UndefinedLanguage
			     
let frequencies:FieldModel.t array = Array.init maxlang (fun _ -> FieldModel.create (Config.Ngram !Config.length))

let rank:FieldModel.t array = Array.init maxlang (fun _ -> FieldModel.create (Config.Ngram !Config.length))
			     
let getDictionnary lan = frequencies.(langIndex lan)
let getRank        lan = rank.(langIndex lan)

let addNgram fldmdl str v = countNgram fldmdl (abstract str) (float_of_int v)

let load langopt fldmdl =
  match langopt with
    None      -> ()
  | Some lang -> ()
     (* Add all the values for the language that were not found in the dataset, add the frequencies in 
        the prior as default distribution. 
     FieldModel.iter
       (fun tk d ->
	try
	  let _ = FieldModel.find fldmdl tk in ()
	with Not_found ->
	  FieldModel.replace fldmdl tk d)
       (getDictionnary lang)
      *)      

let print out lan = FieldModel.print out " " (getDictionnary (langOfString lan))
	       

