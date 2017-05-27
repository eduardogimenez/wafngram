open Printf
open Lexer
open Lexing
open Lexerlib
open Ngram
open Config
open Arff


(* Configuration file *)
let conffile = ref "config.txt"
(* Name of the input file to be processed. *)
let input    = ref "input.arff"
(* Name of the output file to be produced. *)
let output  = ref "output.arff"
let field   = ref 0
let defSep  = ref "&"
let defSym  = ref "="
let printPrior = ref false
let priorLength = ref 3
let priorFile = ref "prior.txt"
let priorLang = ref "Spanish"
let format = ref "http"
let printConfig = ref false
let configFile = ref "selected-config.txt"
let printArff = ref false
let printModel = ref false
let printData  = ref false
let arffFile  = ref "string-model.arff"
let ngramModelFile = ref "ngram-model.arff"
let testFileProvided = ref false
let testFile = ref "test.txt"
let autotest = ref false
let fraction = ref 10
let detect = ref false
let buildNgramModel = ref true
let printFieldDistrib = ref false
let fieldDistribFile = ref "field-distrib.csv"
let printFieldScore = ref false
let fieldScoreFile  = ref "field-score.csv"
let loadNgramModel = ref false
let saveNgramModel = ref false
let loadScoreGoals = ref false
let saveScoreGoals = ref false
let binaryLoadModelFile = ref "model.bin"
let binarySaveModelFile = ref "model.bin"
let binaryLoadScoreGoalsFile = ref "scores.bin"
let binarySaveScoreGoalsFile = ref "scores.bin"
		    
(********************************************************************************************)
let parseConfigFile file =
  parseInputFile (Configparser.file Configlexer.read) file
       
let parseHttpFile file cls =
  let parser =
    match !format with
    | "http"   -> Httpparser.data Httplexer.read
    | "waflog" -> Waflogparser.data  Wafloglexer.read
    | "weka"   -> Parser.file Lexer.read
    | str      -> raise (Invalid_argument ("format:"^str))
  in begin Http.category:= cls; parseInputFile parser file end
       
(********************************************************************************************)

let opFromString str =
  match str with
  | "Detect"           -> detect := true
  | "ExportAsAbnormal" -> begin printData:= true; Http.category := "Abnormal" end
  | "ExportAsNormal"   -> begin printData:= true; Http.category := "Normal"   end
  | _ -> ()
		    
(* Command parameters *)
let speclist = [
  ("-config",      Arg.Set_string conffile,  "The name of the configuration file");
  ("-print-prior", Arg.Tuple[Arg.Set printPrior;Arg.Set_string priorFile;Arg.Set_int priorLength;Arg.Symbol(["Spanish";"English";"Catalonian"],fun str -> priorLang:=str)], "Prints the specified prior's dictionnary");
  ("-input",       Arg.Set_string input, "The name of the input ARFF file to be processed");
  ("-output",      Arg.Set_string output, "The name of the ARFF file to be saved");
  ("-format",      Arg.Symbol(["http";"waflog";"arff"],fun str -> format := str), "The format of the input file: http, waflog or arff");
  ("-uudecoded",   Arg.Set Arff.uudecoded,   "The input has been previously uudecoded. Simple quotes are escaped.");
  ("-load-model", Arg.Tuple[Arg.Set loadNgramModel;Arg.Set_string binaryLoadModelFile], "Load the ngram model from a binary file, to be loadad later");
  ("-save-model", Arg.Tuple[Arg.Set saveNgramModel;Arg.Set_string binarySaveModelFile], "Load the score goals from a binary file, to be loadad later");
  ("-load-score-goals", Arg.Tuple[Arg.Set loadScoreGoals;Arg.Set_string binaryLoadScoreGoalsFile], "Save the computed score goals into a binary file, to be loadad later");
  ("-save-score-goals", Arg.Tuple[Arg.Set saveScoreGoals;Arg.Set_string binarySaveScoreGoalsFile], "Save the ngram model into a binary file, to be loadad later");
  ("-print-arff-model", Arg.Tuple[Arg.Set printArff;Arg.Set_string arffFile], "Do not compute the model, only print the input in ARFF format");
  ("-print-config", Arg.Tuple[Arg.Set printConfig;Arg.Set_string configFile], "Do not compute the model, only print the configuration");
  ("-print-weka-model", Arg.Tuple[Arg.Set printModel;Arg.Set_string ngramModelFile], "Print the ngram Weka model to the specified file");
  ("-print-field-distrib", Arg.Tuple[Arg.Set printFieldDistrib;Arg.Set_string fieldDistribFile], "Print the distribution of the ngrams as a tablulate-separated text field, to be loadad in Excel");
  ("-print-field-score", Arg.Tuple[Arg.Set printFieldScore;Arg.Set_string fieldScoreFile], "Print the distribution of the field scores as a tablulate-separated text field, to be loadad in Excel");
  ("-no-ngram-model",Arg.Clear buildNgramModel, "Skip model construction. To be used when the purpose is just to transform the input into the ARFF format.");
  ("-autotest",    Arg.Set autotest, "Take a portion of the training dataset for positive testing (use with -fraction)");
  ("-fraction",    Arg.Set_int fraction, "Randomly choose approximatly 1/n of the datasets used for testing. Chooses nothing if n=0. Use n=1 for selecting the whole dataset. If the -autotest option is not selected, then the fraction is set to 0 (the whole dataset is used for training).");
  ("-test-abnormal", Arg.Tuple[Arg.Set testFileProvided;Arg.Symbol(["Detect";"ExportAsAbnormal";"ExportAsNormal"],opFromString)], "Use the instances tagged as abnormal in the input file in waflog format as the dataset for testing");
  ("-test-set",    Arg.Tuple[Arg.Set testFileProvided;Arg.Symbol(["Detect";"ExportAsAbnormal";"ExportAsNormal"],opFromString);Arg.Set_string testFile], "Parse the specified file and use it as the dataset for testing")
]

(* User message *)
let userMessage =
  "The following parameters are accepted:"

let printResults str conf testabs mdl gmdl output =
  begin
    Printf.fprintf stdout "Printing results for %s....\n" str; 
    Printf.fprintf stdout "Testing on %d instances.\n" (Arff.numberOfInstances testabs);flush stdout;
    Ngram.printTestFileDistances conf testabs mdl (Ngram.getInstances testabs) gmdl (open_out output);
    Printf.fprintf stdout "Number of outlier instances: %d\n" !Ngram.nbOutliers;
    (let percent = (float_of_int (!Ngram.nbOutliers)) /. (float_of_int (Arff.numberOfInstances testabs))
     in Printf.fprintf stdout "Outliers ratio: %f\n\n" percent);
    Ngram.nbOutliers :=0;
    flush stdout
  end

let progressMessage str = Printf.fprintf stdout "%s\n" str;flush stdout

(* Main program: parse and process *)
exception IsHere
let main () =
  begin
    Arg.parse speclist (fun filein -> input:=filein) userMessage;
    (try
	let conf = parseConfigFile !conffile in
	begin
	  Printf.fprintf stdout "%a\n" Config.printHeader conf.header;
	  (if !printPrior then let file = (open_out !priorFile) in (Prior.print file !priorLang); flush file);
	  progressMessage "Parsing input file...";
	  let abs  = parseHttpFile !input "Normal" in 
	  begin
	    (if !printArff then let file = (open_out !arffFile) in begin Arff.printf file abs;flush file end);
	    (if !buildNgramModel then
	       let frac = if !autotest then !fraction else 0 in
	       let (absmdl,abstest) = Http.splitData "Normal" frac abs in
	       let mdl       =
		 if !loadNgramModel
		 then
		   let chnl = (open_in_bin !binaryLoadModelFile) in
		   begin progressMessage "Loading model from binary file...."; Marshal.from_channel chnl end
		 else
		   begin
		     Printf.fprintf stdout "%d instances selected for training.\n" (Arff.numberOfInstances absmdl); flush stdout;
		     progressMessage "Building model from training input file...";
		     Ngram.train conf.fields absmdl;
		   end in
               let gmdl =
		 if !loadScoreGoals
		 then
		   let chnl = (open_in_bin !binaryLoadScoreGoalsFile) in
		   begin progressMessage "Loading score goals from binary file...."; Marshal.from_channel chnl end
		 else
		   begin
		     progressMessage "Computing scores from training input file...";
		     Ngram.goals conf mdl (getInstances abs)
		   end in
	       begin
		 Printf.fprintf stdout "Number of ngrams: %d\n" (countNgrams mdl); flush stdout;
		 if !saveNgramModel then
		   let outchnlm = (open_out_bin !binarySaveModelFile) in
		   begin
		     progressMessage "Saving model to specified file";
		     Marshal.to_channel outchnlm  mdl [Marshal.Closures;Marshal.Compat_32];
		     close_out outchnlm;
		   end
		 else ();
		 if !saveScoreGoals then
		   let outchnls = (open_out_bin !binarySaveScoreGoalsFile) in
		   begin
		     Marshal.to_channel outchnls gmdl [Marshal.Closures;Marshal.Compat_32];
		     close_out outchnls
		   end
		 else ();
		 if !autotest then printResults "autotest" conf abstest mdl gmdl "autotest.txt" else ();
		 if !printFieldDistrib then Ngram.printHistograms (open_out !fieldDistribFile)  mdl else ();
		 if !printConfig       then Config.print          (open_out !configFile)       conf else ();
		 if !printFieldScore   then Ngram.printScore      (open_out !fieldScoreFile)   gmdl else ();
		 let (testabs,ntestabs) =		  
		   if !testFileProvided then
		     begin
		       let testabs =
			 if !format = "waflog"
			 then begin progressMessage "Selecting abnormal instances from input file...."; fst (Http.splitData "Abnormal" 0 abs) end
			 else begin progressMessage "Parsing test file...."; parseHttpFile !testFile "Abnormal" end in
		       let (_,ntestabs) = Http.splitData "Abnormal" !fraction testabs
		       in  (testabs,ntestabs)
		     end
		   else (Arff.empty Arff.emptyHeader,Arff.empty Arff.emptyHeader) in
		 if !detect then
		   begin
		     progressMessage "Searching for outliers in the test file....";
		     printResults !testFile conf ntestabs mdl gmdl !output
		   end
		 else ();
		 if !printModel
		 then let file = (open_out !ngramModelFile) in
		      let hdr     =  Ngram.makeHeader conf mdl (getInstances testabs) in
			begin
			  Printf.fprintf stdout "Printing model header...\n";flush stdout;
			  Ngram.printModel file hdr;
			  if !printData then
			    begin
			      progressMessage "Computing ordinals...";
			      let ordtbl = computeOrdinals hdr in
			      begin
				progressMessage "Printing training instances...";
				Ngram.extractAndPrintModel file "Normal" ordtbl conf.fields absmdl;
				progressMessage "Printing test file instances...";
				Ngram.extractAndPrintModel file !Http.category ordtbl conf.fields testabs;
			      end
			    end
			  else ();
			  flush file
			end
		 else ()
	       end)
	  end
	  (*; let file = (open_out "ngramnotfound.txt")
	  in begin Hashtbl.iter (fun ngrm n -> Printf.fprintf file "%s %d\n" ngrm n) Ngram.Frequency.count;flush file end*)
	end
     with
       CouldNotSplitField s   -> Printf.fprintf stderr "Error in instance %d: colud not split field %s\n" !Ngram.instNumbProcessing s
     | UnknownTestField fld -> Printf.fprintf stderr "Don't know field %a" Http.printField fld
    )
  end

let () = main ()  

(********************************************************************************************)
(********************************************************************************************)
