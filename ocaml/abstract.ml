open Printf
open Lexer
open Lexing
open Tokenizer

(* Name of the input file to be processed. *)
let input    = ref "input.arff"
(* Name of the output file to be produced. *)
let output   = ref "output.arff"
let field   = ref 0
let defSep = ref "&"
let defSym = ref "="
let lowBound  = ref 0
let highBound = ref 0
(*
let fields:(int*(string*string)) list ref = ref []
let transformations:(Str.regexp*string) list ref = ref replacements
 *)
		    
(********************************************************************************************)
let printPosition outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "File %s, line %d, position:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parseWithError lexbuf =
  try Some (Parser.file Lexer.read lexbuf) with
  | Lexerlib.SyntaxError msg ->
     begin
       fprintf stderr "%a: %s\n" printPosition lexbuf msg;
       None
     end
  | Parser.Error ->
     begin
       fprintf stderr "%a: syntax error when reaching this token: %s\n"
	       printPosition lexbuf (Lexing.lexeme lexbuf);
       None
     end
(********************************************************************************************)

(* Command parameters *)
let speclist = [
  ("-input",    Arg.Set_string input, "The name of the input ARFF file to be processed");
  ("-output",   Arg.Set_string output, "The name of the ARFF file to be saved");
  ("-no-ident", Arg.Clear Tokenizer.identTransform, "Do not abstract away the field names");
  ("-bounds",   Arg.Tuple [Arg.Set_int lowBound;Arg.Set_int highBound],
   "Two indexes starting from 0 which determine the rank of fields to be processed");
  ("-remove-idents", Arg.Clear Tokenizer.identKeep, "Remove the subfield names");
  ("-subfield",
   Arg.Tuple[Arg.Set_int field;Arg.Set_string defSep;Arg.String (fun defSym -> subfields.(!field) <- Some (!defSep,defSym))],
   "The index rank (starting from 0) of the attributes to be processed and the strings separating the definitions inside it")
]

(* User message *)
let userMessage =
  "The following parameters are accepted:"

(* Main program: parse and process *)
let main () =
  begin
    Arg.parse speclist (fun filein -> input:=filein) userMessage;
    let lexbuf  = Lexing.from_channel (open_in !input) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input };
    match parseWithError lexbuf with
    | Some abs ->
       let newabs = Tokenizer.process abs !lowBound !highBound
       in Arff.printf (open_out !output) newabs
    | None     -> exit (-1)
  end
    
let () = main ()  

(********************************************************************************************)
(********************************************************************************************)
