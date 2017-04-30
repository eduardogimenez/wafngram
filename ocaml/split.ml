open Printf
open Lexer
open Lexing
open Splitter

let input  = ref "input.arff"
let output = ref "output.arff"
let field = ref 0
let defSep = ref "&"
let defSym = ref "="
let newFields = ref []

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
       fprintf stderr "%a: syntax error when reaching this token: %s\n" printPosition lexbuf (Lexing.lexeme lexbuf);
       None
     end

		 
let speclist = [
  ("-input",     Arg.Set_string input,     "The name of the input ARFF file to be processed");
  ("-output",    Arg.Set_string output,    "The name of the ARFF file to be saved");
  ("-field",     Arg.Set_int field,        "The index of the field to be splitted, starting from 0");
  ("-separator", Arg.Set_string defSep,    "The symbol used to separate new fields");
  ("-definer",   Arg.Set_string defSym,    "The symbol used to define values for the new fields");
  ("-introduce", Arg.Rest (fun str -> newFields:= str::!newFields), "The names of the new fields to be introduced, separated by spaces")
]

let userMessage =
  "Splits a given field into a list of new ones. The following parameters are accepted:"
   
let main () =
  begin
    Arg.parse speclist (fun filein -> input:=filein) userMessage;
    let lexbuf  = Lexing.from_channel (open_in !input) in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = !input };
    match parseWithError lexbuf with
    | Some abs ->
       let attrl = List.rev !newFields in
       begin
	 List.iter (fun id -> Printf.fprintf stdout "%s\n" id) attrl;
	 let newabs = Splitter.split abs !defSep !defSym !field attrl in
	 let Arff.Values l = newabs.Arff.data in
	 begin
	   (*List.iteri (fun i inst  -> Printf.fprintf stdout "Instance %d, length %d\n" i (Array.length inst)) l;*)
	 (try
	   if (Arff.wellFormed newabs)=true then Printf.fprintf stdout "Generated file is well-formed\n";
	 with Arff.BadLength i -> Printf.fprintf stderr "Wrong length in instance %d\n" i);
	 Arff.printf (open_out !output) newabs;
	 Printf.fprintf stdout "Input  file has %d instances\n"  ( Arff.numberOfInstances abs);
	 Printf.fprintf stdout "Output file has %d instances\n"  (Arff.numberOfInstances newabs);
	 Printf.fprintf stdout "Output file has %d attributes\n" (Array.length (List.hd l));
	 end
       end
    | None     -> exit (-1)
  end
    
let () = main ()  
