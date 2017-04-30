open Printf
open Lexerlib

let parser filename =
  let lexbuf  = Lexing.from_channel (open_in filename)
  in Lexerlib.parserWithError (Priorparser.file Priorlexer.read) lexbuf

				    
