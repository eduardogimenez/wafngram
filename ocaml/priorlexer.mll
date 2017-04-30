{
open Lexing
open Priorparser
open Lexerlib
}

let blank = [' ']
let newline  = ('\r' | '\n' | "\r\n")
let ngram = ['a'-'z''ç'-'ñ''A'-'Z''Ç'-'Ñ''+']+
let count = ['0'-'9']+

rule read =
  parse
  | blank         {read lexbuf}
  | newline       {Lexerlib.nextLine lexbuf; read lexbuf }
  | "Spanish"     {LANGUAGE Config.Spanish}
  | "English"     {LANGUAGE Config.English}
  | "Catalonian"  {LANGUAGE Config.Catalonian}
  | ngram {NGRAM  (Lexing.lexeme lexbuf)} 
  | count {NUMBER (int_of_string (Lexing.lexeme lexbuf))}
  | eof   {EOF}



		  
