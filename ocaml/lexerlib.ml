open Lexing
open Printf
       
exception SyntaxError of string

let nextLine lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let nextLines n lexbuf = for i = 1 to n do new_line lexbuf done

let trace str lexbuf= ()
			(*
  begin
    Printf.fprintf stdout "Match %s: %s\n" str (Lexing.lexeme lexbuf);
    flush stdout
  end
			 *)
exception SyntaxError of string

let printPosition outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "File %s, line %d, position:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parserWithError parser lexbuf =
  try parser lexbuf with
  | SyntaxError msg ->
     begin
       fprintf stderr "%a: syntax error when reaching this token: %s\n"
	       printPosition lexbuf msg;
       exit (-1)
     end

   | _ ->
     begin
       fprintf stderr "%a: syntax error when reaching this token: %s\n"
	       printPosition lexbuf (Lexing.lexeme lexbuf);
       exit (-1)
     end      

let parseInputFile parser filename =
  let lexbuf  = Lexing.from_channel (open_in filename) in
  begin
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    parserWithError parser lexbuf
  end
  

