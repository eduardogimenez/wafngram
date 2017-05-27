{
open Lexing
open Httpparser
open Lexerlib
}

let blank = [' ']
let newline  = ('\r' | '\n' | "\r\n")
let twonewlines   = newline newline
let threenewlines = newline newline newline		 
let methods  = ( "GET"|"POST"|"PUT"|"HEAD"|"DELETE"|"TRACE"|"CONNECT")
let protocol = ( "HTTP/1.1" | "HTTP/1.0" )
let dosblank = ": "
let dosblanknewline = dosblank newline
let dosblankdosnewline = dosblank newline newline
let uricontents   = [^ ' ''\n']+
let fieldcontents = [^ '\n']+
let bodycontents  = [^ '\n']*

let fieldname   = ['-''.''_''0'-'9''a'-'z''A'-'Z']+

(*rule read =
  parse
  | blank              { read lexbuf}
  | dosblank           { readFieldContents lexbuf}
  | newline            { (*Printf.fprintf stdout "%s\n" "Newline1";*)  nextLines 1 lexbuf; readFieldName lexbuf }
  | twonewlines        { (*Printf.fprintf stdout "%s\n" "Newline2"; *) nextLines 2 lexbuf; readBody  (Buffer.create 50) lexbuf }
  | threenewlines      { (*Printf.fprintf stdout "%s\n" "Newline3";*)  nextLines 3 lexbuf; (BODY "")}
  | methods            { (*trace "Method:" lexbuf*)   (METHOD (Lexing.lexeme lexbuf))}
  | protocol           { (*trace "Protocol:" lexbuf*) (PROTOCOL (Lexing.lexeme lexbuf))}   
  | uricontents        { (*trace "URI:" lexbuf*)      (URI (Lexing.lexeme lexbuf))}   
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 1: " ^ Lexing.lexeme lexbuf)) }		       
and readFieldName =
   parse
  | fieldname          { (* trace "Fieldname" lexbuf*) (FIELDNAME     (Lexing.lexeme lexbuf))}
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 2: " ^ Lexing.lexeme lexbuf)) }
and readFieldContents =
   parse
  | fieldcontents      { (* trace "Fieldcontents 1" lexbuf*) (FIELDCONTENTS (Lexing.lexeme lexbuf))}		      
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 3: " ^ Lexing.lexeme lexbuf)) }
and readBody buff =
  parse
  | bodycontents as str { Buffer.add_string buff str; readBody buff lexbuf}
  | twonewlines         { nextLines 2 lexbuf;BODY (Buffer.contents buff)}
  | eof                 {EOF}
  | _                   { raise (SyntaxError ("Illegal string character 4: " ^ Lexing.lexeme lexbuf)) }
 *)

rule read =
parse
  | blank              { trace "blank" lexbuf; read lexbuf}
  | dosblank           { trace "dosblank" lexbuf;  readFieldContents lexbuf}
  (* This rule is to cover an empty field *)
  | dosblanknewline    { trace "dosblanknewline" lexbuf; nextLines 1 lexbuf; read lexbuf}
  (* This rule is to cover an empty field followed by the request body *)
  | dosblankdosnewline { trace "dosblankdosnewline" lexbuf;  nextLines 2 lexbuf; readBody  (Buffer.create 50) lexbuf }
  | newline            { trace "Newline1" lexbuf;  nextLines 1 lexbuf; readFieldName lexbuf }
  | twonewlines        { trace "Newline2" lexbuf;  nextLines 2 lexbuf; readBody  (Buffer.create 50) lexbuf }
  | threenewlines      { (*Printf.fprintf stdout "%s\n" "Newline3";*)  nextLines 3 lexbuf; (BODY "")}
  | methods            { (*trace "Method:" lexbuf*)   (METHOD (Lexing.lexeme lexbuf))}
  | protocol           { trace "Protocol:" lexbuf; (PROTOCOL (Lexing.lexeme lexbuf))}   
  | uricontents        { trace "URI:" lexbuf; (URI (Lexing.lexeme lexbuf))}   
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 1: " ^ Lexing.lexeme lexbuf)) }		       
and readFieldName =
  parse
  | methods            { trace "Method:" lexbuf;   (METHOD (Lexing.lexeme lexbuf))}
  | fieldname          { trace "Fieldname" lexbuf; (FIELDNAME     (Lexing.lexeme lexbuf))}
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 2: " ^ Lexing.lexeme lexbuf)) }
and readFieldContents =
   parse
  | fieldcontents      {trace "Fieldcontents 2" lexbuf; (FIELDCONTENTS (Lexing.lexeme lexbuf))}		      
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 3: " ^ Lexing.lexeme lexbuf)) }
and readBody buff =
  parse
  | twonewlines         {trace "Newline 4" lexbuf; nextLines 2 lexbuf;BODY (Buffer.contents buff)}
  | eof                 {EOF}
  | bodycontents as str {trace "Body contents" lexbuf;Buffer.add_string buff str; readBody buff lexbuf}
  | _                   { raise (SyntaxError ("Illegal string character 4: " ^ Lexing.lexeme lexbuf)) }

		    
{}
