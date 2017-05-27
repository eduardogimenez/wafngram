{
open Lexing
open Waflogparser
open Lexerlib
}

let blank = [' ']
let newline  = ('\r' | '\n' | "\r\n")
let twonewlines   = newline newline
let threenewlines = newline newline newline		 
let methods  = ( "GET"|"POST"|"PUT"|"HEAD"|"DELETE"|"TRACE"|"CONNECT"|"OPTIONS"|"PROPFIND")
let protocol = ( "HTTP/1.1" | "HTTP/1.0" )
let dosblank = ": "
let dosblanknewline = dosblank newline
let dosblankdosnewline = dosblank newline newline
let uricontents   = [^ ' ''\n']+
let fieldcontents = [^ '\n']+
let bodycontents  = [^ '\n']*

let reqIdent   = ['0'-'9']+
let beginToken  = "Start - Id"
let endToken    = "End - Id"
let classToken  = "class"
let classField  = ( "Valid" | "Attack" )
let fieldname   = ['-''.''_''0'-'9''a'-'z''A'-'Z']+
		   

rule read =
parse
  | blank              { trace "blank" lexbuf; read lexbuf}
  | beginToken         { trace "beginreq" lexbuf;  BEGINREQ}
  | dosblank           { trace "dosblank" lexbuf;  readFieldContents lexbuf}
  (* This rule is to cover an empty field *)
  | dosblanknewline    { trace "dosblanknewline" lexbuf; nextLines 1 lexbuf; read lexbuf}
  (* This rule is to cover an empty field followed by the request body *)
  | dosblankdosnewline { trace "dosblankdosnewline" lexbuf;  nextLines 2 lexbuf; readBody  (Buffer.create 50) lexbuf }
  | newline            { trace "Newline1" lexbuf;  nextLines 1 lexbuf; readFieldName lexbuf }
  | twonewlines        { trace "Newline2" lexbuf;  nextLines 2 lexbuf; readBody  (Buffer.create 50) lexbuf }
  | endToken           { trace "endToken" lexbuf; ENDREQ}
  | protocol           { trace "Protocol:" lexbuf; (PROTOCOL (Lexing.lexeme lexbuf))}   
  | uricontents        { trace "URI:" lexbuf; (URI (Lexing.lexeme lexbuf))}   
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 1: " ^ Lexing.lexeme lexbuf)) }		       
and readFieldName =
  parse
  | beginToken         { trace "beginreq" lexbuf;  BEGINREQ}
  | endToken           { trace "endToken" lexbuf; ENDREQ}
  | classToken         { trace "classtoken in redClass" lexbuf; CLASSTOKEN}
  | methods            { trace "Method:" lexbuf;   (METHOD (Lexing.lexeme lexbuf))}
  | fieldname          { trace "Fieldname" lexbuf; (FIELDNAME     (Lexing.lexeme lexbuf))}
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 2: " ^ Lexing.lexeme lexbuf)) }
and readFieldContents =
   parse
  | classField         {trace "class in redClass" lexbuf;CLASS (Lexing.lexeme lexbuf)}
  | fieldcontents      {trace "Fieldcontents 2" lexbuf; (FIELDCONTENTS (Lexing.lexeme lexbuf))}		      
  | eof                {EOF}
  | _                  { raise (SyntaxError ("Illegal string character 3: " ^ Lexing.lexeme lexbuf)) }
and readBody buff =
  parse
  (*  | endToken2           {trace "EndToken2" lexbuf; nextLines 3 lexbuf;BODY (Buffer.contents buff)} *)
  | twonewlines         {trace "EndToken2" lexbuf; nextLines 2 lexbuf;BODY (Buffer.contents buff)}
  | newline as str      {trace "Newline4" lexbuf;Buffer.add_string buff str;nextLines 1 lexbuf; readBody buff lexbuf }
  | eof                 {EOF}
  | bodycontents as str {trace "Body contents" lexbuf;Buffer.add_string buff str; readBody buff lexbuf}
  | _                   { raise (SyntaxError ("Illegal string character 4: " ^ Lexing.lexeme lexbuf)) }
		      
{}
