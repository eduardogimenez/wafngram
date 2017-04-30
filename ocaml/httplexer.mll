{
open Lexing
open Httpparser
open Lexerlib
}

let fieldname      =(
   "Method" |
   "Protocol"|
   "Uri" | 
   "Query" | 
   "Content-Length" | 
   "Content-Language" | 
   "Content-Encoding" | 
   "Content-Location" | 
   "Content-MD5" | 
   "Content-Type" | 
   "Expires" |
   "Last-Modified" |
   "Host" |
   "Connection" |
   "Accept"|
   "Accept-Charset" | 
   "Accept-Encoding" |
   "Accept-Language" |
   "Cache-control" |
   "Client-ip" |
   "Cookie" |
   "Cookie2" |
   "Date" |
   "ETag" |
   "Expect" |
   "From" |
   "If-Unmodified-Since" |
   "If-Match" |
   "If-None-Match" |
   "If-Range" |
   "Max-Forwards" |
   "MIME-Version" |
   "Pragma" |
   "Proxy-Authorization" |
   "Authorization" |
   "Range" |
   "Referer" |
   "TE" |
   "Trailer" |
   "User-Agent" |
   "UA-CPU" |
   "UA-Disp" |
   "UA-OS" |
   "UA-Color" |
   "UA-Pixels" |
   "Via" |
   "Transfer-Encoding" |
   "Upgrade" |
   "Warning" |
   "X-Forwarded-For" |
   "X-Serial-Number" |
   "Remaining")
 
let blank = [' ']
let newline  = ('\r' | '\n' | "\r\n")
let twonewlines   = newline newline
let threenewlines = newline newline newline		 
let methods  = ( "GET"|"POST"|"PUT"|"HEAD"|"DELETE"|"TRACE"|"CONNECT")
let protocol = ( "HTTP/1.1" | "HTTP/1.0" )
let dosblank = ": "
let uricontents   = [^ ' ''\n']+
let fieldcontents = [^ '\n']+
let bodycontents  = [^ '\n']*
		   
rule read =
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
		      
{}
