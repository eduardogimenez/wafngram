{
open Lexing
open Waflogparser
open Lexerlib
}
(*
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
 *)

let fieldname = (
  (* Standard *)
   "Accept" 	
 | "Accept-Charset"
 | "Accept-Encoding"
 | "Accept-Language"
 | "Accept-Datetime"
 | "Authorization"
 | "Cache-Control"
 | "Connection"
 | "Cookie"
 | "Content-Length"
 | "Content-MD5"
 | "Content-Type"
 | "Date"
 | "Expect"
 | "Forwarded"
 | "From"
 | "Host"
 | "If-Match"
 | "If-Modified-Since"
 | "If-None-Match"
 | "If-Range"
 | "If-Unmodified-Since"
 | "Max-Forwards"
 | "Origin"
 | "Pragma"
 | "Proxy-Authorization"
 | "Range"
 | "Referer"
 | "TE"
 | "User-Agent"
 | "Upgrade"
 | "Via"
 | "Warning"
(* No standard *)
 | "X-Requested-With"
 | "DNT"
 | "X-Forwarded-For"
 | "X-Forwarded-Host"
 | "X-Forwarded-Proto"
 | "Front-End-Https"
 | "X-Http-Method-Override"
 | "X-ATT-DeviceId"
 | "X-Wap-Profile"
 | "Proxy-Connection"
 | "X-UIDH"
 | "X-Csrf-Token"
 | "X-Request-ID"
 | "X-Correlation-ID"
 (* Also found *)
 | "X-FB-HTTP-Engine"
 | "X-FB-ABSURL-DEBUG"
 | "X-Purpose"
 | "Keep-Alive"
 | "DAV"
 | "X-Moz"
 | "UA-CPU"
 | "Save-Data"
 | "Purpose"
 | "x-fb-sim-operator"
 | "x-fb-sim-hni"
 | "cache-control"
 | "x-fb-net-hni"
 | "X-IWS-Via"
 | "Depth"
 | "Translate"
 | "X-FB-SIM-HNI"
 | "X-FB-Connection-Type"
 | "fetchergroup"
 | "Dapper-Host-Ip"
 | "x-wap-profile"
 | "X-P2P-PeerDist"
 | "X-P2P-PeerDistEx"
 | "X-Geo"
 | "Chrome-Proxy"
 | "Chrome-Proxy-Accept-Transform"
(* Response Field Names - Standard *)
 | "Access-Control-Allow-Origin"
 | "Accept-Patch"
 | "Accept-Ranges"
 | "Age"
 | "Allow"
 | "Alt-Svc"
 | "Cache-Control"
 | "Connection"
 | "Content-Disposition"
 | "Content-Encoding"
 | "Content-Language"
 | "Content-Length"
 | "Content-Location"
 | "Content-MD5"
 | "Content-Range"
 | "Content-Type"
 | "Date"
 | "ETag"
 | "Expires"
 | "Last-Modified"
 | "Link"
 | "Location"
 | "P3P"
 | "Pragma"
 | "Proxy-Authenticate"
 | "Public-Key-Pins"
 | "Refresh"
 | "Retry-After"
 | "Server"
 | "Set-Cookie"
 | "Status"
 | "Strict-Transport-Security"
 | "Trailer"
 | "Transfer-Encoding"
 | "TSV"
 | "Upgrade"
 | "Vary"
 | "Via"
 | "Warning"
 | "WWW-Authenticate"
 | "X-Frame-Options"
(* Response Field Names - Non standard *)
 | "X-XSS-Protection"
 | "Content-Security-Policy"
 | "X-Content-Type-Options"
 | "X-Powered-By"
 | "X-UA-Compatible"
 | "X-UA-Compatible"
 | "X-UA-Compatible"
 | "X-Content-Duration"
 | "Upgrade-Insecure-Requests"
 | "X-Request-ID"
 | "X-Correlation-ID"
)
  
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
