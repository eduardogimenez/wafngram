{
  open Lexing
  open Lexerlib
  open Parser

let nextLine lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}
let digit = ['0'-'9']
let uppercase = ['A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = alpha('+'alpha)*
let alphanum = (digit|alpha)+
let number = digit+
let dni = digit+uppercase
let address = (text | number | "C/" | "S/N")+
let identifier = alphanum(('.'|'-'|'_')alphanum)*
let email = identifier '@' identifier
let integer = ('+'|'-')?  digit+
let exponent = ('e'|'E') ('+'|'-')? digit+
let float   = ('+'|'-')? ((digit+)? '.' digit+ exponent? | ['0'-'9']+ exponent)
let qstring = ('"'  [^'"']* '"') | ('\'' [^'\'']* '\'')
let string = [^' ' '\r' '\t' '\n' ',' '{' '}' '\'' '\"']+|'\\'[^ ',' '{' '}' '\'' '\"']+
let linecomment = '%' [^'\n' '\r']* '\r'? '\n'
let escaped = '\\' ['\'' '"' '%' 'r' 'n' 'f' 'b' '\\']
			 
rule read =
  parse
  | white              { read lexbuf }
  | newline            { new_line lexbuf; read lexbuf }
  | "@relation"        { RELATION}
  | "@data"            { DATA }
  | "@attribute"       { ATTRIBUTE}
  | '{'                { LBRACKET}
  | '}'                { RBRACKET}
  | "numeric"          { NUMERICTYPE}
  | "integer"          {INTEGERTYPE}
  | "real"             {REALTYPE}
  | "string"           {STRINGTYPE}
  | "date"             {DATETYPE}
  | ','                {COMMA}
  | '?'                {QMARK}
  | integer            { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float              { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | string as str      { STRING str }
  | '"'                { read_string_double (Buffer.create 17) lexbuf }
  | '\''               { read_string_simple (Buffer.create 17) lexbuf }
  | eof                { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string_double buf =
  parse
  | '"'       { QSTRING (Buffer.contents buf) }
  | escaped as str  {Buffer.add_string buf str; read_string_double buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string_double buf lexbuf
    }
  | eof { raise (SyntaxError ("String is not terminated")) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

and read_string_simple buf =
  parse
  | '\''       { QSTRING  (Buffer.contents buf) }
  | escaped as str  {Buffer.add_string buf str; read_string_simple buf lexbuf }
  | [^ '\'' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string_simple  buf lexbuf }
  | eof { raise (SyntaxError ("String is not terminated")) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }

 {}
