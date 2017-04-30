{
open Lexing
open Lexerlib
open Configparser      
}

let blank = [' ']
let newline  = ('\r' | '\n' | "\r\n")
let digit = ['0'-'9']
let uppercase = ['A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']
let white = [' ' '\t']+
let alphanum = (digit|alpha)+
let number = digit+
let fieldname = alpha(alphanum)*	
let escaped = '\\' ['\'' '"' '%' 'r' 'n' 'f' 'b' '\\']
let real = digit+'.'digit+
let bool = ("true"|"false")	     
				     
rule read =
  parse
| blank              {read lexbuf}
| newline            {Lexerlib.nextLine lexbuf; read lexbuf }
| "FIELDS"           {FIELDS}
| "="                {EQUALS}
| "HEADER"           {HEADER}
| "FIELDS"           {FIELDS}
| "NgramLength"      {NGRAMLENGTH}
| "MinimumNgramMean" {MINIMUMNGRAMEAN}
| "MinimumUrlMean"   {MINIMUMURLMEAN}
| "MinimumDistance"  {MINIMUMDISTANCE}
| "Verbose"          {VERBOSE}
| "IdOnly"           {IDONLY}
| "Inversion"        {INVERSION}
| "HEADER"           {HEADER}
| "Field"            {FIELD}
| "end"              {END}	    
| "Tokenization"     {TOKENIZATION}
| "Ngram"            {NGRAM}
| "Subfields"        {SUBFIELDS}	  
| "Method"           {METHOD}
| "GlobalCount"      {GLOBALCOUNT}
| "NumberInField"    {NUMBERINFIELD}
| "FrequencyInField" {FREQUENCYINFIELD}
| "Prior"            {PRIOR}
| "Spanish"          {SPANISH}
| "Catalonian"       {CATALONIAN}
| "English"          {ENGLISH}
| "Rank"             {RANK}
| "Delimiter"        {DELIMITER}
| "Exclude"          {EXCLUDE}
| bool               {BOOL (bool_of_string   (Lexing.lexeme lexbuf)) }
| real               {REAL (float_of_string (Lexing.lexeme lexbuf)) }
| number             {NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| fieldname          {FIELDNAME (Lexing.lexeme lexbuf) }
| '"'                {read_string_double (Buffer.create 17) lexbuf }
| eof                {EOF }
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
| _   { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }



