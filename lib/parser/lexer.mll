
{
  open Lexing
  open Parser

}

let digit   = ['0' - '9']
let letter  = ['a' - 'z' 'A' - 'Z']
let int     = '-'?digit+
let frac    = '.' digit*
let exp     = ['e' 'E'] ['-' '+']? digit+
let float   = digit* frac? exp?|"nan"|"inf"
let bool    = "true"|"false"
let var     = (letter | '_'*letter)(letter|digit|'_'|'\'')*
let symbol  = '\''(var|int)
let white   = (' '|'\t')+
let newline = '\r'|'\n'|"\r\n"


rule read =
  parse
  | white             { read lexbuf }
  | newline           { new_line lexbuf; read lexbuf }
  | int               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float             { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | bool              { BOOLEAN (bool_of_string (Lexing.lexeme lexbuf)) }
  | eof               { EOF }