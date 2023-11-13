{
  open Parser
  exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let id = letter (letter | digit | '_')*

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | id as var { VAR(var) }
  | "(*" { comment 0 lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | ":" { COLON } 
  | "let" { LET }
  | "prove" { PROVE }
  | "=" { EQUALS }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
and comment level = parse
  | "*)" { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
  | "(*hint: axiom *)" { AXIOM }
  | "(*" { comment (level + 1) lexbuf }
  | _ { comment level lexbuf }