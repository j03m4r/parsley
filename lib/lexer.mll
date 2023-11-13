{
 open Parser
 exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
| [' ' '\t'] { token lexbuf }
| newline { Lexing.new_line lexbuf;token lexbuf }
| ['a'-'z' 'A'-'Z' '0'-'9']+ as var { VAR(var) }
| "(*" { comment 0 lexbuf }
| "(" { LPAREN }
| ")" { RPAREN }
| "," { COMMA }
| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
| eof { EOF }
and comment level = parse
| "*)" { if level = 0 then token lexbuf
else comment (level - 1) lexbuf }
| "(*" { comment (level + 1) lexbuf }
| _ { comment level lexbuf }