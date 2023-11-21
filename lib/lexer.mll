{
 open Parser
 exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"

rule token = parse
    | [' ' '\t'] { token lexbuf }
    | newline { Lexing.new_line lexbuf; token lexbuf }
    | "let (*prove*)" { PROVE }
    | "let rec" { LET }
    | "(*hint: " { HINT }
    | "axiom" { AXIOM }
    | "induction" { INDUCTION }
    | "type" { TYPE }
    | "of" { OF }
    | "match" { MATCH }
    | "with" { WITH }
    | "->" { ARROW }
    | "|" { VERTBAR }
    | ['a'-'z' 'A'-'Z' '0'-'9' '_']+ as var { VAR(var) }
    | "(*" { comment 0 lexbuf }
    | "*)" { ENDCOMMENT }
    | "*" { STAR }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "," { COMMA }
    | ":" { COLON }
    | "=" { EQUALS }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }
and comment level = parse
    | "*)" { if level = 0 then token lexbuf else comment (level - 1) lexbuf }
    | "(*" { comment (level + 1) lexbuf }
    | _ { comment level lexbuf }

