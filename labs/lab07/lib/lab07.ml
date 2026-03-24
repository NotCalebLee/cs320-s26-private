
let parse (s : string) : Ast.sexpr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None


<sexpr>   ::= <atom>
            | (<list>)

<list>   ::= <sexpr>
            | <sexpr> <list>



<sexpr> ::= <atom>
  | ({<sexpr>})


{<sexpr>} = 0 or more sexpr
<sexpr>* = 0 or more
<sexpr>+ = 1 or more


