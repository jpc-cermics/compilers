type token =
  | STRING of (string)
  | EOF

val parse_string :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> string
