exception Eof
val __ocaml_lex_tables : Lexing.lex_tables
val token : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_token_rec : Lexing.lexbuf -> int -> XMLParser.token
val element_terminal_comment_value : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_element_terminal_comment_value_rec :
  Lexing.lexbuf -> int -> XMLParser.token
val element_terminal_initial_value_value : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_element_terminal_initial_value_value_rec :
  Lexing.lexbuf -> int -> XMLParser.token
val element_fixed_value : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_element_fixed_value_rec :
  Lexing.lexbuf -> int -> XMLParser.token
val element_terminal_nominal_value_value : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_element_terminal_nominal_value_value_rec :
  Lexing.lexbuf -> int -> XMLParser.token
val equation_value : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_equation_value_rec : Lexing.lexbuf -> int -> XMLParser.token
val when_clause_value : Lexing.lexbuf -> XMLParser.token
val __ocaml_lex_when_clause_value_rec :
  Lexing.lexbuf -> int -> XMLParser.token
