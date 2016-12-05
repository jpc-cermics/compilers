val parse :
  Location.filename ->
  (Lexing.lexbuf -> Parser.token) ->
  Lexing.lexbuf ->
  (Location.t Syntax.toplevel_element_desc, Location.t) Syntax.node
