
let parse filename token_fun lexbuf =
  try Parser.stored_definition_eof token_fun lexbuf with
    | Parsing.Parse_error ->
        let linenum, linebeg =
          Linenum.for_position filename (Lexing.lexeme_start lexbuf) in
        let first_char = Lexing.lexeme_start lexbuf - linebeg in
        Printf.eprintf
          "Syntax error at line %d, characters %d to %d\n"
          linenum
          first_char 
          ((Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf) + first_char);
        raise Parsing.Parse_error
;;
