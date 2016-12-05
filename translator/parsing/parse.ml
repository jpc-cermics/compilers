open Parse_error;;
open Location;;

let parse filename token_fun lexbuf =
  Config.inputfile := filename;
  try
    Parser.definition token_fun lexbuf
  with
  | Parse_error.Unclosed (pos, symbol, pos', _symbol') ->
    raise (
      Parse_error.SyntacticError {
        Parse_error.err_msg = [ "_Unclosed"; symbol; ];
        err_info = [];
        err_ctx = {
          location = {
             Location.start = pos;
             enddd = pos';
             filename = filename;
          };
        };
      }
    )
  | Parse_error.Invalid_matrix (pos, pos') ->
    raise (
      Parse_error.SyntacticError {
        err_msg = [ "_InvalidMatrixConstruct" ];
        err_info = [];
        err_ctx = {
          location = {
            start = pos;
            enddd = pos';
            filename = filename;
          };
        };
      }
    )
  | Parse_error.Invalid_array (pos, pos') ->
    raise (
      Parse_error.SyntacticError {
        err_msg = [ "_InvalidArrayConstruct" ];
        err_info = [];
        err_ctx = {
          location = {
            start = pos;
            enddd = pos';
            filename = filename;
          };
        };
      }
    )
  | Parsing.Parse_error ->
    raise (
      Parse_error.SyntacticError {
        err_msg = [ "_SyntaxError" ];
        err_info = [];
        err_ctx = {
          location = {
            start = Lexing.lexeme_start lexbuf;
            enddd = Lexing.lexeme_end lexbuf;
            filename = filename;
          };
        };
      }
    )
;;

