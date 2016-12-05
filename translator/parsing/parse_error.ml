exception Unclosed of int * string * int * string
exception Invalid_matrix of int * int
exception Invalid_array of int * int

type error_description =
  {
    err_msg: string list;
    err_info: (string * string) list;
    err_ctx: err_ctx
  }

and err_ctx =
  {
    location: Location.t;
  }

exception SyntacticError of error_description

