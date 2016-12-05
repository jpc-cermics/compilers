type t =
  {
    start: int; (* offset in the parsed stream *)
    enddd: int;  (* offset in the parsed stream *)
    filename: filename
  }

and filename =
  | LibraryFile of string
  | CommandLine

