type token =
  | IDENT of (string)
  | END_IDENT of (string)
  | UNSIGNED_INTEGER of (string)
  | UNSIGNED_REAL of (string)
  | STRING of (string)
  | ALGORITHM
  | AND
  | ANNOTATION
  | BLOCK
  | BREAK
  | CLASS
  | CONNECT
  | CONNECTOR
  | CONSTANT
  | DISCRETE
  | EACH
  | ELSE
  | ELSEIF
  | ELSEWHEN
  | ENCAPSULATED
  | END
  | END_IF
  | END_FOR
  | END_WHEN
  | END_WHILE
  | ENUMERATION
  | EQUATION
  | EXPANDABLE
  | EXTENDS
  | EXTERNAL
  | FALSE
  | FINAL
  | FLOW
  | FOR
  | FUNCTION
  | IF
  | IMPORT
  | IN
  | INITIAL_ALGORITHM
  | INITIAL_EQUATION
  | INNER
  | INPUT
  | LOOP
  | MODEL
  | NOT
  | NOEVENT
  | OR
  | OUTER
  | OUTPUT
  | PACKAGE
  | PARAMETER
  | PARTIAL
  | PROTECTED
  | PUBLIC
  | RECORD
  | REDECLARE
  | REPLACEABLE
  | RESTRICTS
  | RETURN
  | THEN
  | TRUE
  | TYPE
  | WHEN
  | WHILE
  | WITHIN
  | LP
  | RP
  | LSB
  | RSB
  | LCB
  | RCB
  | DOT
  | CM
  | SC
  | CL
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EXP
  | EQ
  | COLEQ
  | LT
  | GT
  | LE
  | GE
  | EE
  | NE
  | EOF

val definition :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Location.t Syntax.toplevel_element_desc, Location.t) Syntax.node
