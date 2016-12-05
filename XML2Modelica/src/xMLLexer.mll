(*
 *  XML to Modelica
 *
 *  Copyright (C) 2005 - 2007 Imagine S.A.
 *  For more information or commercial use please contact us at www.amesim.com
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 *)

{

open XMLParser

exception Eof

}

let blank = [' ' '\t' '\r']
let digit = ['0'-'9']
let nondigit = ['_' 'A'-'Z' 'a'-'z' '[' ']']
let schar = [^'\"' '\\']
let sescape = "\\\'" | "\\\"" | "\\?" | "\\\\" | "\\a" | "\\b" | "\\f" |
              "\\n" | "\\r" | "\\t" | "\\v"

let unsigned_integer = digit+

let unsigned_number =
  unsigned_integer ('.' unsigned_integer?)?
    (('e' | 'E')('+' | '-')? unsigned_integer)?

let end_value_tag = blank+ "value" blank* '=' blank* '\"' (schar | sescape)* '\"' blank* "/>"

rule token = parse

    | blank
                { token lexbuf }

    | ['\n']
                { token lexbuf }


    | unsigned_integer
                { let lxm = Lexing.lexeme lexbuf in
                    UNSIGNED_INTEGER lxm }

    | unsigned_number
                { let lxm = Lexing.lexeme lexbuf in
                    UNSIGNED_NUMBER lxm }

    | (nondigit (nondigit | digit)* '.')* nondigit (nondigit | digit)*
                { let lxm = Lexing.lexeme lexbuf in
                    IDENT lxm }

    | '\"' (schar | sescape)* '\"'
                { let lxm = Lexing.lexeme lexbuf in
                    STRING (String.sub lxm 1 (String.length lxm - 2)) }

    | "<model>"  { let _ = Lexing.lexeme lexbuf in MODEL_TAG }
      | "<name>"  { let _ = Lexing.lexeme lexbuf in NAME_TAG }
      | "</name>"  { let _ = Lexing.lexeme lexbuf in NAME_END_TAG }
      | "<elements>"  { let _ = Lexing.lexeme lexbuf in ELEMENTS_TAG }
        | "<struct>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_STRUCT_TAG }
          | "<subnodes>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_STRUCT_SUBNODES_TAG }
          | "</subnodes>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_STRUCT_SUBNODES_END_TAG }
        | "</struct>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_STRUCT_END_TAG }
        | "<terminal>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_TAG }
          | "<kind>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_KIND_TAG }
          | "</kind>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_KIND_END_TAG }
          | "<id>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_ID_TAG }
          | "</id>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_ID_END_TAG }
          | "<fixed" blank+ { let _ = Lexing.lexeme lexbuf in element_fixed_value lexbuf }
          | "<initial_value" blank+ { let _ = Lexing.lexeme lexbuf in element_terminal_initial_value_value lexbuf }
          | "<weight" end_value_tag { token lexbuf }
          | "<max" end_value_tag { token lexbuf }
          | "<min" end_value_tag { token lexbuf }
          | "<fixed_orig>" (schar | sescape)* "</fixed_orig>"  { token lexbuf }
          | "<comment" blank+ { let _ = Lexing.lexeme lexbuf in element_terminal_comment_value lexbuf }
          | "<nominal_value" blank+ { let _ = Lexing.lexeme lexbuf in element_terminal_nominal_value_value lexbuf }
          | "<output/>" { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_OUTPUT_TAG }
          | "<selected" blank+ "value" blank* '=' blank* "y" blank* "/>" { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_SELECT_TAG }
          | "<selected" end_value_tag { token lexbuf }
        | "</terminal>"  { let _ = Lexing.lexeme lexbuf in ELEMENT_TERMINAL_END_TAG }
      | "</elements>"  { let _ = Lexing.lexeme lexbuf in ELEMENTS_END_TAG }
      | "<equations>"  { let _ = Lexing.lexeme lexbuf in EQUATIONS_TAG }
        | "<equation" blank+ { let _ = Lexing.lexeme lexbuf in equation_value lexbuf }
      | "</equations>"  { let _ = Lexing.lexeme lexbuf in EQUATIONS_END_TAG }
      | "<when_clauses/>" { token lexbuf }
      | "<when_clauses>"  { let _ = Lexing.lexeme lexbuf in WHEN_CLAUSES_TAG }
        | "<when_clause" blank+ { let _ = Lexing.lexeme lexbuf in when_clause_value lexbuf }
      | "</when_clauses>"  { let _ = Lexing.lexeme lexbuf in WHEN_CLAUSES_END_TAG }
    | "</model>"  { let _ = Lexing.lexeme lexbuf in MODEL_END_TAG }

    | eof           { EOF }

    | _             { ERROR }

and element_terminal_comment_value = parse

    | "value" blank* '=' blank* '\"' [^ '\"']* '\"' blank* "/>"
                { let lxm = Lexing.lexeme lexbuf in
                  let i = String.index lxm '\"'
                  and j = String.rindex lxm '\"' in
                  ELEMENT_TERMINAL_COMMENT_VALUE_TAG (String.sub lxm (i + 1) (j - i - 1)) }
    | eof           { EOF }

    | _             { ERROR }

and element_terminal_initial_value_value = parse

    | "value" blank* '=' blank* '\"' (schar | sescape)* '\"' blank* "/>"
                { let lxm = Lexing.lexeme lexbuf in
                  let i = String.index lxm '\"'
                  and j = String.rindex lxm '\"' in
                  ELEMENT_TERMINAL_INITIAL_VALUE_VALUE_TAG (String.sub lxm (i + 1) (j - i - 1)) }
    | eof           { EOF }

    | _             { ERROR }

and element_fixed_value = parse

    | "value" blank* '=' blank* '\"' (schar | sescape)* '\"' blank* "/>"
                { let lxm = Lexing.lexeme lexbuf in
                  let i = String.index lxm '\"'
                  and j = String.rindex lxm '\"' in
                  ELEMENT_TERMINAL_FIXED_VALUE_TAG (String.sub lxm (i + 1) (j - i - 1)) }
    | eof           { EOF }

    | _             { ERROR }

and element_terminal_nominal_value_value = parse

    | "value" blank* '=' blank* '\"' (schar | sescape)* '\"' blank* "/>"
                { let lxm = Lexing.lexeme lexbuf in
                  let i = String.index lxm '\"'
                  and j = String.rindex lxm '\"' in
                  ELEMENT_TERMINAL_NOMINAL_VALUE_VALUE_TAG (String.sub lxm (i + 1) (j - i - 1)) }
    | eof           { EOF }

    | _             { ERROR }

and equation_value = parse

    | "value" blank* '=' blank* '\"' (schar | sescape)* '\"' blank* "/>"
                { let lxm = Lexing.lexeme lexbuf in
                  let i = String.index lxm '\"'
                  and j = String.rindex lxm '\"' in
                  EQUATION_VALUE_TAG (String.sub lxm (i + 1) (j - i - 1)) }
    | eof           { EOF }

    | _             { ERROR }

and when_clause_value = parse

    | "value" blank* '=' blank* '\"' (schar | sescape)* '\"' blank* "/>"
                { let lxm = Lexing.lexeme lexbuf in
                  let i = String.index lxm '\"'
                  and j = String.rindex lxm '\"' in
                  WHEN_CLAUSE_VALUE_TAG (String.sub lxm (i + 1) (j - i - 1)) }
    | eof           { EOF }

    | _             { ERROR }
