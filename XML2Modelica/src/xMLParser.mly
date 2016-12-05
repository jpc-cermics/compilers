/*
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
 */

/*
 * Parser
 * Grammar for XML to Modelica translation
 */

%{

  open XMLTree

%}

/*names*/
%token <string> IDENT

/*literals*/
%token <string> UNSIGNED_INTEGER UNSIGNED_NUMBER STRING

/*keywords*/
%token MODEL_TAG MODEL_END_TAG NAME_TAG NAME_END_TAG ELEMENTS_TAG ELEMENT_STRUCT_TAG
%token ELEMENT_STRUCT_SUBNODES_TAG ELEMENT_STRUCT_SUBNODES_END_TAG ELEMENT_STRUCT_END_TAG
%token ELEMENT_TERMINAL_TAG ELEMENT_TERMINAL_KIND_TAG ELEMENT_TERMINAL_KIND_END_TAG
%token ELEMENT_TERMINAL_ID_TAG ELEMENT_TERMINAL_ID_END_TAG
%token ELEMENT_TERMINAL_END_TAG ELEMENTS_END_TAG
%token EQUATIONS_TAG EQUATIONS_END_TAG WHEN_CLAUSES_TAG WHEN_CLAUSES_END_TAG
%token ELEMENT_TERMINAL_OUTPUT_TAG ELEMENT_TERMINAL_SELECT_TAG
%token <string> ELEMENT_TERMINAL_COMMENT_VALUE_TAG ELEMENT_TERMINAL_INITIAL_VALUE_VALUE_TAG
%token <string> ELEMENT_TERMINAL_NOMINAL_VALUE_VALUE_TAG ELEMENT_TERMINAL_FIXED_VALUE_TAG
%token <string> EQUATION_VALUE_TAG WHEN_CLAUSE_VALUE_TAG

/* Error */
%token ERROR

/*end of file*/
%token EOF

%type <XMLTree.t> xML_definition_eof
%start xML_definition_eof

%%

/*(0.0)*/
xML_definition_eof
    : definition EOF                                            { $1 }
    ;

definition
    : MODEL_TAG model_definition MODEL_END_TAG                  { $2 }
    ;

model_definition
    : name elements_definition equations_definition when_clauses_definition
                                                                { { name = $1; elements = $2; equations = $3; when_clauses = $4 } }
    ;

elements_definition
    : ELEMENTS_TAG element_list ELEMENTS_END_TAG                { List.rev $2 }
    ;

element_list
    :                                                           { [] }
    | element_list element_definition                           { $2 :: $1 }
    ;

element_definition
    : ELEMENT_STRUCT_TAG struct_contents ELEMENT_STRUCT_END_TAG { Struct $2 }
    | ELEMENT_TERMINAL_TAG terminal_contents
      ELEMENT_TERMINAL_END_TAG                                  { Terminal $2 }
    ;

struct_contents
    : name subnodes_definition                                  { { struct_name = $1; subnodes = $2 } }
    ;

name
    : NAME_TAG IDENT NAME_END_TAG                                   { $2 }
    ;

subnodes_definition
    : ELEMENT_STRUCT_SUBNODES_TAG element_list
      ELEMENT_STRUCT_SUBNODES_END_TAG                           { List.rev $2 }
    ;

terminal_contents
    : name kind id fixed_value initial_value_value nominal_value_value comment_value output
      select                                                    
                                          { { terminal_name = $1;
                                              kind = $2;
                                              id = $3; 
                                              comment = $7;
                                              initial_value = $5;
                                              nominal = $6;
                                              output = $8;
                                              select = $9;
                                              fixed = $4 } }
    ;

kind
    : ELEMENT_TERMINAL_KIND_TAG IDENT
      ELEMENT_TERMINAL_KIND_END_TAG                             { match $2 with
                                                                    | "input" -> Input
                                                                    | "fixed_parameter" -> FixedParameter
                                                                    | "parameter" -> Parameter
                                                                    | "variable" -> Variable
                                                                    | "discrete_variable" -> DiscreteVariable
                                                                    | _ -> raise Parsing.Parse_error }
    ;

id
    : ELEMENT_TERMINAL_ID_TAG IDENT
      ELEMENT_TERMINAL_ID_END_TAG                               { $2 }
    ;

comment_value
    :                                                           { "" }
    | ELEMENT_TERMINAL_COMMENT_VALUE_TAG                        { $1 }
    ;

initial_value_value
    :                                                           { "" }
    | ELEMENT_TERMINAL_INITIAL_VALUE_VALUE_TAG                  { $1 }
    ;

fixed_value
    :                                                           { "" }
    | ELEMENT_TERMINAL_FIXED_VALUE_TAG                          { $1 }
    ;

nominal_value_value
    :                                                           { "" }
    | ELEMENT_TERMINAL_NOMINAL_VALUE_VALUE_TAG                  { $1 }
    ;

output
    :                                                           { false }
    | ELEMENT_TERMINAL_OUTPUT_TAG                               { true }
    ;

select
    :                                                           { false }
    | ELEMENT_TERMINAL_SELECT_TAG                               { true }
    ;

equations_definition
    : EQUATIONS_TAG equation_list EQUATIONS_END_TAG             { List.rev $2 }
    ;

equation_list
    :                                                           { [] }
    | equation_list equation_definition                         { $2 :: $1 }
    ;

equation_definition
    : EQUATION_VALUE_TAG                                        { $1 }
    ;

when_clauses_definition
    :                                                           { [] }
    | WHEN_CLAUSES_TAG when_clause_list WHEN_CLAUSES_END_TAG    { List.rev $2 }
    ;

when_clause_list
    :                                                           { [] }
    | when_clause_list when_clause_definition                   { $2 :: $1 }
    ;

when_clause_definition
    : WHEN_CLAUSE_VALUE_TAG                                     { $1 }
    ;

%%
