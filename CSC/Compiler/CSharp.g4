grammar CSharp;

source_file     : root_expr+ ;
root_expr       : using_statement | namespace_declaration ;



using_statement : 'using' qualified_identifier ';' ;

namespace_declaration
    : 'namespace' qualified_identifier namespace_body
    ;

qualified_identifier
    : IDENTIFIER ('.' IDENTIFIER)*
    ;

namespace_body
    : '{' namespace_member_declaration* '}'
    ;

namespace_member_declaration : type_declaration ;


type_declaration : class_declaration ;


class_declaration
    : class_modifier* 'partial'? 'class' IDENTIFIER
      class_base? class_body ';'?
    ;

class_base
    : ':' 'TODO'
    ;

class_body
    : '{' class_member_declaration* '}'
    ;


class_member_declaration
    : method_declaration
    ;

method_declaration
    : method_header method_body
    ;

method_body
    : block
    | ';'
    ;

block
    : '{' statement_list? '}'
    ;

statement_list
    : statement+
    ;

//~~~~~~~~~~~~~~

statement
    : ';'
    | qualified_identifier '(' STRING ')' ';'
    ;

STRING : '"' ~["]* '"' ;


//~~~~~~~~~~~~~~

method_header
    : method_modifier* 'partial'? return_type member_name
      '(' formal_parameter_list? ')'
    ;

formal_parameter_list
    : fixed_parameters
    ;

fixed_parameters
    : fixed_parameter (',' fixed_parameter)*
    ;

fixed_parameter
    : type IDENTIFIER
    ;

member_name
    : IDENTIFIER
    ;

return_type : type ;

type
    : 'string[]'
    | 'void'
    ;

method_modifier
    : 'new'
    | 'public'
    | 'protected'
    | 'internal'
    | 'private'
    | 'static'
    | 'virtual'
    | 'sealed'
    | 'override'
    | 'abstract'
    | 'extern'
    | 'async'
    ;

class_modifier
    : 'new'
    | 'public'
    | 'protected'
    | 'internal'
    | 'private'
    | 'abstract'
    | 'sealed'
    | 'static'
    ;

IDENTIFIER : IDENTIFIER_STARTING_CHAR IDENTIFIER_PART_CHAR* ;

fragment
IDENTIFIER_STARTING_CHAR : [_a-zA-Z] ;
fragment
IDENTIFIER_PART_CHAR     : [_a-zA-Z0-9] ;


NEWLINE  :'\r'? '\n' -> skip ;
WS       :   [ \t]+  -> skip ;
