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
    | embedded_statement
    ;

embedded_statement
    : jump_statement
    ;

jump_statement
    : return_statement
    ;

return_statement
    : 'return' expression? ';'
    ;

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#expression
expression
    : non_assignment_expression
    | assignment
    ;

non_assignment_expression
    : conditional_expression
    | lambda_expression
    | query_expression
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#constant-expressions
constant_expression
    : expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#assignment-operators
assignment
    : unary_expression assignment_operator expression
    ;

assignment_operator
    : '='
    | '+='
    | '-='
    | '*='
    | '/='
    | '%='
    | '&='
    | '|='
    | '^='
    | '<<='
    | right_shift_assignment
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#conditional-operator
conditional_expression
    : null_coalescing_expression
    | null_coalescing_expression '?' expression ':' expression
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#anonymous-function-expressions
lambda_expression
    : anonymous_function_signature '=>' anonymous_function_body
    ;

anonymous_method_expression
    : 'delegate' explicit_anonymous_function_signature? block
    ;

anonymous_function_signature
    : explicit_anonymous_function_signature
    | implicit_anonymous_function_signature
    ;

explicit_anonymous_function_signature
    : '(' explicit_anonymous_function_parameter_list? ')'
    ;

explicit_anonymous_function_parameter_list
    : explicit_anonymous_function_parameter (',' explicit_anonymous_function_parameter)*
    ;

explicit_anonymous_function_parameter
    : anonymous_function_parameter_modifier? type identifier
    ;

anonymous_function_parameter_modifier
    : 'ref'
    | 'out'
    ;

implicit_anonymous_function_signature
    : '(' implicit_anonymous_function_parameter_list? ')'
    | implicit_anonymous_function_parameter
    ;

implicit_anonymous_function_parameter_list
    : implicit_anonymous_function_parameter (',' implicit_anonymous_function_parameter)*
    ;

implicit_anonymous_function_parameter
    : identifier
    ;

anonymous_function_body
    : expression
    | block
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#query-expressions
query_expression
    : from_clause query_body
    ;

from_clause
    : 'from' type? identifier 'in' expression
    ;

query_body
    : query_body_clauses? select_or_group_clause query_continuation?
    ;

query_body_clauses
    : query_body_clause
    | query_body_clauses query_body_clause
    ;

query_body_clause
    : from_clause
    | let_clause
    | where_clause
    | join_clause
    | join_into_clause
    | orderby_clause
    ;

let_clause
    : 'let' identifier '=' expression
    ;

where_clause
    : 'where' boolean_expression
    ;

join_clause
    : 'join' type? identifier 'in' expression 'on' expression 'equals' expression
    ;

join_into_clause
    : 'join' type? identifier 'in' expression 'on' expression 'equals' expression 'into' identifier
    ;

orderby_clause
    : 'orderby' orderings
    ;

orderings
    : ordering (',' ordering)*
    ;

ordering
    : expression ordering_direction?
    ;

ordering_direction
    : 'ascending'
    | 'descending'
    ;

select_or_group_clause
    : select_clause
    | group_clause
    ;

select_clause
    : 'select' expression
    ;

group_clause
    : 'group' expression 'by' expression
    ;

query_continuation
    : 'into' identifier query_body
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#unary-operators
unary_expression
    : primary_expression
    | null_conditional_expression
    | '+' unary_expression
    | '-' unary_expression
    | '!' unary_expression
    | '~' unary_expression
    | pre_increment_expression
    | pre_decrement_expression
    | cast_expression
    | await_expression
    | unary_expression_unsafe
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#operators-and-punctuators
operator_or_punctuator
    : '{'  | '}'  | '['  | ']'  | '('   | ')'  | '.'  | ','  | ':'  | ';'
    | '+'  | '-'  | '*'  | '/'  | '%'   | '&'  | '|'  | '^'  | '!'  | '~'
    | '='  | '<'  | '>'  | '?'  | '??'  | '::' | '++' | '--' | '&&' | '||'
    | '->' | '==' | '!=' | '<=' | '>='  | '+=' | '-=' | '*=' | '/=' | '%='
    | '&=' | '|=' | '^=' | '<<' | '<<=' | '=>'
    ;

right_shift
    : '>>'
    ;

right_shift_assignment
    : '>>='
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#the-null-coalescing-operator
null_coalescing_expression
    : conditional_or_expression
    | conditional_or_expression '??' null_coalescing_expression
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#identifiers
identifier
    : available_identifier
    | '@' identifier_or_keyword
    ;

available_identifier
    : '<An identifier_or_keyword that is not a keyword>'
    ;

identifier_or_keyword
    : identifier_start_character identifier_part_character*
    ;

identifier_start_character
    : letter_character
    | '_'
    ;

identifier_part_character
    : letter_character
    | decimal_digit_character
    | connecting_character
    | combining_character
    | formatting_character
    ;

letter_character
    : '<A Unicode character of classes Lu, Ll, Lt, Lm, Lo, or Nl>'
    | '<A unicode_escape_sequence representing a character of classes Lu, Ll, Lt, Lm, Lo, or Nl>'
    ;

combining_character
    : '<A Unicode character of classes Mn or Mc>'
    | '<A unicode_escape_sequence representing a character of classes Mn or Mc>'
    ;

decimal_digit_character
    : '<A Unicode character of the class Nd>'
    | '<A unicode_escape_sequence representing a character of the class Nd>'
    ;

connecting_character
    : '<A Unicode character of the class Pc>'
    | '<A unicode_escape_sequence representing a character of the class Pc>'
    ;

formatting_character
    : '<A Unicode character of the class Cf>'
    | '<A unicode_escape_sequence representing a character of the class Cf>'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#boolean-expressions
boolean_expression
    : expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#primary-expressions
primary_expression
    : primary_no_array_creation_expression
    | array_creation_expression
    ;

primary_no_array_creation_expression
    : literal
    | interpolated_string_expression
    | simple_name
    | parenthesized_expression
    | member_access
    | invocation_expression
    | element_access
    | this_access
    | base_access
    | post_increment_expression
    | post_decrement_expression
    | object_creation_expression
    | delegate_creation_expression
    | anonymous_object_creation_expression
    | typeof_expression
    | checked_expression
    | unchecked_expression
    | default_value_expression
    | nameof_expression
    | anonymous_method_expression
    | primary_no_array_creation_expression_unsafe
    ;
// END

// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#null-conditional-operator
null_conditional_expression
    : primary_expression null_conditional_operations
    ;

null_conditional_operations
    : null_conditional_operations? '?' '.' identifier type_argument_list?
    | null_conditional_operations? '?' '[' argument_list ']'
    | null_conditional_operations '.' identifier type_argument_list?
    | null_conditional_operations '[' argument_list ']'
    | null_conditional_operations '(' argument_list? ')'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#prefix-increment-and-decrement-operators
pre_increment_expression
    : '++' unary_expression
    ;

pre_decrement_expression
    : '--' unary_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#cast-expressions
cast_expression
    : '(' type ')' unary_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#await-expressions
await_expression
    : 'await' unary_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#pointers-in-expressions
primary_no_array_creation_expression_unsafe
    : pointer_member_access
    | pointer_element_access
    | sizeof_expression
    ;

unary_expression_unsafe
    : pointer_indirection_expression
    | addressof_expression
    ;
// END



// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#conditional-logical-operators
conditional_and_expression
    : inclusive_or_expression
    | conditional_and_expression '&&' inclusive_or_expression
    ;

conditional_or_expression
    : conditional_and_expression
    | conditional_or_expression '||' conditional_and_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#array-creation-expressions
array_creation_expression
    : 'new' non_array_type '[' expression_list ']' rank_specifier* array_initializer?
    | 'new' array_type array_initializer
    | 'new' rank_specifier array_initializer
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#literals
literal
    : boolean_literal
    | integer_literal
    | real_literal
    | character_literal
    | string_literal
    | null_literal
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#interpolated-strings
interpolated_string_expression
    : '$' interpolated_regular_string
    | '$' interpolated_verbatim_string
    ;

interpolated_regular_string
    : interpolated_regular_string_whole
    | interpolated_regular_string_start interpolated_regular_string_body interpolated_regular_string_end
    ;

interpolated_regular_string_body
    : interpolation (interpolated_regular_string_mid interpolation)*
    ;

interpolation
    : expression
    | expression ',' constant_expression
    ;

interpolated_verbatim_string
    : interpolated_verbatim_string_whole
    | interpolated_verbatim_string_start interpolated_verbatim_string_body interpolated_verbatim_string_end
    ;

interpolated_verbatim_string_body
    : interpolation (interpolated_verbatim_string_mid interpolation)+
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#simple-names
simple_name
    : identifier type_argument_list?
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#parenthesized-expressions
parenthesized_expression
    : '(' expression ')'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#member-access
member_access
    : primary_expression '.' identifier type_argument_list?
    | predefined_type '.' identifier type_argument_list?
    | qualified_alias_member '.' identifier
    ;

predefined_type
    : 'bool'   | 'byte'  | 'char'  | 'decimal' | 'double' | 'float' | 'int' | 'long'
    | 'object' | 'sbyte' | 'short' | 'string'  | 'uint'   | 'ulong' | 'ushort'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#invocation-expressions
invocation_expression
    : primary_expression '(' argument_list? ')'
    ;

// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#element-access
element_access
    : primary_no_array_creation_expression '[' expression_list ']'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#this-access
this_access
    : 'this'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#base-access
base_access
    : 'base' '.' identifier
    | 'base' '[' expression_list ']'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#postfix-increment-and-decrement-operators
post_increment_expression
    : primary_expression '++'
    ;

post_decrement_expression
    : primary_expression '--'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#object-creation-expressions
object_creation_expression
    : 'new' type '(' argument_list? ')' object_or_collection_initializer?
    | 'new' type object_or_collection_initializer
    ;

object_or_collection_initializer
    : object_initializer
    | collection_initializer
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#delegate-creation-expressions
delegate_creation_expression
    : 'new' delegate_type '(' expression ')'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#anonymous-object-creation-expressions
anonymous_object_creation_expression
    : 'new' anonymous_object_initializer
    ;

anonymous_object_initializer
    : '{' member_declarator_list? '}'
    | '{' member_declarator_list ',' '}'
    ;

member_declarator_list
    : member_declarator (',' member_declarator)*
    ;

member_declarator
    : simple_name
    | member_access
    | base_access
    | null_conditional_member_access
    | identifier '=' expression
    ;

// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#the-typeof-operator
typeof_expression
    : 'typeof' '(' type ')'
    | 'typeof' '(' unbound_type_name ')'
    | 'typeof' '(' 'void' ')'
    ;

unbound_type_name
    : identifier generic_dimension_specifier?
    | identifier '::' identifier generic_dimension_specifier?
    | unbound_type_name '.' identifier generic_dimension_specifier?
    ;

generic_dimension_specifier
    : '<' comma* '>'
    ;

comma
    : ','
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#the-checked-and-unchecked-operators
checked_expression
    : 'checked' '(' expression ')'
    ;

unchecked_expression
    : 'unchecked' '(' expression ')'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#default-value-expressions
default_value_expression
    : 'default' '(' type ')'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#nameof-expressions
nameof_expression
    : 'nameof' '(' named_entity ')'
    ;

named_entity
    : simple_name
    | named_entity_target '.' identifier type_argument_list?
    ;

named_entity_target
    : 'this'
    | 'base'
    | named_entity 
    | predefined_type 
    | qualified_alias_member
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/types#type-arguments
type_argument_list
    : '<' type_arguments '>'
    ;

type_arguments
    : type_argument (',' type_argument)*
    ;

type_argument
    : type
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#argument-lists
argument_list
    : argument (',' argument)*
    ;

argument
    : argument_name? argument_value
    ;

argument_name
    : identifier ':'
    ;

argument_value
    : expression
    | 'ref' variable_reference
    | 'out' variable_reference
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#pointer-member-access
pointer_member_access
    : primary_expression '->' identifier
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#pointer-element-access
pointer_element_access
    : primary_no_array_creation_expression '[' expression ']'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#the-sizeof-operator
sizeof_expression
    : 'sizeof' '(' unmanaged_type ')'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#pointer-indirection
pointer_indirection_expression
    : '*' unary_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#the-address-of-operator
addressof_expression
    : '&' unary_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#logical-operators
and_expression
    : equality_expression
    | and_expression '&' equality_expression
    ;

exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression
    ;

inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/types#reference-types
reference_type
    : class_type
    | interface_type
    | array_type
    | delegate_type
    ;

class_type
    : type_name
    | 'object'
    | 'dynamic'
    | 'string'
    ;

interface_type
    : type_name
    ;

array_type
    : non_array_type rank_specifier+
    ;

non_array_type
    : type
    ;

rank_specifier
    : '[' dim_separator* ']'
    ;

dim_separator
    : ','
    ;

delegate_type
    : type_name
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#collection-initializers
collection_initializer
    : '{' element_initializer_list '}'
    | '{' element_initializer_list ',' '}'
    ;

element_initializer_list
    : element_initializer (',' element_initializer)*
    ;

element_initializer
    : non_assignment_expression
    | '{' expression_list '}'
    ;

expression_list
    : expression (',' expression)*
    ;

// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/arrays#array-initializers
array_initializer
    : '{' variable_initializer_list? '}'
    | '{' variable_initializer_list ',' '}'
    ;

variable_initializer_list
    : variable_initializer (',' variable_initializer)*
    ;

variable_initializer
    : expression
    | array_initializer
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#boolean-literals
boolean_literal
    : 'true'
    | 'false'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#integer-literals
integer_literal
    : decimal_integer_literal
    | hexadecimal_integer_literal
    ;

decimal_integer_literal
    : decimal_digit+ integer_type_suffix?
    ;

decimal_digit
    : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    ;

integer_type_suffix
    : 'U' | 'u' | 'L' | 'l' | 'UL' | 'Ul' | 'uL' | 'ul' | 'LU' | 'Lu' | 'lU' | 'lu'
    ;

hexadecimal_integer_literal
    : '0x' hex_digit+ integer_type_suffix?
    | '0X' hex_digit+ integer_type_suffix?
    ;

hex_digit
    : '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f';

// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#real-literals
real_literal
    : decimal_digit+ '.' decimal_digit+ exponent_part? real_type_suffix?
    | '.' decimal_digit+ exponent_part? real_type_suffix?
    | decimal_digit+ exponent_part real_type_suffix?
    | decimal_digit+ real_type_suffix
    ;

exponent_part
    : 'e' sign? decimal_digit+
    | 'E' sign? decimal_digit+
    ;

sign
    : '+'
    | '-'
    ;

real_type_suffix
    : 'F' | 'f' | 'D' | 'd' | 'M' | 'm'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#character-literals
character_literal
    : '\'' character '\''
    ;

character
    : single_character
    | simple_escape_sequence
    | hexadecimal_escape_sequence
    | unicode_escape_sequence
    ;

single_character
    : '<Any character except \' (U+0027), \\ (U+005C), and new_line_character>'
    ;

simple_escape_sequence
    : '\\\'' | '\\"' | '\\\\' | '\\0' | '\\a' | '\\b' | '\\f' | '\\n' | '\\r' | '\\t' | '\\v'
    ;

hexadecimal_escape_sequence
    : '\\x' hex_digit hex_digit? hex_digit? hex_digit?;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#string-literals
string_literal
    : regular_string_literal
    | verbatim_string_literal
    ;

regular_string_literal
    : '"' regular_string_literal_character* '"'
    ;

regular_string_literal_character
    : single_regular_string_literal_character
    | simple_escape_sequence
    | hexadecimal_escape_sequence
    | unicode_escape_sequence
    ;

single_regular_string_literal_character
    : '<Any character except " (U+0022), \\ (U+005C), and new_line_character>'
    ;

verbatim_string_literal
    : '@"' verbatim_string_literal_character* '"'
    ;

verbatim_string_literal_character
    : single_verbatim_string_literal_character
    | quote_escape_sequence
    ;

single_verbatim_string_literal_character
    : '<any character except ">'
    ;

quote_escape_sequence
    : '""'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#the-null-literal
null_literal
    : 'null'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#interpolated-string-literals
interpolated_string_literal
    : '$' interpolated_regular_string_literal
    | '$' interpolated_verbatim_string_literal
    ;

interpolated_regular_string_literal
    : interpolated_regular_string_whole
    | interpolated_regular_string_start  interpolated_regular_string_literal_body interpolated_regular_string_end
    ;

interpolated_regular_string_literal_body
    : regular_balanced_text
    | interpolated_regular_string_literal_body interpolated_regular_string_mid regular_balanced_text
    ;

interpolated_regular_string_whole
    : '"' interpolated_regular_string_character* '"'
    ;

interpolated_regular_string_start
    : '"' interpolated_regular_string_character* '{'
    ;

interpolated_regular_string_mid
    : interpolation_format? '}' interpolated_regular_string_characters_after_brace? '{'
    ;

interpolated_regular_string_end
    : interpolation_format? '}' interpolated_regular_string_characters_after_brace? '"'
    ;

interpolated_regular_string_characters_after_brace
    : interpolated_regular_string_character_no_brace
    | interpolated_regular_string_characters_after_brace interpolated_regular_string_character
    ;

interpolated_regular_string_character
    : single_interpolated_regular_string_character
    | simple_escape_sequence
    | hexadecimal_escape_sequence
    | unicode_escape_sequence
    | open_brace_escape_sequence
    | close_brace_escape_sequence
    ;

interpolated_regular_string_character_no_brace
    : single_interpolated_regular_string_character
    | simple_escape_sequence
    | open_brace_escape_sequence
    ;

single_interpolated_regular_string_character
    :  ~('"' | '\\' | '{' | '}' | NEW_LINE_CHARACTER)  // '<Any character except \" (U+0022), \\ (U+005C), { (U+007B), } (U+007D), and new_line_character>'
    ;

open_brace_escape_sequence
    : '{{'
    ;

close_brace_escape_sequence
    : '}}'
    ;
    
regular_balanced_text
    : regular_balanced_text_part+
    ;

regular_balanced_text_part
    : single_regular_balanced_text_character
    | delimited_comment
    | '@' identifier_or_keyword
    | string_literal
    | interpolated_string_literal
    | '(' regular_balanced_text ')'
    | '[' regular_balanced_text ']'
    | '{' regular_balanced_text '}'
    ;
    
single_regular_balanced_text_character
    : ~('/' | '@' | '"' | '$' | '(' | ')' | '[' | ']' | '{' | '}' | NEW_LINE_CHARACTER) // '<Any character except / (U+002F), @ (U+0040), \" (U+0022), $ (U+0024), ( (U+0028), ) (U+0029), [ (U+005B), ] (U+005D), { (U+007B), } (U+007D) and new_line_character>'
    | '/' ~('/' | '*') // '</ (U+002F), if not directly followed by / (U+002F) or * (U+002A)>'
    ;
    
interpolation_format
    : ':' interpolation_format_character+
    ;
    
interpolation_format_character
    :  ~('"' | ':' | '{' | '}') // '<Any character except \" (U+0022), : (U+003A), { (U+007B) and } (U+007D)>'
    ;
    
interpolated_verbatim_string_literal
    : interpolated_verbatim_string_whole
    | interpolated_verbatim_string_start interpolated_verbatim_string_literal_body interpolated_verbatim_string_end
    ;

interpolated_verbatim_string_literal_body
    : verbatim_balanced_text
    | interpolated_verbatim_string_literal_body interpolated_verbatim_string_mid verbatim_balanced_text
    ;
    
interpolated_verbatim_string_whole
    : '@"' interpolated_verbatim_string_character* '"'
    ;
    
interpolated_verbatim_string_start
    : '@"' interpolated_verbatim_string_character* '{'
    ;
    
interpolated_verbatim_string_mid
    : interpolation_format? '}' interpolated_verbatim_string_characters_after_brace? '{'
    ;
    
interpolated_verbatim_string_end
    : interpolation_format? '}' interpolated_verbatim_string_characters_after_brace? '"'
    ;
    
interpolated_verbatim_string_characters_after_brace
    : interpolated_verbatim_string_character_no_brace
    | interpolated_verbatim_string_characters_after_brace interpolated_verbatim_string_character
    ;
    
interpolated_verbatim_string_character
    : single_interpolated_verbatim_string_character
    | quote_escape_sequence
    | open_brace_escape_sequence
    | close_brace_escape_sequence
    ;
    
interpolated_verbatim_string_character_no_brace
    : single_interpolated_verbatim_string_character
    | quote_escape_sequence
    | open_brace_escape_sequence
    ;
    
single_interpolated_verbatim_string_character
    :  ~('"' | '{' | '}')  // '<Any character except \" (U+0022), { (U+007B) and } (U+007D)>'
    ;
    
verbatim_balanced_text
    : verbatim_balanced_text_part+
    ;

verbatim_balanced_text_part
    : single_verbatim_balanced_text_character
    | comment
    | '@' identifier_or_keyword
    | string_literal
    | interpolated_string_literal
    | '(' verbatim_balanced_text ')'
    | '[' verbatim_balanced_text ']'
    | '{' verbatim_balanced_text '}'
    ;
    
single_verbatim_balanced_text_character
    : ~('/' | '@' | '"' | '$' | '(' | ')' | '[' | ']' | '{' | '}') // '<Any character except / (U+002F), @ (U+0040), \" (U+0022), $ (U+0024), ( (U+0028), ) (U+0029), [ (U+005B), ] (U+005D), { (U+007B) and } (U+007D)>'
    | '/' ~('/' | '*') // '</ (U+002F), if not directly followed by / (U+002F) or * (U+002A)>'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#comments
comment
    : single_line_comment
    | delimited_comment
    ;

single_line_comment
    : '//' input_character*
    ;

input_character
    : '<Any Unicode character except a new_line_character>'
    ;

NEW_LINE_CHARACTER
    : '\u000D' // carriage return
    | '\u000A' // line feed
    | '\u0085' // new line character
    | '\u2028' // line separator
    | '\u2029' // paragraph separator
    ;

delimited_comment
    : '/*' delimited_comment_section* asterisk+ '/'
    ;

delimited_comment_section
    : '/'
    | asterisk* not_slash_or_asterisk
    ;

asterisk
    : '*'
    ;

not_slash_or_asterisk
    : '<Any Unicode character except / or *>'
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/namespaces#namespace-alias-qualifiers
qualified_alias_member
    : identifier '::' identifier type_argument_list?
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#object-initializers
object_initializer
    : '{' member_initializer_list? '}'
    | '{' member_initializer_list ',' '}'
    ;

member_initializer_list
    : member_initializer (',' member_initializer)*
    ;

member_initializer
    : initializer_target '=' initializer_value
    ;

initializer_target
    : identifier
    | '[' argument_list ']'
    ;

initializer_value
    : expression
    | object_or_collection_initializer
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#null-conditional-expressions-as-projection-initializers
null_conditional_member_access
    : primary_expression null_conditional_operations? '?' '.' identifier type_argument_list?
    | primary_expression null_conditional_operations '.' identifier type_argument_list?
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/variables#variable-references
variable_reference
    : expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/unsafe-code#pointer-types
type_unsafe
    : pointer_type
    ;

pointer_type
    : unmanaged_type '*'
    | 'void' '*'
    ;

unmanaged_type
    : type
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#relational-and-type-testing-operators
relational_expression
    : shift_expression
    | relational_expression '<' shift_expression
    | relational_expression '>' shift_expression
    | relational_expression '<=' shift_expression
    | relational_expression '>=' shift_expression
    | relational_expression 'is' type
    | relational_expression 'as' type
    ;

equality_expression
    : relational_expression
    | equality_expression '==' relational_expression
    | equality_expression '!=' relational_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/basic-concepts#namespace-and-type-names
namespace_name
    : namespace_or_type_name
    ;

type_name
    : namespace_or_type_name
    ;

namespace_or_type_name
    : identifier type_argument_list?
    | namespace_or_type_name '.' identifier type_argument_list?
    | qualified_alias_member
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/lexical-structure#unicode-character-escape-sequences
unicode_escape_sequence
    : '\\u' hex_digit hex_digit hex_digit hex_digit
    | '\\U' hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit hex_digit
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#shift-operators
shift_expression
    : additive_expression
    | shift_expression '<<' additive_expression
    | shift_expression right_shift additive_expression
    ;
// END


// BEGIN: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#arithmetic-operators
multiplicative_expression
    : unary_expression
    | multiplicative_expression '*' unary_expression
    | multiplicative_expression '/' unary_expression
    | multiplicative_expression '%' unary_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
    | additive_expression '-' multiplicative_expression
    ;
// END








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
    | 'object'
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
