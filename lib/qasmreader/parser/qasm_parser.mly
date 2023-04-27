/* 
 * Documentation:  
 *  - http://gallium.inria.fr/~fpottier/menhir/manual.html#sec2
 *  - https://arxiv.org/pdf/1707.03429.pdf
 *
 * Specification:
 * - https://arxiv.org/abs/1707.03429v2
 */
%{
open Abs
exception ParseError of string

let safe_int_of_float f =
    (* only then it is safe to convert float to int, however this should always be satsified here *)
    if f <= float_of_int max_int && f >= float_of_int min_int then
        int_of_float f
    else
        raise (ParseError (string_of_float f ^ " cannot converted to int"))
%}

/* entry point */
%start file
%type <statement list> file
/* only the type of the entry point MUST be specified, sometimes it helps to define also the type of other rules because
 * error messages are more informative then */
%type <operation> uop
%type <expression> exp

/* precedence */
%left PLUS MINUS
%left STAR SLASH
%right SIGN
%right CIRC

%%
/* Rules */
/* unary operation
 * even though it seems that the same type is returned as read, the input type (token) is different from the output type
 * (unary_operator in this case) */
unaryop:
    | MINUS
        { MINUS }
    | SIN
        { SIN }
    | COS
        { COS }
    | TAN
        { TAN }
    | EXP
        { EXP }
    | LN
        { LN }
    | SQRT
        { SQRT }
    ;

/* additive operation 
 * The following three rules do not exist in the original specification but are needed for the LR(1) parser to assign
 * meaningful precedences to them. */
additveop:
    | PLUS
        { ADD }
    | MINUS
        { SUB }
    ;

/* multiplicative operation */
multiplicativeop:
    | STAR
        { MUL }
    | SLASH
        { DIV }
    ;

/* exponential operation */
exponentialop:
    | CIRC
        { EXP }
    ;

/* expression */
exp:
    | FLOAT_CONST
        { CONSTANT (FLOAT_CONST $1) }
    | INT_CONST
        { CONSTANT (INT_CONST $1) }
    | PI
        { CONSTANT PI }
    | IDENTIFIER
        { VARIABLE $1 }
    | exp additveop exp %prec PLUS
        { BINARY ($2, $1, $3) }
    | exp multiplicativeop exp %prec STAR
        { BINARY ($2, $1, $3) }
    | exp exponentialop exp %prec CIRC
        { BINARY ($2, $1, $3) }
    | unaryop exp %prec SIGN
        { UNARY ($1, $2) }
    | LPAREN exp RPAREN
        { $2 }
    ;

/* expression list 
 * Note: the list is constructed in reverse order for efficiency considerations (tail-recurcsive) 
 * and reversed when it is used. */
explist:
    | exp
        { $1 :: [] }
    | explist COMMA exp
        { $3 :: $1 }
    ;

/* argument 
 * Just a variable denotes either an entire quantum or classical register or a local classical parameter within a gate 
 * definition */
argument:
    | IDENTIFIER
        { VARIABLE $1 }
    | IDENTIFIER LBRACKET INT_CONST RBRACKET
        { INDEX ($1, safe_int_of_float $3) }
    ;

/* mixed list 
 * List of registers and single elements within registers (INDEX) mixed 
 * Note: the list is constructed in reverse order for efficiency considerations (tail-recursive) 
 * and reversed when it is used. */
mixedlist:
    | IDENTIFIER LBRACKET INT_CONST RBRACKET
        { [INDEX ($1, safe_int_of_float $3)] }
    | mixedlist COMMA IDENTIFIER
        { VARIABLE $3 :: $1 }
    | mixedlist COMMA IDENTIFIER LBRACKET INT_CONST RBRACKET
        { INDEX ($3, safe_int_of_float $5) :: $1 }
    | idlist COMMA IDENTIFIER LBRACKET INT_CONST RBRACKET
        { INDEX ($3, safe_int_of_float $5) :: List.map (fun s -> (VARIABLE s : argument)) $1 }
    ;

/* identifier list 
 * Such a list can also be seen as a list of registers (classical or quantum)
 * Note: the list is constructed in reverse order for efficiency considerations (tail-recursive) 
 * and reversed when it is used. */
idlist:
    | IDENTIFIER
        { $1 :: [] }
    | idlist COMMA IDENTIFIER
        { $3 :: $1 }
    ;

/* any list
 * Either a mixed list or an identifier list. Since an identifier list stores the identifier as raw strings, those must
 * be first transformed into variables. 
 * Note: both, idlist and mixedlist, are constructed in reverse order. Consequently, they are reversed here. */
anylist:
    | idlist
        { List.rev_map (fun s -> (VARIABLE s : argument)) $1 }
    | mixedlist
        { List.rev $1 }
    ;

/* unary operation */
uop:
    | U LPAREN exp COMMA exp COMMA exp RPAREN argument SEMICOLON
        { U ($3, $5, $7, $9) }
    | CX argument COMMA argument SEMICOLON
        { CX ($2, $4) }
    | IDENTIFIER anylist SEMICOLON
        { CALL ($1, [], $2) }  /* a call of a defined gate without parameters */
    | IDENTIFIER LPAREN RPAREN anylist SEMICOLON
        { CALL ($1, [], $4) }  /* a call of a defined gate without parameters */
    | IDENTIFIER LPAREN explist RPAREN anylist SEMICOLON
        { CALL ($1, List.rev $3, $5) }  /* a call of a defined gate with parameters */
    ;

/* quantum operation */
qop:
    | uop
        { $1 }
    | MEASURE argument ARROW argument SEMICOLON
        { MEASURE ($2, $4) }
    | RESET argument SEMICOLON
        { RESET $2 }
    ;

/* general operation list */
goplist:
    | uop
        { $1 :: [] }
    | BARRIER idlist SEMICOLON
        { BARRIER (List.rev_map (fun s -> (VARIABLE s : argument)) $2) :: [] }
    | goplist uop
        { $2 :: $1 }
    | goplist BARRIER idlist SEMICOLON
        { BARRIER (List.rev_map (fun s -> (VARIABLE s : argument)) $3) :: $1 }
    ;

/* gate declaration 
 * Classical parameters (if any) are given within round parentheses, quantum arguements are enumerated outside of any
 * parenthesis before the body starts */
gatedecl:
    | GATE IDENTIFIER idlist LBRACE RBRACE
        { DEFINITION ($2, [], List.rev $3, []) }
    | GATE IDENTIFIER idlist LBRACE goplist RBRACE
        { DEFINITION ($2, [], List.rev $3, List.rev $5) }
    | GATE IDENTIFIER LPAREN RPAREN idlist LBRACE RBRACE
        { DEFINITION ($2, [], List.rev $5, []) }
    | GATE IDENTIFIER LPAREN RPAREN idlist LBRACE goplist RBRACE
        { DEFINITION ($2, [], List.rev $5, List.rev $7) }
    | GATE IDENTIFIER LPAREN idlist RPAREN idlist LBRACE RBRACE
        { DEFINITION ($2, List.rev $4, List.rev $6, []) }
    | GATE IDENTIFIER LPAREN idlist RPAREN idlist LBRACE goplist RBRACE
        { DEFINITION ($2, List.rev $4, List.rev $6, List.rev $8) }
    ;

/* declaration
 * Declaration of classical and quantum registers */
decl:
    | QREG IDENTIFIER LBRACKET INT_CONST RBRACKET SEMICOLON
        { DECLARATION ($2, QREG (safe_int_of_float $4)) }
    | CREG IDENTIFIER LBRACKET INT_CONST RBRACKET SEMICOLON
        { DECLARATION ($2, CREG (safe_int_of_float $4)) }
    ;

/* statement */
statement:
    | decl
        { $1 }
    | gatedecl
        { $1 }
    | OPAQUE IDENTIFIER idlist SEMICOLON
        { OPAQUE_DEFINITION ($2, [], List.rev $3) }
    | OPAQUE IDENTIFIER LPAREN RPAREN idlist SEMICOLON
        { OPAQUE_DEFINITION ($2, [], List.rev $5) }
    | OPAQUE IDENTIFIER LPAREN idlist RPAREN idlist SEMICOLON
        { OPAQUE_DEFINITION ($2, List.rev $4, List.rev $6) }
    | qop
        { OPERATION $1 }
    | IF LPAREN IDENTIFIER EQ_EQ INT_CONST RPAREN qop
        {
            IF ($3, safe_int_of_float $5, $7) 
        }
    | BARRIER anylist SEMICOLON
        { OPERATION (BARRIER $2) }
    ;

/* program
 * Note: the list is constructed in reverse order for efficiency considerations (tail-recursive) 
 * and reversed when it is used. */
program:
    | statement
        { $1 :: [] }
    | program statement
        { $2 :: $1 }
    ;

/* file (entry point) */
file:
    | OPENQASM program EOF
        { List.rev $2 }
    | EOF
        { [] }
    ;
%%
