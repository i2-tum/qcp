(* 
 * Documentation:  http://gallium.inria.fr/~fpottier/menhir/manual.html#sec2
 *)
(* Tokens occuring in OpenQASM *)
(* version statement *)
%token <int * int> OPENQASM
(* kewords *)
%token OPAQUE QREG CREG GATE IF SIN COS TAN EXP LN SQRT PI U CX MEASURE RESET BARRIER
(* punctuators *)
%token COMMA SEMICOLON LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE ARROW PLUS MINUS STAR SLASH CIRC EQ_EQ
(* identifier and constants *)
%token <string> IDENTIFIER
%token <float> INT_CONST (* we use float here to handle also very large integers *)
%token <float> FLOAT_CONST
(* end of file (required to return upon EOF *)
%token EOF
%%