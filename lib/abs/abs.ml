(** Abstract Syntax of OpenQASM programs *)

type constant = INT_CONST of float | FLOAT_CONST of float | PI
type unary_operator = MINUS | SIN | COS | TAN | EXP | LN | SQRT
type binary_operator = ADD | SUB | MUL | DIV | EXP
type argument = VARIABLE of string | INDEX of string * int

type expression =
  | BINARY of binary_operator * expression * expression
  | UNARY of unary_operator * expression
  | CONSTANT of constant
  | VARIABLE of string

type operation =
  | U of expression * expression * expression * argument
      (** general unitary gate. The three expression define the classical parameters of the general unitary and the last 
      the quantum arguement this gate should act on *)
  | CX of argument * argument
      (** controlled x gate with first the controlling qubit and second the target qubit *)
  | MEASURE of argument * argument
      (** measure operation with first the qubit to be measured and second the classical register where to store the result 
      in*)
  | RESET of argument
  | BARRIER of argument list
  | CALL of string * expression list * argument list
      (** a call to a previous defined gate or circuit named with the first element. The provided expression list will 
      evaluate to the calssical parameters the gate/circuit takes and the argument list specifies the qubits this call
      acts on *)

type type_specifier = QREG of int | CREG of int

type statement =
  | OPERATION of operation
  | DECLARATION of string * type_specifier
  | OPAQUE_DEFINITION of string * string list * string list
  | DEFINITION of string * string list * string list * operation list
  | IF of string * int * operation
