open Abs
open Circuit
open Symbol
open Logging

type reg = { size : int; offset : int }
(** [reg] stores the size and the offset of a register. The [size] is the length of the register, the [offset] is the 
    address of this specific register where it starts on some global unbounded register. The first register declared
    will have [offset = 0]. There are two distinct global registers for classical and quantum data. *)

(** [symbol] defines the type of symbols that are stored in a variable environment. Those may be [CIRC] representing 
    circuits declared by a gate declaration, those will be inlined in the resulting circuit, or they are [GATE] 
    representing gates which represent operations besides the basic gates U and CX. These are stored as a function that 
    will return the correct gate when a matching list of indices is provided. The indices denote the qubits they shoudl 
    act on.
    
    Furthermore, we have [CREG] and [QREG] which represent a classical and quantum register, respectively, with their
    size and offset, see also [Qasmreader.reg]. Finally, there is [VAR] representing a parameter value (local variable; 
    always calssical) within the body of a circuit declaration. *)
type symbol =
  | CIRC of string list * string list * operation list
  | GATE of (float list -> int list -> gate)
  | CREG of reg
  | QREG of reg
  | VAR of float

type prog = { qp : int; cp : int; env : symbol environment; insts : gate list }
(** [prog] stores the state of a program (evaluation). It can be compared to the state of a virtual machine. It has two
    heap pointers, one for the classical, and one for the quantum registers, respectively. Then it stores a variable
    environment, and a list of gates the circuit evaluated so far contains (for efficency reasons in reverse order; 
    those will be reverted at the end before returning the circuit). *)

type counter = { mutable barrier : int; mutable identity : int }

let counter = { barrier = 0; identity = 0 }

let init () =
  counter.barrier <- 0;
  counter.identity <- 0

let rec eval_expression env ex =
  match ex with
  | BINARY (op, ex1, ex2) -> (
      let v1 = eval_expression env ex1 in
      let v2 = eval_expression env ex2 in
      match op with
      | ADD -> v1 +. v2
      | SUB -> v1 -. v2
      | MUL -> v1 *. v2
      | DIV -> v1 /. v2
      | EXP -> v1 ** v2)
  | UNARY (op, ex) -> (
      let v = eval_expression env ex in
      match op with
      | MINUS -> -.v
      | SIN -> sin v
      | COS -> cos v
      | TAN -> tan v
      | EXP -> exp v
      | LN -> log v
      | SQRT -> sqrt v)
  | CONSTANT c -> (
      match c with INT_CONST r | FLOAT_CONST r -> r | PI -> Float.pi)
  | VARIABLE id -> (
      match get env (symbol id) with
      | VAR value -> value
      | _ -> failwith "[qasm_reader]: Must be VAR")

(* Note: In general, classical parameters will be refered shortly to as parameters, and quantum arguements will be refered
   shortly to as arguments. *)

let simplified_u3 ?(precision = 1e-8) theta phi lambda = function
  | i ->
      (* [=~] wil be used to check approximate equality up to the given precision in the follown instead of using
         strict [=], which might lead to unexpected behavior with floats- *)
      let ( =~ ) = Util.approx precision in
      (* try to simplify the general U gate as much as possible and return simpler gates like X, or P(phi) instead. *)
      if theta =~ 0. && phi =~ 0. then
        if Float.abs lambda =~ Float.pi then Z i
        else if lambda =~ Float.pi /. 2. then S i
        else if lambda =~ -.Float.pi /. 2. then SDG i
        else if lambda =~ Float.pi /. 4. then T i
        else if lambda =~ -.Float.pi /. 4. then TDG i
        else if lambda =~ 0. then Id i
        else P (lambda, i)
      else if theta =~ Float.pi /. 2. then
        if phi =~ 0. && lambda =~ Float.pi then H i else U2 (phi, lambda, i)
      else if phi =~ 0. && lambda =~ 0. then RY (theta, i)
      else if phi =~ -.Float.pi /. 2. && lambda =~ Float.pi /. 2. then
        RX (theta, i)
      else if theta =~ Float.pi && phi =~ 0. && lambda =~ Float.pi then X i
      else if
        theta =~ Float.pi && phi =~ Float.pi /. 2. && lambda =~ Float.pi /. 2.
      then Y i
      else U3 (theta, phi, lambda, i)

(** [eval_operation prog op] applies the given operation to the program state, i.e. the specified gates to the list in 
    the program state. *)
let rec eval_operation prog (op : operation) =
  let { qp; cp; env; insts } = prog in
  match op with
  | U (theta, phi, lambda, q) ->
      (* Handle the basic gate U *)
      (* evaluate the expression for the parameters to floats *)
      let theta = eval_expression env theta in
      let phi = eval_expression env phi in
      let lambda = eval_expression env lambda in
      let gate = simplified_u3 theta phi lambda in
      (* add the gate to the list of gates [insts] collected so far. If the gate is applied on an entire qubit
          register, add the gate application for every qubit in that gate. *)
      let insts =
        match q with
        | INDEX (id, i) -> (
            match get env (symbol id) with
            | QREG { size; offset } -> (
                if i < 0 || i >= size then
                  failwith "[qasm_reader]: Index out of bounds"
                else
                  match gate (offset + i) with
                  | Id _ ->
                      counter.identity <- counter.identity + 1;
                      insts
                  | g -> g :: insts)
            | _ -> failwith "[qasm_reader]: Must be quantum type")
        | VARIABLE id -> (
            match get env (symbol id) with
            | QREG { size; offset } ->
                Util.init_fold_left size
                  (fun i insts ->
                    match gate (offset + i) with
                    | Id _ ->
                        counter.identity <- counter.identity + 1;
                        insts
                    | _ as gate -> gate :: insts)
                  insts
            | _ -> failwith "[qasm_reader]: Must be quantum type")
      in
      { qp; cp; env; insts }
  | CX (c, t) ->
      (* handle the basic CX gate *)
      (* add the gate to the list of gates [insts] collected so far. If the gate is applied on an entire qubit register,
         add the gate application for every qubit in that gate. In this case either both arguments must be registers of
         the same length or one must be a single qubit. Then the gate is added [n] times with the [i]-th element of each
         register as argument where [i] is in 0, ..., (n-1) and [n] is the length of the register.*)
      let insts =
        match (c, t) with
        | INDEX (cid, ci), INDEX (tid, ti) -> (
            match (get env (symbol cid), get env (symbol tid)) with
            | ( QREG { size; offset = coffset },
                QREG { size = tsize; offset = toffset } )
              when size = tsize ->
                if ci < 0 || ci >= size || ti < 0 || ti >= size then
                  failwith "[qasm_reader]: Index out of bounds"
                else CTRL ([ ci + coffset ], 0., X (ti + toffset)) :: insts
            | QREG _, QREG _ -> failwith "[qasm_reader]: Must have same length"
            | _ -> failwith "[qasm_reader]: Must be quantum type")
        | VARIABLE cid, INDEX (tid, ti) -> (
            match (get env (symbol cid), get env (symbol tid)) with
            | ( QREG { size = csize; offset = coffset },
                QREG { size = tsize; offset = toffset } ) ->
                if ti < 0 || ti >= tsize then
                  failwith "[qasm_reader]: Index out of bounds"
                else
                  Util.init_fold_left csize
                    (fun i insts ->
                      CTRL ([ i + coffset ], 0., X (ti + toffset)) :: insts)
                    insts
            | _ -> failwith "[qasm_reader]: Must be quantum type")
        | INDEX (cid, ci), VARIABLE tid -> (
            match (get env (symbol cid), get env (symbol tid)) with
            | ( QREG { size = csize; offset = coffset },
                QREG { size = tsize; offset = toffset } ) ->
                if ci < 0 || ci >= csize then
                  failwith "[qasm_reader]: Index out of bounds"
                else
                  Util.init_fold_left tsize
                    (fun i insts ->
                      CTRL ([ ci + coffset ], 0., X (i + toffset)) :: insts)
                    insts
            | _ -> failwith "[qasm_reader]: Must be quantum type")
        | VARIABLE cid, VARIABLE tid -> (
            match (get env (symbol cid), get env (symbol tid)) with
            | ( QREG { size; offset = coffset },
                QREG { size = tsize; offset = toffset } )
              when size = tsize ->
                Util.init_fold_left size
                  (fun i insts ->
                    CTRL ([ i + coffset ], 0., X (i + toffset)) :: insts)
                  insts
            | QREG _, QREG _ -> failwith "[qasm_reader]: Must have same length"
            | _ -> failwith "[qasm_reader]: Must be quantum type")
      in
      { qp; cp; env; insts }
  | MEASURE (q, c) -> (
      (* handle a measure *)
      (* the arguments must be both single register cells (one classical and one quantum) or both must be registers of
          the same length. In the latter case the measure operation is added for every cell [i] in both registers for [i]
         in 0, ...,(n-1) where [n] is the length of the registers. *)
      match (q, c) with
      | VARIABLE qid, VARIABLE cid -> (
          match (get env (symbol qid), get env (symbol cid)) with
          | ( QREG { size; offset = qoffset },
              CREG { size = csize; offset = coffset } )
            when size = csize ->
              let insts =
                List.rev_append
                  (List.init size (fun i -> MEASURE (i + qoffset, i + coffset)))
                  insts
              in
              { qp; cp; env; insts }
          | QREG _, CREG _ -> failwith "[qasm_reader]: Must be same length"
          | _, _ -> failwith "[qasm_reader]: Must be quantum/classical type")
      | INDEX (qid, i), INDEX (cid, j) -> (
          match (get env (symbol qid), get env (symbol cid)) with
          | ( QREG { size = qsize; offset = qoffset },
              CREG { size = csize; offset = coffset } ) ->
              if i < 0 || i >= qsize || j < 0 || j >= csize then
                failwith "[qasm_reader]: Index out of bounds"
              else
                let insts = MEASURE (i + qoffset, j + coffset) :: insts in
                { qp; cp; env; insts }
          | _, _ -> failwith "[qasm_reader]: Must be quantum/classical type")
      | _ -> failwith "[qasm_reader]: Must be either both registers or indices")
  | RESET _ ->
      (* reset operation not supported yet *)
      failwith "[qasm_reader]: Not supported: reset"
  | BARRIER _ ->
      (* barrier will be dropped since they have no influence on the resulting quantum state.
         A warning will be printed to stdout, though. *)
      (* The cryptic characters surrounding "Warning" make the word "Warning" appear in purple and bold. *)
      let _ = counter.barrier <- counter.barrier + 1 in
      prog
  | CALL (id, params, args) -> (
      (* handle defined circuits (as calls to functions) and other operations besides U and CX *)
      let call prog params args =
        (* here params needs to be evaluated to floats already, and args are the indices *)
        let { qp; cp; env; insts } = prog in
        match get env (symbol id) with
        | CIRC (formal_params, formal_args, ops) ->
            (* simulate a function call:
               (1) setup the environment by opening a new scope
               (2) add passed parameters as local variables to the environment
               (3) declare (local) quantum registers for passed (quantum) arguments
               (4) execute the body of the called circuit (translates basically to an inling in the resulting circuit)
               (5) close scope *)
            (* 1 *)
            let _ = openScope prog.env in
            (* 2 *)
            let _ =
              List.iter2
                (fun fp p -> add env (symbol fp) (VAR p))
                formal_params params
            in
            (* 3 *)
            let _ =
              List.iter2
                (fun fa a ->
                  add env (symbol fa) (QREG { size = 1; offset = a }))
                formal_args args
            in
            (* 4 *)
            let { qp; cp; env; insts } =
              List.fold_left eval_operation { qp; cp; env; insts } ops
            in
            (* 5 *)
            let _ = closeScope prog.env in
            { qp; cp; env; insts }
        | GATE gate ->
            (* handle gates besides U and CX, see also explanation of [Qasmreader.eval_statement] *)
            let insts =
              match gate params args with
              | Id _ | CTRL (_, 0., Id _) -> insts
              | _ as gate -> gate :: insts
            in
            { qp; cp; env; insts }
        | _ -> failwith "[qasm_reader]: Must be gate or circ"
      in
      (* evaluate parameters to floats *)
      let params = List.map (eval_expression env) params in
      (* find and return the length of the first argument that is a register; returns None if all arguments are single
         qubits*)
      let n_opt =
        List.find_map
          (function
            | (VARIABLE id : argument) -> (
                match get env (symbol id) with
                | QREG { size; _ } -> Some size
                | _ -> None)
            | _ -> None)
          args
      in
      match n_opt with
      | None ->
          (* all arguements are single qubits *)
          let idxs =
            List.map
              (function
                | INDEX (id, i) -> (
                    match get env (symbol id) with
                    | QREG { size; offset } ->
                        if i < 0 || i >= size then
                          failwith "[qasm_reader]: Index out of bounds"
                        else i + offset
                    | _ -> failwith "[qasm_reader]: Must be quantum type")
                | _ -> failwith "[qasm_reader]: Cannot occur")
              args
          in
          call { qp; cp; env; insts } params idxs
      | Some n ->
          (* some arguements are registers *)
          (* check wehther all arguments that are registers have the same length *)
          if
            List.for_all
              (function
                | (VARIABLE id : argument) -> (
                    match get env (symbol id) with
                    | QREG { size; _ } -> size = n
                    | _ -> true)
                | _ -> true)
              args
          then
            let idxs i =
              List.map
                (function
                  | (VARIABLE id : argument) -> (
                      match get env (symbol id) with
                      | QREG { size; offset } ->
                          if i < 0 || i >= size then
                            failwith "Index out of bounds"
                          else i + offset
                      | _ -> failwith "[qasm_reader]: Must be quantum type")
                  | INDEX (id, i) -> (
                      match get env (symbol id) with
                      | QREG { size; offset } ->
                          if i < 0 || i >= size then
                            failwith "[qasm_reader]: Index out of bounds"
                          else i + offset
                      | _ -> failwith "[qasm_reader]: Must be quantum type"))
                args
            in
            Util.init_fold_left n
              (fun i prog -> call prog params (idxs i))
              { qp; cp; env; insts }
          else failwith "[qasm_reader]: Long args must have same length")

(** [eval_statement prog stmt] takes a state of the program and a statement to process. The program state [prog] 
    contains heap pointer for the classical and quantum registers, a variable environment, and a list of gates added so
    far to the resulting circuit. See also the type [Qasmreader.prog] definition of prog. 
    
    Known operations besides U and CX are treated as opaque gate declarations. Those must be declared as opaque in the
    included library, see also `include/qelib1.inc`. For each such known opqaue declaration a function is put into the
    environment that when called with an index return the appropriate operation of type [Circuit.gate]. 
    
    For that to work, those gate declaration that should not be inlined in the normal fashion, their definition in the
    original `qelib1.inc` must be changed: Their body must be removed and the keyword *gate* must be replaced by 
    *opaque*. *)
let eval_statement prog stmt =
  match stmt with
  | OPERATION op -> eval_operation prog op
  | DECLARATION (id, QREG n) ->
      let { qp; cp; env; insts } = prog in
      let _ = add env (symbol id) (QREG { size = n; offset = qp }) in
      { qp = qp + n; cp; env; insts }
  | DECLARATION (id, CREG n) ->
      let { qp; cp; env; insts } = prog in
      let _ = add env (symbol id) (CREG { size = n; offset = cp }) in
      { qp; cp = cp + n; env; insts }
  | DEFINITION (id, params, args, ops) ->
      let { qp; cp; env; insts } = prog in
      let _ = add env (symbol id) (CIRC (params, args, ops)) in
      { qp; cp; env; insts }
  | OPAQUE_DEFINITION (id, params, args) -> (
      (* The following are operations that should not be inlined and replaced by simpler gates as this increases the
         gate count and may compromise the abilities for optimization. The operations listed here must be declared as
         *opaque* in the qasm file to be in effect.

         For the format of [Qasmreader.Gate] see also its definition. *)
      let { qp; cp; env; insts } = prog in
      match (id, params, args) with
      | "sx", [], [ _ ] ->
          let _ =
            add env (symbol "sx")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ a ] -> SX a
                     | _ -> failwith "[qasm_reader]: Expects one argument")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | "ccx", [], [ _; _; _ ] ->
          let _ =
            add env (symbol "ccx")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ c1; c2; t ] -> CTRL ([ c1; c2 ], 0., X t)
                     | _ -> failwith "[qasm_reader]: Expects three arguments")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | "crx", [ _ ], [ _; _ ] ->
          let _ =
            add env (symbol "crx")
              (GATE
                 (function
                 | [ lambda ] -> (
                     function
                     | [ c; t ] -> CTRL ([ c ], 0., RX (lambda, t))
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects one parameter"))
          in
          { qp; cp; env; insts }
      | "cry", [ _ ], [ _; _ ] ->
          let _ =
            add env (symbol "cry")
              (GATE
                 (function
                 | [ lambda ] -> (
                     function
                     | [ c; t ] -> CTRL ([ c ], 0., RY (lambda, t))
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects one parameter"))
          in
          { qp; cp; env; insts }
      | "crz", [ _ ], [ _; _ ] ->
          let _ =
            add env (symbol "crz")
              (GATE
                 (function
                 | [ lambda ] -> (
                     function
                     | [ c; t ] -> CTRL ([ c ], -.lambda /. 2., P (lambda, t))
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects one parameter"))
          in
          { qp; cp; env; insts }
      | "cu3", [ _; _; _ ], [ _; _ ] ->
          let _ =
            add env (symbol "cu3")
              (GATE
                 (function
                 | [ theta; phi; lambda ] -> (
                     function
                     | [ c; t ] ->
                         CTRL ([ c ], 0., simplified_u3 theta phi lambda t)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects three parameters"))
          in
          { qp; cp; env; insts }
      | "cu", [ _; _; _; _ ], [ _; _ ] ->
          let _ =
            add env (symbol "cu")
              (GATE
                 (function
                 | [ theta; phi; lambda; gamma ] -> (
                     function
                     | [ c; t ] ->
                         CTRL ([ c ], gamma, simplified_u3 theta phi lambda t)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects four parameters"))
          in
          { qp; cp; env; insts }
      | "ecr", [], [ _; _ ] ->
          let _ =
            add env (symbol "ecr")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ a; b ] -> ECR (a, b)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | "rxx", [ _ ], [ _; _ ] ->
          let _ =
            add env (symbol "rxx")
              (GATE
                 (function
                 | [ theta ] -> (
                     function
                     | [ a; b ] -> RXX (theta, a, b)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects one parameter"))
          in
          { qp; cp; env; insts }
      | "rzz", [ _ ], [ _; _ ] ->
          let _ =
            add env (symbol "rzz")
              (GATE
                 (function
                 | [ theta ] -> (
                     function
                     | [ a; b ] -> RZZ (theta, a, b)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects one parameter"))
          in
          { qp; cp; env; insts }
      | "csx", [], [ _; _ ] ->
          let _ =
            add env (symbol "csx")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ a; b ] -> CTRL ([ a ], 0., SX b)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | "swap", [], [ _; _ ] ->
          let _ =
            add env (symbol "swap")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ a; b ] -> SWAP (a, b)
                     | _ -> failwith "[qasm_reader]: Expects two arguments")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | "cswap", [], [ _; _; _ ] ->
          let _ =
            add env (symbol "cswap")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ c; a; b ] -> CTRL ([ c ], 0., SWAP (a, b))
                     | _ -> failwith "[qasm_reader]: Expects three arguments")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | "rccx", [], [ _; _; _ ] ->
          let _ =
            add env (symbol "rccx")
              (GATE
                 (function
                 | [] -> (
                     function
                     | [ a; b; c ] -> RCCX (a, b, c)
                     | _ -> failwith "[qasm_reader]: Expects three arguments")
                 | _ -> failwith "[qasm_reader]: Expects no parameters"))
          in
          { qp; cp; env; insts }
      | _ -> failwith ("[qasm_reader]: Not supported operation: " ^ id))
  | IF _ -> failwith "[qasm_reader]: Not supported: if"

(** [eval ast] takes a syntax tree and produces a circuit representation of it. See also its type definition 
    [Circuit.circuit]*)
let eval ast =
  let prog =
    List.fold_left eval_statement
      { qp = 0; cp = 0; env = Symbol.empty 23; insts = [] }
      ast
  in
  { size = max prog.qp prog.cp; gates = List.rev prog.insts }

(** [from_file fname] reads a circuit from a qasm file. It will first preprocess the qasm file and by doing that resolve
    all include statements in the qasm file. Second, it will parse the file. The generated syntax tree will be evaluated
    by [eval] that generates a circuit representation of the syntax tree, see also the type 
    [Circuit.circuit]. *)
let scan ic =
  let _ = init () in
  let f = Preprocessor.preprocess ic in
  let lexbuf = Lexing.from_function f in
  let ast = Qasm_parser.file Qasm_lexer.token lexbuf in
  let _ = close_in ic in
  let circ = eval ast in
  let _ =
    if counter.barrier > 0 then
      log_msg WARN
        ("[qasmreader]: dropped "
        ^ string_of_int counter.barrier
        ^ " barrier(s)")
  in
  let _ =
    if counter.identity > 0 then
      log_msg WARN
        ("[qasmreader]: dropped "
        ^ string_of_int counter.identity
        ^ " gates because they reduced to identiy")
  in
  circ
