open Util

(** Those are the known gates. If one wants to add a gate here, special handling for these also need to be added to the
    [Qasmreader.eval_statement] and it must be declared as an opaque operation in the QASM file. 
    
    See also [Qasmreade.eval_operation] who a general unitary U is simplified to simpler gates. Here you can also find
    information by checking the implementation that U1 is translated into P. *)
type gate =
  | Id of int
  | H of int
  | P of float * int
  | RX of float * int
  | RY of float * int
  | S of int
  | SDG of int
  | SX of int
  | T of int
  | TDG of int
  | U2 of float * float * int
  | U3 of float * float * float * int
  | X of int
  | Y of int
  | Z of int
  | ECR of int * int
  | RXX of float * int * int
  | RYY of float * int * int
  | RZZ of float * int * int
  | RCCX of int * int * int
  | SWAP of int * int
  | CTRL of int list * float * gate
  | MEASURE of int * int

(* Representation of a circuit. The gates are just stored as a list. *)
type circuit = { size : int; gates : gate list }

(* [string_of_gate gate] return a string representation of the [gate] in OpenQASM syntax. The default name for qubit
   registers is q and for classical registers is c. However, you can change that through the optional arguments. *)
let rec string_of_gate ?(qreg_name = "q") ?(creg_name = "c") = function
  | Id i -> "id " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | H i -> "h " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | X i -> "x " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | Y i -> "y " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | Z i -> "z " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | S i -> "s " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | T i -> "t " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | TDG i -> "tdg " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | SDG i -> "sdg " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | SX i -> "sx " ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | RX (phi, i) ->
      "rx(" ^ pi_string_of_float phi ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "]"
  | RY (phi, i) ->
      "ry(" ^ pi_string_of_float phi ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "]"
  | RXX (phi, i, j) ->
      "rxx(" ^ pi_string_of_float phi ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "], " ^ qreg_name ^ "[" ^ string_of_int j ^ "]"
  | RYY (phi, i, j) ->
      "ryy(" ^ pi_string_of_float phi ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "], " ^ qreg_name ^ "[" ^ string_of_int j ^ "]"
  | RZZ (phi, i, j) ->
      "rzz(" ^ pi_string_of_float phi ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "], " ^ qreg_name ^ "[" ^ string_of_int j ^ "]"
  | P (phi, i) ->
      "p(" ^ pi_string_of_float phi ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "]"
  | U2 (phi, lambda, i) ->
      "u2(" ^ pi_string_of_float phi ^ ", " ^ pi_string_of_float lambda ^ ") "
      ^ qreg_name ^ "[" ^ string_of_int i ^ "]"
  | U3 (theta, phi, lambda, i) ->
      "u3(" ^ pi_string_of_float theta ^ ", " ^ pi_string_of_float phi ^ ", "
      ^ pi_string_of_float lambda ^ ") " ^ qreg_name ^ "[" ^ string_of_int i
      ^ "]"
  | ECR (i, j) ->
      "ecr " ^ qreg_name ^ "[" ^ string_of_int i ^ "], " ^ qreg_name ^ "["
      ^ string_of_int j ^ "]"
  | RCCX (i, j, k) ->
      "rccx " ^ qreg_name ^ "[" ^ string_of_int i ^ "], " ^ qreg_name ^ "["
      ^ string_of_int j ^ "], " ^ qreg_name ^ "[" ^ string_of_int k ^ "]"
  | SWAP (i, j) ->
      "swap " ^ qreg_name ^ "[" ^ string_of_int i ^ "], " ^ qreg_name ^ "["
      ^ string_of_int j ^ "]"
  | CTRL (cs, gamma, t) ->
      let bf = Buffer.create 16 in
      if List.length cs > 2 then invalid_arg "[circuit] Too many controls."
      else (
        List.iter (fun _ -> Buffer.add_string bf "c") cs;
        let is =
          match t with
          | CTRL _ -> invalid_arg "[circuit] Nested controls are not allowed."
          | U3 (theta, phi, lambda, i) when gamma = 0. ->
              Buffer.add_string bf "u3(";
              Buffer.add_string bf (pi_string_of_float theta);
              Buffer.add_string bf ", ";
              Buffer.add_string bf (pi_string_of_float phi);
              Buffer.add_string bf ", ";
              Buffer.add_string bf (pi_string_of_float lambda);
              Buffer.add_string bf ") ";
              [ i ]
          | X i when gamma = 0. ->
              (* TODO perhaps add an approximate equals here *)
              Buffer.add_string bf "x ";
              [ i ]
          | Y i when gamma = 0. ->
              Buffer.add_string bf "y ";
              [ i ]
          | Z i when gamma = 0. ->
              Buffer.add_string bf "z ";
              [ i ]
          | H i when gamma = 0. ->
              Buffer.add_string bf "h ";
              [ i ]
          | P (phi, i) when gamma = 0. ->
              Buffer.add_string bf ("p(" ^ pi_string_of_float phi ^ ") ");
              [ i ]
          | S i when gamma = 0. ->
              Buffer.add_string bf "p(pi/2) ";
              [ i ]
          | SDG i when gamma = 0. ->
              Buffer.add_string bf "p(-pi/2) ";
              [ i ]
          | T i when gamma = 0. ->
              Buffer.add_string bf "p(pi/4) ";
              [ i ]
          | TDG i when gamma = 0. ->
              Buffer.add_string bf "p(-pi/4) ";
              [ i ]
          | RY (phi, i) when gamma = 0. ->
              Buffer.add_string bf ("ry(" ^ pi_string_of_float phi ^ ") ");
              [ i ]
          | SWAP (i, j) when gamma = 0. ->
              Buffer.add_string bf "swap ";
              [ i; j ]
          | U3 (theta, phi, lambda, i) ->
              Buffer.add_string bf "u(";
              Buffer.add_string bf (pi_string_of_float theta);
              Buffer.add_string bf ", ";
              Buffer.add_string bf (pi_string_of_float phi);
              Buffer.add_string bf ", ";
              Buffer.add_string bf (pi_string_of_float lambda);
              Buffer.add_string bf ", ";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | X i ->
              (* TODO perhaps add an approximate equals here *)
              Buffer.add_string bf "u(pi,0,pi,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | Y i ->
              Buffer.add_string bf "u(pi,pi/2,pi/2,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | Z i ->
              Buffer.add_string bf "u(0,0,pi,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | H i ->
              Buffer.add_string bf "u(pi/2,0,pi,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | P (phi, i) ->
              Buffer.add_string bf "u(0,0,";
              Buffer.add_string bf (pi_string_of_float phi);
              Buffer.add_string bf ",";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | S i ->
              Buffer.add_string bf "u(0,0,pi/2,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | SDG i ->
              Buffer.add_string bf "u(0,0,-pi/2,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | T i ->
              Buffer.add_string bf "u(0,0,pi/4,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | TDG i ->
              Buffer.add_string bf "u(0,0,-pi/4,";
              Buffer.add_string bf (pi_string_of_float gamma);
              Buffer.add_string bf ") ";
              [ i ]
          | _ ->
              invalid_arg
                ("[circuit] Gate does not support controls: " ^ string_of_gate t)
        in
        List.iter
          (fun i ->
            Buffer.add_string bf (qreg_name ^ "[");
            Buffer.add_string bf (string_of_int i);
            Buffer.add_string bf "], ")
          cs;
        Buffer.add_string bf (qreg_name ^ "[");
        Buffer.add_string bf (string_of_int (List.hd is));
        Buffer.add_string bf "]";
        List.iter
          (fun i ->
            Buffer.add_string bf (", " ^ qreg_name ^ "[");
            Buffer.add_string bf (string_of_int i);
            Buffer.add_string bf "]")
          (List.tl is);
        Buffer.contents bf)
  | MEASURE (i, j) ->
      "measure " ^ qreg_name ^ "[" ^ string_of_int i ^ "] -> " ^ creg_name ^ "["
      ^ string_of_int j ^ "]"

(** [unitary_of_gate t] return the unitary represenataion of a gate [t] as a 2D array *)
let unitary_of_gate t : Complex.t array array =
  match t with
  | H _ ->
      [|
        [| { re = 1. /. sqrt 2.; im = 0. }; { re = 1. /. sqrt 2.; im = 0. } |];
        [| { re = 1. /. sqrt 2.; im = 0. }; { re = -1. /. sqrt 2.; im = 0. } |];
      |]
  | P (phi, _) ->
      [|
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
        [| { re = 0.; im = 0. }; Complex.exp { re = 0.; im = phi } |];
      |]
  | RX (theta, _) ->
      [|
        [|
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = -.sin (theta /. 2.) };
        |];
        [|
          { re = 0.; im = -.sin (theta /. 2.) };
          { re = cos (theta /. 2.); im = 0. };
        |];
      |]
  | RY (theta, _) ->
      [|
        [|
          { re = cos (theta /. 2.); im = 0. };
          { re = -.sin (theta /. 2.); im = 0. };
        |];
        [|
          { re = sin (theta /. 2.); im = 0. };
          { re = cos (theta /. 2.); im = 0. };
        |];
      |]
  | S _ ->
      [|
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
        [| { re = 0.; im = 0. }; Complex.exp { re = 0.; im = Float.pi /. 2. } |];
      |]
  | SDG _ ->
      [|
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
        [|
          { re = 0.; im = 0. }; Complex.exp { re = 0.; im = -.Float.pi /. 2. };
        |];
      |]
  | SX _ ->
      [|
        [|
          { re = 1. /. sqrt 2.; im = 0. };
          Complex.neg { re = 0.; im = -1. /. sqrt 2. };
        |];
        [|
          Complex.neg { re = 0.; im = -1. /. sqrt 2. };
          { re = 1. /. sqrt 2.; im = 0. };
        |];
      |]
  | T _ ->
      [|
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
        [| { re = 0.; im = 0. }; Complex.exp { re = 0.; im = Float.pi /. 4. } |];
      |]
  | TDG _ ->
      [|
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
        [|
          { re = 0.; im = 0. }; Complex.exp { re = 0.; im = -.Float.pi /. 4. };
        |];
      |]
  | X _ ->
      [|
        [| { re = 0.; im = 0. }; { re = 1.; im = 0. } |];
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
      |]
  | Y _ ->
      [|
        [| { re = 0.; im = 0. }; { re = 0.; im = -1. } |];
        [| { re = 0.; im = 1. }; { re = 0.; im = 0. } |];
      |]
  | Z _ ->
      [|
        [| { re = 1.; im = 0. }; { re = 0.; im = 0. } |];
        [| { re = 0.; im = 0. }; { re = -1.; im = 0. } |];
      |]
  | U2 (phi, lambda, _) ->
      [|
        [|
          { re = 1. /. sqrt 2.; im = 0. };
          Complex.neg
          @@ Complex.mul
               { re = 1. /. sqrt 2.; im = 0. }
               (Complex.exp { re = 0.; im = lambda });
        |];
        [|
          Complex.mul
            { re = 1. /. sqrt 2.; im = 0. }
            (Complex.exp { re = 0.; im = phi });
          Complex.mul
            { re = 1. /. sqrt 2.; im = 0. }
            (Complex.exp { re = 0.; im = phi +. lambda });
        |];
      |]
  | U3 (theta, phi, lambda, _) ->
      [|
        [|
          { re = cos (theta /. 2.); im = 0. };
          Complex.neg
          @@ Complex.mul
               (Complex.exp { re = 0.; im = lambda })
               { re = sin (theta /. 2.); im = 0. };
        |];
        [|
          Complex.mul
            (Complex.exp { re = 0.; im = phi })
            { re = sin (theta /. 2.); im = 0. };
          Complex.mul
            (Complex.exp { re = 0.; im = phi +. lambda })
            { re = cos (theta /. 2.); im = 0. };
        |];
      |]
  | ECR _ ->
      [|
        [|
          { re = 0.; im = 0. };
          { re = 1. /. sqrt 2.; im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = 1. /. sqrt 2. };
        |];
        [|
          { re = 1. /. sqrt 2.; im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = -1. /. sqrt 2. };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = 0. };
          { re = 0.; im = 1. /. sqrt 2. };
          { re = 0.; im = 0. };
          { re = 1. /. sqrt 2.; im = 0. };
        |];
        [|
          { re = 0.; im = -1. /. sqrt 2. };
          { re = 0.; im = 0. };
          { re = 1. /. sqrt 2.; im = 0. };
          { re = 0.; im = 0. };
        |];
      |]
  | RXX (theta, _, _) ->
      [|
        [|
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = -.sin (theta /. 2.) };
        |];
        [|
          { re = 0.; im = 0. };
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = -.sin (theta /. 2.) };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = 0. };
          { re = 0.; im = -.sin (theta /. 2.) };
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = -.sin (theta /. 2.) };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          { re = cos (theta /. 2.); im = 0. };
        |];
      |]
  | RYY (theta, _, _) ->
      [|
        [|
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = sin (theta /. 2.) };
        |];
        [|
          { re = 0.; im = 0. };
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = -.sin (theta /. 2.) };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = 0. };
          { re = 0.; im = -.sin (theta /. 2.) };
          { re = cos (theta /. 2.); im = 0. };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = sin (theta /. 2.) };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          { re = cos (theta /. 2.); im = 0. };
        |];
      |]
  | RZZ (theta, _, _) ->
      [|
        [|
          Complex.exp { re = 0.; im = -.theta /. 2. };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = 0. };
          Complex.exp { re = 0.; im = theta /. 2. };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          Complex.exp { re = 0.; im = theta /. 2. };
          { re = 0.; im = 0. };
        |];
        [|
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          { re = 0.; im = 0. };
          Complex.exp { re = 0.; im = -.theta /. 2. };
        |];
      |]
  | RCCX _ ->
      [|
        [|
          Complex.one;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
        |];
        [|
          Complex.zero;
          Complex.one;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
        |];
        [|
          Complex.zero;
          Complex.zero;
          Complex.one;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
        |];
        [|
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.one;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
        |];
        [|
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.one;
          Complex.zero;
          Complex.zero;
          Complex.zero;
        |];
        [|
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          { re = -1.; im = 0. };
          Complex.zero;
          Complex.zero;
        |];
        [|
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.one;
        |];
        [|
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.zero;
          Complex.one;
          Complex.zero;
        |];
      |]
  | Id _ ->
      [| [| Complex.one; Complex.zero |]; [| Complex.zero; Complex.one |] |]
  | _ ->
      invalid_arg @@ "[circuit] Gate not supported in unitary_of_gate: "
      ^ string_of_gate t ^ "."

(** [target gate] return the target of a gate. For uncontrolled gates this is just the qubit they act on; for controlled
    gates this is the target qubit. *)
let rec target = function
  | CTRL (_, _, gate) -> target gate
  | Id i
  | H i
  | P (_, i)
  | RX (_, i)
  | RY (_, i)
  | S i
  | SDG i
  | SX i
  | T i
  | TDG i
  | U2 (_, _, i)
  | U3 (_, _, _, i)
  | X i
  | Y i
  | Z i
  | MEASURE (i, _) ->
      [ i ]
  | ECR (i, j) | RXX (_, i, j) | RYY (_, i, j) | RZZ (_, i, j) | SWAP (i, j) ->
      if i <= j then [ i; j ] else [ j; i ]
  | RCCX (i, j, k) -> List.sort Int.compare [ i; j; k ]

(** [single gate] returns true if gate acts only on one qubit. *)
let single gate = List.compare_length_with (target gate) 1 = 0

(** [control gate] returns the list of controlling qubits. *)
let control = function CTRL (cs, _, _) -> cs | _ -> []

(** [dom gate] returns all qubits as a list the gate uses. This is the union of [target gate] and [control gate]. *)
let dom g = control g @ target g

(** [adjoint u] returns the adjoint of a unitary [u] *)
let adjoint u =
  [|
    [| Complex.conj u.(0).(0); Complex.conj u.(1).(0) |];
    [| Complex.conj u.(0).(1); Complex.conj u.(1).(1) |];
  |]

(** [self_adjoint gate] return true if the gate (or more precise, the unitary of the gate) is self-adjoint, i.e. 
    duplicates of that gate can be cancelled out. *)
let self_adjoint = function
  | CTRL (_, _, g) | g ->
      let u = unitary_of_gate g in
      u = adjoint u
