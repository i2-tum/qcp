open Circuit

(** [print fname circ] will print the OpenQASM code describing the circuit [circ] to the out_channel [out]. The issued 
    file will only have one quantum and one classical register. Consequently, if in the input multiple registers existed, 
    they were concatenated. For the translation of the operations, the function [Circuit.string_of_gate] is used. *)
let print oc circ =
  let { size; gates } = circ in
  output_string oc
    "OPENQASM 2.0;\ninclude \"qelib1.inc\";\nopaque ecr q0, q1;\n";
  output_string oc ("qreg q[" ^ string_of_int size ^ "];\n");
  output_string oc ("creg c[" ^ string_of_int size ^ "];\n");
  List.iter (fun gate -> output_string oc (string_of_gate gate ^ ";\n")) gates
