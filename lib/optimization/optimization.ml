open Circuit

(** This module serves as a template for every optimisation. See also [qcprop.ml] how they are used. *)

module type Optimization = sig
  val transform : circuit -> circuit
end
