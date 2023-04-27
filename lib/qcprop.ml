(** Quantum Constant Propagation *)

(** [process src tar passes] will read the circuit from the [src] in_channel, run the circuit through all [passes], 
    and write the resulting circuit into the [tar] out_channel. *)
let process src tar passes =
  let circ = Qasmreader.scan src in
  let circ = List.fold_left (fun circ pass -> pass circ) circ passes in
  Qasmwriter.print tar circ
