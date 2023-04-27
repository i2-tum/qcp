(** The Module Quantumstate manages the state of a set of qubits. It is implemented as a map that maps computational
    basis states (keys) to their coeeficients (values). *)

open Util

type key = Z.t
type value = Complex.t

module QM = Map.Make (Z)

type t = { qubits_n : int; state : value QM.t }

(** [empty n] returns an empty map for [n] qubits. *)
let empty n : t = { qubits_n = n; state = QM.empty }

(** [get i] returns the coefficients of the basis state [k] 
    @raise Not_found if key [k] does not exists. *)
let get k (qs : t) = QM.find k qs.state

(** [mem k qs] checks whether the key [k] exists in [qs]. *)
let mem k (qs : t) = QM.mem k qs.state

(** [add k v qs] adds the binding [(k, v)] to the quantum state. 
    @raise Illegal_arguement if key already exists. *)
let add k v (qs : t) =
  if QM.mem k qs.state then invalid_arg "[quantum state] Key already exists."
  else { qubits_n = qs.qubits_n; state = QM.add k v qs.state }

(** [init k v n] returns a new map for [n] qubits only containing the one given key-value pair. *)
let init k v n : t = add k v { qubits_n = n; state = QM.empty }

(** [set k v qs] adds the binding [(k, v)] to the quantum state. 
    @raise Illegal_arguement if key does not exist. *)
let set k v (qs : t) =
  if not @@ QM.mem k qs.state then
    invalid_arg "[quantum state] Key does not exist."
  else { qubits_n = qs.qubits_n; state = QM.add k v qs.state }

(** [remove k qs] removes the key-value pair denoted by the given key.
    @raise  Invalid_arguement if the given key does not exist in this entry. *)
let remove k (qs : t) =
  if not @@ QM.mem k qs.state then
    invalid_arg "[quantum state] Key does not exist."
  else { qubits_n = qs.qubits_n; state = QM.remove k qs.state }

(** [update f combine qs] applies [f] to all key-value pairs existing in the map. The function [f] has to return a list
    (associative list) with all key-value pairs that should be present in the new updated map. If the function [f] 
    generates a key-value pair whose key already was added by an earlier invocation of [f], the [combine] function is 
    used to combine those two values, the existing one and the new one for the same key. *)
let update f (qs : t) =
  let ff k v m =
    (* args: key value map *)
    List.fold_left
      (fun m (k, v) ->
        if mem k m then
          let v = Complex.add (get k m) v in
          set k v m
        else add k v m)
      m (f k v)
  in
  QM.fold ff qs.state (empty qs.qubits_n)

(** [merge qs1 qs2 seq] merges the two quantum states into one, i.e. it merges the bit sequences of the key
    according to the sequence given as [seq] and updates the amplitudes accordingly. *)
let merge (qs1 : t) (qs2 : t) seq =
  QM.fold
    (fun k1 v1 m ->
      QM.fold
        (fun k2 v2 m ->
          let v = Complex.mul v1 v2 in
          add (merge_bits k1 k2 seq) v m)
        qs2.state m)
    qs1.state
    (empty (qs1.qubits_n + qs2.qubits_n))

(** [filter alpha qs] drops all basis-states with amplitudes below or equal to the threshold [alpha] and rescales the
    remaining amplitudes afterwards such that the sum over the squared absolute values of the amplitudes is still equal 
    to one. *)
let filter alpha (qs : t) =
  let { qubits_n; state } = qs in
  let state = QM.filter (fun _ v -> Complex.norm v > alpha) state in
  let sum =
    QM.fold
      (fun _ v sum ->
        let abs = Complex.norm v in
        (abs *. abs) +. sum)
      state 0.
  in
  (* rescale if not empty *)
  let state =
    if not @@ QM.is_empty state then
      let factor = 1.0 /. sum in
      QM.map (Complex.mul { re = factor; im = 0. }) state
    else state
  in
  { qubits_n; state }

let is_empty (qs : t) = QM.is_empty qs.state

(** [to_string qs] returns the quantum state as its string representation, e.g.
    (0.1243 + i0.2143)|0101001⟩ + ... *)
let to_string (qs : t) =
  let ff k v bf =
    Buffer.add_string bf "(\x1b[34m";
    Buffer.add_string bf (string_of_complex v);
    Buffer.add_string bf "\x1b[0m)\x1b[32m|";
    Buffer.add_string bf (bitstring_of_int k qs.qubits_n);
    Buffer.add_string bf "⟩\x1b[0m + ";
    bf
  in
  let bf = Buffer.create 16 in
  let bf = QM.fold ff qs.state bf in
  (* remove last plus sign *)
  let _ = Buffer.truncate bf (Buffer.length bf - 3) in
  Buffer.contents bf

let for_all_keys p (qs : t) = QM.for_all (fun k _ -> p k) qs.state
let exists_key p (qs : t) = QM.exists (fun k _ -> p k) qs.state
let for_all_values p (qs : t) = QM.for_all (fun _ v -> p v) qs.state
let exists_value p (qs : t) = QM.exists (fun _ v -> p v) qs.state
let cardinal (qs : t) = QM.cardinal qs.state
let count_keys p (qs : t) = QM.cardinal @@ QM.filter (fun k _ -> p k) qs.state
