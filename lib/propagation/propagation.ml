open Quantumstate
open Flatlattice
open Uniontable
open Circuit
open Util
open Logging

(** This files contains a module whose [transform] function performs quantum constant propagation to reduce the number
    of controls of controlled gates. The quantum constatnt propagation is a restricted simulation of the circuit, which
    means that the circuits is simulated until the state of subgroups of qubits of the system becomes too complex. In
    this case one switches to ⊤ for the state describing that subsystem. 
    
    TODO:
    - implement the change to ⊤ if maximum number of summands is reached 
    - write tests
    - refactor code, i.e. make it more modular, e.g. smaller (and more) functions
  *)

(** This module actually implements the quantum constant propagation. *)
module Propagation = struct
  type state = Quantumstate.t Flatlattice.t Uniontable.t

  let summands_per_basis u s =
    if s <> 0 && s <> 1 then invalid_arg "[propagation] basis must be 0 or 1."
    else if u.(0).(s) = Complex.zero then
      if u.(1).(s) = Complex.zero then 0 else 1
    else if u.(1).(s) = Complex.zero then 1
    else 2

  (** Applies a given gate to the give state, i.e. the state is modified according to the gate application. This 
      function does not perform any optimisations such as gate elimination or control reduction.*)
  let rec apply nmax alpha gate (state : state) =
    match gate with
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
    | Z i ->
        (* all non-controlled single-qubit gates *)
        if get i state = TOP then
          (* cannot do anything, entanglement group is already in top *)
          state
        else
          (* the position of qubit [i] within its entanglement group is [j] *)
          let j = pos_in_group i state in
          (* get the matrix description of the [gate] *)
          let u = unitary_of_gate gate in
          let n =
            count_keys (fun k -> nth_bit k j = 0) (the @@ get i state)
            * summands_per_basis u 0
            + count_keys (fun k -> nth_bit k j = 1) (the @@ get i state)
              * summands_per_basis u 1
          in
          (* calculate the number of summands estimated after the application of the gate without actually calculating
             the state. For that we count all states where qubit [i] is in |0⟩ state and multiply it by the number of non-
             zero entries in the corresponding (first) column of the unitary. Analogously, we do the same for the state |1⟩
             and add the result. *)
          if n > nmax then set i TOP state
          else
            (* n ≤ nmax *)
            (* if the estimated number is less or equal to the maximum number of summands, we continue *)
            (* return all key-value pairs that need to be inserted in the transformed state for each key-value in the
               initial state. A pair is only added if its value is non-zero. *)
            let mul k v =
              if nth_bit k j = 0 then
                let bindings =
                  if u.(0).(0) <> Complex.zero then
                    [ (k, Complex.mul v u.(0).(0)) ]
                  else []
                in
                if u.(1).(0) <> Complex.zero then
                  (flip_bit k j, Complex.mul v u.(1).(0)) :: bindings
                else bindings
              else
                (* nth_bit k j = 1 *)
                let bindings =
                  if u.(0).(1) <> Complex.zero then
                    [ (flip_bit k j, Complex.mul v u.(0).(1)) ]
                  else []
                in
                if u.(1).(1) <> Complex.zero then
                  (k, Complex.mul v u.(1).(1)) :: bindings
                else bindings
            in
            let top_if_empty state =
              let state = filter alpha state in
              if is_empty state then TOP else ELEMENT state
            in
            (* apply the above defined transformations to the [state]. *)
            set i (top_if_empty @@ update mul (the @@ get i state)) state
    | ECR (i1, i2) | RXX (_, i1, i2) | RYY (_, i1, i2) | RZZ (_, i1, i2) ->
        if get i1 state = TOP then set i2 TOP state
        else if get i2 state = TOP then set i1 TOP state
        else
          let j1 = pos_in_group i1 state in
          let j2 = pos_in_group i2 state in
          (* get the matrix description of the [gate] *)
          let u = unitary_of_gate gate in
          let n = 0 (* TODO *) in
          (* calculate the number of summands estimated after the application of the gate without actually calculating
             the state. For that we count all states where qubit [i] is in |0⟩ state and multiply it by the number of non-
             zero entries in the corresponding (first) column of the unitary. Analogously, we do the same for the state |1⟩
             and add the result. *)
          if n > nmax then
            let state = set i1 TOP state in
            set i2 TOP state
          else
            let state =
              if same i1 i2 state then state
              else
                let top_if_empty state =
                  let state = filter alpha state in
                  if is_empty state then TOP else ELEMENT state
                in
                union i1 i2
                  (fun x y seq -> top_if_empty @@ merge (the x) (the y) seq)
                  state
            in
            let mul k v =
              if nth_bit k j1 = 0 && nth_bit k j2 = 0 then
                (* |00⟩ *)
                let bindings =
                  if u.(0).(0) <> Complex.zero then
                    [ (k, Complex.mul v u.(0).(0)) ]
                  else []
                in
                let bindings =
                  if u.(1).(0) <> Complex.zero then
                    (flip_bit k j2, Complex.mul v u.(1).(0)) :: bindings
                  else bindings
                in
                let bindings =
                  if u.(2).(0) <> Complex.zero then
                    (flip_bit k j1, Complex.mul v u.(2).(0)) :: bindings
                  else bindings
                in
                if u.(3).(0) <> Complex.zero then
                  (flip_bit (flip_bit k j1) j2, Complex.mul v u.(3).(0))
                  :: bindings
                else bindings
              else if nth_bit k j1 = 0 && nth_bit k j2 = 1 then
                (* |01⟩ *)
                let bindings =
                  if u.(0).(1) <> Complex.zero then
                    [ (flip_bit k j2, Complex.mul v u.(0).(1)) ]
                  else []
                in
                let bindings =
                  if u.(1).(1) <> Complex.zero then
                    (k, Complex.mul v u.(1).(1)) :: bindings
                  else bindings
                in
                let bindings =
                  if u.(2).(1) <> Complex.zero then
                    (flip_bit (flip_bit k j1) j2, Complex.mul v u.(2).(1))
                    :: bindings
                  else bindings
                in
                if u.(3).(1) <> Complex.zero then
                  (flip_bit k j1, Complex.mul v u.(3).(1)) :: bindings
                else bindings
              else if nth_bit k j1 = 1 && nth_bit k j2 = 1 then
                (* |10⟩ *)
                let bindings =
                  if u.(0).(2) <> Complex.zero then
                    [ (flip_bit k j1, Complex.mul v u.(0).(2)) ]
                  else []
                in
                let bindings =
                  if u.(1).(2) <> Complex.zero then
                    (flip_bit (flip_bit k j1) j2, Complex.mul v u.(1).(2))
                    :: bindings
                  else bindings
                in
                let bindings =
                  if u.(2).(2) <> Complex.zero then
                    (k, Complex.mul v u.(2).(2)) :: bindings
                  else bindings
                in
                if u.(3).(2) <> Complex.zero then
                  (flip_bit k j2, Complex.mul v u.(3).(2)) :: bindings
                else bindings
              else
                (* |11⟩ *)
                let bindings =
                  if u.(0).(3) <> Complex.zero then
                    [ (flip_bit (flip_bit k j1) j2, Complex.mul v u.(0).(3)) ]
                  else []
                in
                let bindings =
                  if u.(1).(3) <> Complex.zero then
                    (flip_bit k j1, Complex.mul v u.(1).(3)) :: bindings
                  else bindings
                in
                let bindings =
                  if u.(2).(3) <> Complex.zero then
                    (flip_bit k j2, Complex.mul v u.(2).(3)) :: bindings
                  else bindings
                in
                if u.(3).(3) <> Complex.zero then
                  (k, Complex.mul v u.(3).(3)) :: bindings
                else bindings
            in
            let top_if_empty state =
              let state = filter alpha state in
              if is_empty state then TOP else ELEMENT state
            in
            (* apply the above defined transformations to the [state]. *)
            set i1 (top_if_empty @@ update mul (the @@ get i1 state)) state
    | RCCX (i, j, k) ->
        (* decomposition of the simplified toffoli gate according to https://arxiv.org/pdf/quant-ph/0312225.pdf *)
        state
        |> apply nmax alpha (CTRL ([ i; j ], 0., Y k))
        |> apply nmax alpha (CTRL ([ i ], 0., Z k))
    | CTRL (cs, gamma, (H i as gate))
    | CTRL (cs, gamma, (X i as gate))
    | CTRL (cs, gamma, (Y i as gate))
    | CTRL (cs, gamma, (Z i as gate))
    | CTRL (cs, gamma, (S i as gate))
    | CTRL (cs, gamma, (SDG i as gate))
    | CTRL (cs, gamma, (T i as gate))
    | CTRL (cs, gamma, (TDG i as gate))
    | CTRL (cs, gamma, (SX i as gate))
    | CTRL (cs, gamma, (RX (_, i) as gate))
    | CTRL (cs, gamma, (RY (_, i) as gate))
    | CTRL (cs, gamma, (U2 (_, _, i) as gate))
    | CTRL (cs, gamma, (U3 (_, _, _, i) as gate))
    | CTRL (cs, gamma, (P (_, i) as gate)) ->
        (* Will not try to create the smallest entanglement group since it assumes that all superflous qubits, e.g.
           those that are in a computational basis state were removed before *)
        (* Opposed to earlier assumptions, all affected qubits must be enclosed in one entanglement group *)
        (* construct the union of all elements covered by the controls and the target qubit. *)
        (* For the union of separate groups, we need a filtered list of control qubits. One that only contains one
           qubit per entry (= entanglement group), here the first one of those is choosen. *)
        let rec filter_cs acc = function
          | [] -> (
              match acc with
              | [] -> invalid_arg "[propagation] this should not happen."
              | [ c ] ->
                  (* if in the end only one control remains, we keep it in the
                      list if it needs to be united with [i], otherwise, we return an empty list. *)
                  if same i c state then [] else [ c ]
              | _ ->
                  List.rev acc (* here [acc] contains at least two elements. *))
          | c :: cs ->
              (* add the first element of the list to [acc] and filter out qubits in the same entanglement group in the
                  remaining list. *)
              filter_cs (c :: acc)
                (List.filter (fun x -> not @@ same c x state) cs)
        in
        let filtered_cs = filter_cs [] cs in
        let state : state =
          List.fold_left
            (fun state c ->
              union i c
                (fun x y seq ->
                  match (x, y) with
                  | ELEMENT x, ELEMENT y -> element @@ merge x y seq
                  | _, _ -> TOP)
                state)
            state filtered_cs
        in
        (* we only need to continue when the entanglement group is not in top yet *)
        if get i state = TOP then state
        else
          (* get the matrix description of the basic gate without controls *)
          let u = unitary_of_gate gate in
          (* apply gloabl phase *)
          let u =
            matrix_mul u
              [|
                [| Complex.polar 1. gamma; Complex.zero |];
                [| Complex.zero; Complex.polar 1. gamma |];
              |]
          in
          (* calculates the number [n] of summands in the element that will be generated.
              This number can be divided into two parts. The latter part are all combinations where at least one control
              qubit is not satisfied and, hence, the identity is applied to the target qubit not changing the number of
              those state. The first part comprises all states where all control qubits are satisfied and, hence, the
              gate is applied to the target qubit. In this case we need to multiply the number of states where all
              control qubits are satisfied with the number of states induced by the application of the gate separated
              by those where the target state is |0⟩ and those where it is |1⟩. *)
          let group = the @@ get i state in
          let js = List.map (fun c -> pos_in_group c state) cs in
          let j = pos_in_group i state in
          (* we count the states per group where all control qubits are satisfied *)
          let n =
            count_keys
              (fun k ->
                List.for_all (fun j -> nth_bit k j = 1) js && nth_bit k j = 0)
              group
            * summands_per_basis u 0
            + count_keys
                (fun k ->
                  List.for_all (fun j -> nth_bit k j = 1) js && nth_bit k j = 1)
                group
              * summands_per_basis u 1
            + count_keys
                (fun k -> List.for_all (fun j -> nth_bit k j = 0) js)
                group
          in
          if n > nmax then set i TOP state
          else
            (* n ≤ nmax *)
            (* if all controls are satisfied the gate is applied similar to the uncontrolled case, if not the key-value pair
                is inserted unchanged again. *)
            let mul k v =
              if List.for_all (fun j -> nth_bit k j = 1) js then
                if nth_bit k j = 0 then
                  let bindings =
                    if u.(0).(0) <> Complex.zero then
                      [ (k, Complex.mul v u.(0).(0)) ]
                    else []
                  in
                  if u.(1).(0) <> Complex.zero then
                    (flip_bit k j, Complex.mul v u.(1).(0)) :: bindings
                  else bindings
                else
                  (* nth_bit k j = 1 *)
                  let bindings =
                    if u.(0).(1) <> Complex.zero then
                      [ (flip_bit k j, Complex.mul v u.(0).(1)) ]
                    else []
                  in
                  if u.(1).(1) <> Complex.zero then
                    (k, Complex.mul v u.(1).(1)) :: bindings
                  else bindings
              else [ (k, v) ]
            in
            let top_if_empty state =
              let state = filter alpha state in
              if is_empty state then TOP else ELEMENT state
            in
            set i (top_if_empty @@ update mul (the @@ get i state)) state
    | SWAP (i1, i2) -> swap i1 i2 state
    | MEASURE (i, _) ->
        let _ =
          log_msg WARN
            ("[propagation]: silently skipped measure on " ^ string_of_int i)
        in
        state
    | _ ->
        invalid_arg @@ "[propagation]: gate not supported to apply on state: "
        ^ string_of_gate gate

  let map nmax alpha state gate =
    let gate =
      match gate with
      | CTRL (cs, gamma, gate) ->
          (* reduce controls here *)
          (* returns [true] if a predicate [p] on the bit corresponding to the index is satisfied for all keys *)
          let filter p i =
            if get i state = TOP then false
            else
              let j = pos_in_group i state in
              for_all_keys (fun k -> p @@ nth_bit k j) (the @@ get i state)
          in
          (* contains all indices whose bit is always zero for all keys *)
          let zero = List.filter (filter (( = ) 0)) cs in
          (* contains all indices whose bit is always one for all keys *)
          let one = List.filter (filter (( = ) 1)) cs in
          (* contains all indices whose bit is both one for some keys and zero for other keys *)
          let both =
            List.filter (fun i -> not (List.mem i zero || List.mem i one)) cs
          in
          if both = [] then
            (* all controls are classically determined *)
            if zero = [] then
              (* all controls are in the one state
                 => gate is always applied (controls can be removed entirely) *)
              Some gate
            else
              (* all controls are in the zero state
                 => gate is never applied (can be removed entirely) *)
              None
          else
            (* there are indices whose bits are sometimes one and sometimes zero *)
            (* check whether there is an all one state, if not never all controls can be one at the same time and thus the
               gate can be removed entirely *)
            (* the function [group] splits a list of indices in groups corresponding to different sets in the state,
               i.e. qubits within one group are entangled but not with ones outside the group *)
            let rec group acc = function
              | [] -> acc
              | c :: _ as cs ->
                  let c1, c2 = List.partition (fun x -> same c x state) cs in
                  group (c1 :: acc) c2
            in
            let groups = group [] cs in
            (* this function assumes that all indices are in one group, i.e. they are entangled *)
            let exists_all_one is =
              let i = List.hd is in
              if get i state = TOP then true
              else
                let js = List.map (fun i -> pos_in_group i state) is in
                exists_key
                  (fun k -> List.for_all (fun j -> nth_bit k j = 1) js)
                  (the @@ get i state)
            in
            if List.for_all exists_all_one groups then
              (* there is a state such that every control can be satisfied *)
              (* Within each group there may be superflouos bits those whose one state is implied by another bit in the
                 group. So we minimize the number of indices in each group such that if all of the remainig indices are in
                 the one state the rest is as well. *)
              let rec filter_group acc = function
                | [] -> acc
                | c :: cs ->
                    if get c state = TOP then
                      (* no controls can be removed because we do not know anything *)
                      c :: cs
                    else
                      let j = pos_in_group c state in
                      (* remove all indices whose one states are implied by [c] *)
                      (* NOTE: the choice of indices to remove is not always unique, for always the first one of a group of equal
                         "strong" indices is taken an the rest is removed. There is space for optimization if one chooses the
                         control to keep carefully such that it cancels out with another gate for example.
                         In case of controlled phase gates there is even thed choice which index is the control and which one the
                         target index. This could also be exploited further. *)
                      let filter i =
                        i <= c
                        (* smaller indices must not be considered in the condition below, see also note above. *)
                        || not
                           @@ for_all_keys
                                (fun k ->
                                  nth_bit k j = 1
                                  ==> (nth_bit k (pos_in_group i state) = 1))
                                (the @@ get c state)
                      in
                      filter_group (List.filter filter acc) cs
              in
              (* filter the groups *)
              let groups = List.map (fun cs -> filter_group cs cs) groups in
              (* flatten the groups again for the final result (cannot be empty) *)
              let both = List.concat groups in
              let gate = CTRL (both, gamma, gate) in
              Some gate
            else
              (* there is no state where all controls are one at the same time
                 => the gate will never be applied (and can be removed entirely) *)
              None
      | _ ->
          (* uncontrolled gates will not be optimised by this pass *)
          Some gate
    in
    match gate with
    | None ->
        (* if the gate was removed entirely, the state does not need to be modified and we signal the filter to
           remove the gate by returning [None]. *)
        None
    | Some gate ->
        (* the state is modified according to the gate with the minimised number of control qubits *)
        let state = apply nmax alpha gate state in
        Some (gate, state)

  (** Transform applies the constant propagation and removes superflouos controls by simulating the circuit partially.
      @param nmax Threshold to limit the number of coefficients
      @param alpha Threshold to cut off coeeficients *)
  let transform nmax alpha circ =
    (* Intialise the state with one element per key, i.e. the qubits are not entangled. *)
    let state =
      make circ.size (element @@ Quantumstate.init Z.zero Complex.one 1)
    in
    (* Map every gate to a gate where the controls are minimised or the gate is removed entirely. For that pass the
       state to the mapping function that keeps track of the current quantum state in a restricted fashion. *)
    let _, gates = map_state_filter (map nmax alpha) state circ.gates in
    { size = circ.size; gates }
end
