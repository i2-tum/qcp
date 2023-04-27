(** [index_of x xs] returns the index of [x] in the list [xs] *)
let index_of x xs =
  let rec aux i = function
    | [] -> raise @@ Not_found
    | y :: _ when x = y -> i
    | _ :: ys -> aux (i + 1) ys
  in
  aux 0 xs

(** Make the same notation for shift left available as used in C/C++/Java/... *)
let ( << ) = Z.shift_left

(** Make the same notation for shift right available as used in C/C++/Java/... *)
let ( >> ) = Z.shift_right

(** [nth_bit x n] returns the [n]-th bit of [x]. *)
let nth_bit x n = Z.( mod ) (Z.( asr ) x n) (Z.of_int 2) |> Z.to_int

(** [flip_bit x n] flips the [n]-th bit in [x]. *)
let flip_bit x n = Z.logxor x (Z.one << n)

let set_nth_bith x n i =
  if not @@ List.mem i [ 0; 1 ] then
    invalid_arg "Bit value must be either 0 or 1"
  else if nth_bit x n <> i then flip_bit x n
  else x

(** [swap_bits x i j] swaps the [i]-th and the [j]-th bit in [x] with each other. *)
let swap_bits x i j =
  let bi = nth_bit x i and bj = nth_bit x j in
  set_nth_bith (set_nth_bith x i bj) j bi

(** [bitstring_of_int x n] returns the bitstring of the first (lowest) [n] bits of [x], e.g. 
   [bitstring_of_int 5 4 = "0101"] 
   @raise Invalid_argument  if [n] is non-positive
   @raise Invalid_argument  if [x] is negative *)
let bitstring_of_int x n =
  let bf = Buffer.create 16 in
  let rec loop i x =
    if i < n then (
      loop (i + 1) (Z.( asr ) x 1);
      Buffer.add_string bf (if Z.is_even x then "0" else "1"))
  in
  if Z.geq x Z.zero then
    if n > 0 then
      let _ = loop 0 x in
      Buffer.contents bf
    else invalid_arg "Length must be positive."
  else invalid_arg "Supports only non-negative numbers."

(** [string_of_complex c] returns a string representation of the complex number c, e.g. 
    [string_of_complex {re=.3; im=-.2} = "0.300000 - 0.200000i"] *)
let string_of_complex (c : Complex.t) =
  if c.im >= 0. then Printf.sprintf "%f + %fi" c.re c.im
  else Printf.sprintf "%f - %fi" c.re (-.c.im)

(** [map_state f state xs] is similar to [List.map f xs] just that the function [f] also receives a state, modifies it,
    and passes it to the next call of [f]. *)
let map_state f state xs =
  let rec aux state acc = function
    | [] -> state, List.rev acc
    | x :: xs ->
        let x, state = f state x in
        aux state (x :: acc) xs
  in
  aux state [] xs

(** [map_state_filter f state xs] is the same as [map_state] only that [f] returns an option where [None] symbolises to
    remove the corresponding item from the list and [Some (x, state)] to map the input to [x] with the new state 
    [state] to be passed to the next call of [f]. *)
let map_state_filter f state xs =
  let rec aux state acc = function
    | [] -> state, List.rev acc
    | x :: xs -> (
        match f state x with
        | None -> aux state acc xs
        | Some (x, state) -> aux state (x :: acc) xs)
  in
  aux state [] xs

(** [fold_left_state f state init xs] is similar to [List.fold_left f init xs] only that it also passes a state though 
    the calls of [f]. *)
let rec fold_left_state f state y xs =
  match xs with
  | [] -> y
  | x :: xs ->
      let y, state = f state y x in
      fold_left_state f state y xs

let init_fold_left len f x =
  if len < 0 then invalid_arg "List.init"
  else
    let rec aux i acc = if i >= len then acc else aux (i + 1) (f i acc) in
    aux 0 x

(** [merge_bits x y s] merges the bits of [x] and [y] from right to left (the lowest bits) according to the given
    sequence. A [true] value in the sequence denotes to take the next bit from [x], and a [false] value denotes to take
    the next bit from [y]. 
    
    Example: 
    [merge_bits 5 6 (fun () -> 
        Seq.Cons (true, fun () -> 
          Seq.Cons (false, fun () -> 
            Seq.Cons (false, fun () -> 
              Seq.Nil)))) = 5] *)
let merge_bits x y s =
  let rec aux x y z i s =
    match s () with
    | Seq.Nil -> z
    | Seq.Cons (true, s) ->
        aux (x >> 1) y (Z.( lor ) z (Z.( mod ) x (Z.of_int 2) << i)) (i + 1) s
    | Seq.Cons (false, s) ->
        aux x (y >> 1) (Z.( lor ) z (Z.( mod ) y (Z.of_int 2) << i)) (i + 1) s
  in
  aux x y Z.zero 0 s

(** Shortcut to express logical implication. *)
let ( ==> ) a b = (not a) || b

(** [unique xs] filter the list [xs] and returns a list only containing each element in [xs] once. *)
let unique xs =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (x :: acc) (List.filter (( <> ) x) xs)
  in
  aux [] xs

(** [uniqueq xs] same as [unique] but uses physical equality for its comparison. *)
let uniqueq xs =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (x :: acc) (List.filter (( != ) x) xs)
  in
  aux [] xs

(** [approx precision x y] returns [true] if [x] and [y] are equal within the given precision, i.e. if 
    |x-y| ≤ precision *)
let approx precision x y = Float.abs (y -. x) <= precision

(** Return the string represantation of a floating-point number. Try to approximate it with a fraction of [pi] or 
[sqrt(2)]within a given precision (default [1e-8]). *)
let pi_string_of_float ?(precision = 1e-8) x =
  (* [fraction x] returns a fraction of two integers as a tuple of two integers. This is done by first calculating the
     continued fraction of the given float and the reevaluating the continued fraction into a common fraction.

     {b Example} The fraction {% -\frac{27}{10} %} is represented by the tuple [(-27, 10)] (the second integer, i.e. the
     denominator is always positive). The same fraction can be expressed as the following continued fraction
     {% -2+\frac{1}{1+\frac{1}{2+\frac{1}{3}}} %} which in turn is represented by the list [[-2;1;2;3]].
     The calculation of the continued fraction will terminate either after the remainder shrinks below a specified
     threshold [precision] or latest after the specified number of steps.

     Note: that [continued_fraction_rev] will return the list of integers representing the continued fraction in reverse
     order and [rev_common_fraction] will receive and process this list in reverse order. *)
  let ( =~ ) = approx precision in
  let fraction ?(steps = 3) x =
    let rec continued_fraction_rev acc n x =
      match n with
      | 0 -> acc
      | _ ->
          let i = int_of_float x in
          let r = x -. float_of_int i in
          if r =~ 0. then i :: acc
          else continued_fraction_rev (i :: acc) (n - 1) (1. /. r)
    in
    let rec rev_common_fraction (n, m) = function
      | [] -> if n >= 0 then (m, n) else (-m, -n)
      | x :: xs -> rev_common_fraction (m, (x * m) + n) xs
    in
    rev_common_fraction (0, 1) (continued_fraction_rev [] steps x)
  in
  let pi_frac_n, pi_frac_m = fraction (x /. Float.pi) in
  if float_of_int pi_frac_n /. float_of_int pi_frac_m *. Float.pi =~ x then
    match (pi_frac_n, pi_frac_m) with
    | 0, _ -> "0"
    | 1, 1 -> "pi"
    | -1, 1 -> "-pi"
    | 1, _ -> "pi/" ^ string_of_int pi_frac_m
    | -1, _ -> "-pi/" ^ string_of_int pi_frac_m
    | _ ->
        if Int.abs pi_frac_m <= 20 then
          string_of_int pi_frac_n ^ "*pi/" ^ string_of_int pi_frac_m
        else Printf.sprintf "%F" x
  else
    let sqrt_frac_n, sqrt_frac_m = fraction (x /. sqrt 2.) in
    if float_of_int sqrt_frac_n /. float_of_int sqrt_frac_m *. sqrt 2. =~ x then
      match (sqrt_frac_n, sqrt_frac_m) with
      | 0, _ -> "0"
      | 1, 1 -> "sqrt(2)"
      | -1, 1 -> "-sqrt(2)"
      | 1, _ -> "sqrt(2)/" ^ string_of_int sqrt_frac_m
      | -1, _ -> "-sqrt(2)/" ^ string_of_int sqrt_frac_m
      | _ ->
          if Int.abs sqrt_frac_m <= 20 then
            string_of_int sqrt_frac_n ^ "*sqrt(2)/" ^ string_of_int sqrt_frac_m
          else Printf.sprintf "%F" x
    else Printf.sprintf "%F" x

let matrix_mul a b =
  if
    Array.length a = 2
    && Array.for_all (fun r -> Array.length r = 2) a
    && Array.length b = 2
    && Array.for_all (fun r -> Array.length r = 2) a
  then
    [|
      [|
        Complex.add
          (Complex.mul a.(0).(0) b.(0).(0))
          (Complex.mul a.(0).(1) b.(1).(0));
        Complex.add
          (Complex.mul a.(0).(0) b.(0).(1))
          (Complex.mul a.(0).(1) b.(1).(1));
      |];
      [|
        Complex.add
          (Complex.mul a.(1).(0) b.(0).(0))
          (Complex.mul a.(1).(1) b.(1).(0));
        Complex.add
          (Complex.mul a.(1).(0) b.(0).(1))
          (Complex.mul a.(1).(1) b.(1).(1));
      |];
    |]
  else invalid_arg "Ill-formated arguments, not 2x2 matrices"
