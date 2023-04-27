(** A UnionTable (UT) stores entries in a list fixed in size (i.e. an array).
    Each entry contains the indices sharing this entry and the elem(ent) of that entry.
    All functions are implemented in an immutable fashion, i.e. they will not modify the input data-structure and 
    return a new modified instance. *)

type 'a entry = { card : int; idxs : int list; elem : 'a }
type 'a t = { size : int; perm : int array; content : 'a entry array }

(** [make n x] creates a UnionTable with the inital value [x] in all [n] entries. Each entry corresponds to its own
    set, no entries are united yet into a common set. *)
let make n x : 'a t =
  {
    size = n;
    perm = Array.init n Fun.id;
    content = Array.init n (fun i -> { card = 1; idxs = [ i ]; elem = x });
  }

(** [init n f] creates a UnionTable of size [n] with each entry initialised according to the initialization function 
    [f]. *)
let init n f : 'a t =
  {
    size = n;
    perm = Array.init n Fun.id;
    content = Array.init n (fun i -> { card = 1; idxs = [ i ]; elem = f i });
  }

(** [pos_in_group i ut] returns the position of [i] in the group where [i] belongs to and considers thereby the 
    permutation. *)
let pos_in_group i (ut : 'a t) =
  let i' = ut.perm.(i) in
  let rec aux p = function
    | [] -> raise Not_found
    | j :: _ when j = i' -> p
    | _ :: js -> aux (p + 1) js
  in
  aux 0 ut.content.(i').idxs

(** [get i ut] returns the element in the entry pointed to by [i].  *)
let get i (ut : 'a t) = ut.content.(ut.perm.(i)).elem

(** [same i j ut] returns [true] if and only if [i] and [j] point to the same entry in the union table. *)
let same i j (ut : 'a t) =
  i = j || ut.content.(ut.perm.(j)).idxs == ut.content.(ut.perm.(i)).idxs

(** [set i x ut] updates element in the entry pointed to by [i]. *)
let set i x (ut : 'a t) : 'a t =
  let new_entry =
    {
      card = ut.content.(ut.perm.(i)).card;
      idxs = ut.content.(ut.perm.(i)).idxs;
      elem = x;
    }
  in
  {
    size = ut.size;
    perm = ut.perm;
    content =
      Array.init ut.size (fun j ->
          if
            j = ut.perm.(i)
            || ut.content.(j).idxs = ut.content.(ut.perm.(i)).idxs
          then new_entry
          else ut.content.(j));
  }

(** [add i x ut] adds the element [x] to the entry pointed to by [i]. Same as [set i x ut]. *)
let add = set

(** [swap i j ut] swaps two elements with each other. This is done by altering the permutation that serves as a view on 
    the content laying behind it. All accesses to the content go through the permutation. *)
let swap i j (ut : 'a t) : 'a t =
  {
    size = ut.size;
    perm =
      Array.init ut.size (fun x ->
          if x = i then ut.perm.(j)
          else if x = j then ut.perm.(i)
          else ut.perm.(x));
    content = ut.content;
  }

(** [cardinal i ut] returns the number of element in the entry pointed to by [i]. *)
let cardinal i (ut : 'a t) = ut.content.(ut.perm.(i)).card

(** [union i j combine ut] unites the two entries pointed to by [i] and [j]. To retrieve the new element in
    the new entry the [combine] function is used. 
    
    The [combine] function receives both elements of the formerly disjoint entries. Additionally, the [combine] 
    function receives a sequence of decision who the two index lists of the two old entries were merged. Here, both 
    lists are traversed from left to right and a [true] entry symbols that the next element was taken from the first 
    index list, a [false] entry that it was taken from the second list. 
    
    @raise  Invalid_argument  If indices point to the same entry. *)
let union i j combine (ut : 'a t) : 'a t =
  if same i j ut then invalid_arg "[union_table]: Sets are already united."
  else
    (* assumes that [ls] and [rs] are sorted, performs one merge step similar to merge sort;
        it tracks its decisions in a list (no sequence because it needs to be reversed in the end) *)
    let rec merge ls rs seq acc =
      match (ls, rs) with
      | [], [] -> (List.rev seq, List.rev acc)
      | l :: ls, ([] as rs) -> merge ls rs (true :: seq) (l :: acc)
      | l :: ls, (r :: _ as rs) when l <= r ->
          merge ls rs (true :: seq) (l :: acc)
      | ls, r :: rs -> merge ls rs (false :: seq) (r :: acc)
    in
    let merge_seq, idxs =
      merge ut.content.(ut.perm.(i)).idxs ut.content.(ut.perm.(j)).idxs [] []
    in
    let merge_seq = List.to_seq merge_seq in
    let elem =
      combine ut.content.(ut.perm.(i)).elem ut.content.(ut.perm.(j)).elem
        merge_seq
    in
    (* build the new entry *)
    let new_entry =
      {
        card = ut.content.(ut.perm.(i)).card + ut.content.(ut.perm.(j)).card;
        idxs;
        elem;
      }
    in
    (* update all references to the two old entries to point to the single new combined one. *)
    {
      size = ut.size;
      perm = ut.perm;
      content =
        Array.init ut.size (fun i ->
            if List.mem i new_entry.idxs then new_entry else ut.content.(i));
    }

(** [to_string f ut] returns a string describing the state of the union table.
    
    For that it requires a function that maps the elements to strings. The function [f] receives an element.
    The output looks as follows for a union table of size 3:

    [[1, 3]: (f x)]

    [[2]: (f y)] *)
let to_string f (ut : 'a t) =
  let perm_rev = Array.make ut.size 0 in
  let _ =
    for i = 0 to ut.size - 1 do
      perm_rev.(ut.perm.(i)) <- i
    done
  in
  let rec loop i (bf : Buffer.t) : Buffer.t =
    if i = ut.size then bf
    else (
      if
        ut.content.(ut.perm.(i)).idxs
        |> List.map (fun i -> perm_rev.(i))
        |> List.fold_left Int.min Int.max_int
        = i
      then (
        (* otherwise, state was already printed *)
        Buffer.add_string bf "[";
        Buffer.add_string bf (string_of_int i);
        List.iter
          (fun i ->
            i |> string_of_int
            |>
            (Buffer.add_string bf ", ";
             Buffer.add_string bf))
          (List.tl (ut.content.(ut.perm.(i)).idxs |> List.sort Int.compare));
        Buffer.add_string bf "]: ";
        let s = f @@ get i ut in
        Buffer.add_string bf s;
        Buffer.add_string bf "\n");
      loop (i + 1) bf)
  in
  Buffer.create 16 |> loop 0 |> Buffer.contents
