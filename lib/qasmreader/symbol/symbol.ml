(* This module provides an implementation of an environemtn storing symbols with their associated data of type ['a].
   this could be its type or value for example. To make the lookup more efficient, identifiers are first translated into
   symbols which basically identify each identifier with an integer. Right now the associated data is looked up in a
   hashtable. However, the translation to integer could be further exploited for better runtime with a more efficient
   datastructure. *)
type symbol = string * int
type 'a environment = (int, 'a) Hashtbl.t Stack.t

exception Symbol_not_found of string

(* storing the next free integer to assign to an identifier *)
let next_symbol = ref 0

let symbol_tbl =
  let stack = Stack.create () in
  Stack.push (Hashtbl.create 89) stack;
  stack

(** [symbol id] returns the symbol for an given identifier. Creates a new symbol representation if this identifier was 
    not queried yet. *)
let symbol id : symbol =
  (* id stands for identifier; i is the unique number for the identifier *)
  let find s_opt tbl =
    match s_opt with None -> Hashtbl.find_opt tbl id | Some s -> Some s
  in
  match Stack.fold find None symbol_tbl with
  | None ->
      let i = !next_symbol in
      next_symbol := i + 1;
      Hashtbl.add (Stack.top symbol_tbl) id i;
      (id, i)
  | Some i -> (id, i)

(** [name symb] returns the name of the idetifier associated with this symbol. *)
let name (symb : symbol) =
  let id, _ = symb in
  id

(** [openScope env] opens a new scope, i.e. it puts a new layer on the environment. This is used if a new scope in the
    program starts, for example in the body of a function. Queried symbols are looked up first in the uppermost scope,
    if they do not exist their than one goes down layer by layer until the symbol is found. *)
let openScope ?(size = 89) (env : 'a environment) =
  Stack.push (Hashtbl.create size) env

(** [closeScope env] closes a Scope by removing the uppermost layer. For more detail, see also [Symbol.openScope] *)
let closeScope env = ignore (Stack.pop env)

(** [empty size] initializes a new empty environment. *)
let empty size : 'a environment =
  let stack = Stack.create () in
  openScope ~size stack;
  stack

(** [add env symb v] adds a new symbol with the associated data [v] to the uppermost layer of the environment. *)
let add (env : 'a environment) symb v =
  let _, i = symb in
  Hashtbl.add (Stack.top env) i v

(** [get env symb] returns the data associated with the given symbol as an option. Data present in a higher layer will 
    shadow data for the same symbol in a lower layer in the environment. If the symbol is not found [None] will be 
    returned. *)
let get (env : 'a environment) symb =
  let _, i = symb in
  let find v_opt tbl =
    match v_opt with None -> Hashtbl.find_opt tbl i | Some v -> Some v
  in
  try Option.get (Stack.fold find None env)
  with Invalid_argument _ -> raise @@ Symbol_not_found (name symb)
