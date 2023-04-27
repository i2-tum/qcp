type 'a t = TOP | ELEMENT of 'a | BOT

let least_upper_bound x y =
  match (x, y) with BOT, x | x, BOT -> x | x, y when x = y -> x | _ -> TOP

let greatest_lower_bound x y =
  match (x, y) with TOP, x | x, TOP -> x | x, y when x = y -> x | _ -> BOT

let sup = least_upper_bound
let inf = greatest_lower_bound

let the = function
  | ELEMENT x -> x
  | _ -> invalid_arg "No element, either TOP or BOT"

let element x = ELEMENT x

let to_string string_of_element = function
  | TOP -> "\x1b[1;31m⊤\x1b[0m"
  | ELEMENT x -> string_of_element x
  | BOT -> "\x1b[1;31m⊥\x1b[0m"
