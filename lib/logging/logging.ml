type level = DEBUG | INFO | WARN | ERROR | FATAL

let counter = ref 0
let max_meassages = 4

let log_msg lvl msg =
  if !counter < max_meassages || lvl = DEBUG then
    let _ = counter := !counter + 1 in
    match lvl with
    | DEBUG -> Printf.eprintf "\x1b[1;34mDebug\x1b[0m %s\n" msg
    | INFO -> Printf.eprintf "\x1b[1;33mInfo\x1b[0m %s\n" msg
    | WARN -> Printf.eprintf "\x1b[1;35mWarning\x1b[0m %s\n" msg
    | ERROR -> Printf.eprintf "\x1b[1;31mError\x1b[0m %s\n" msg
    | FATAL -> Printf.eprintf "\x1b[1;31mFatal Error\x1b[0m %s\n" msg
  else if !counter = max_meassages then
    let _ = counter := !counter + 1 in
    Printf.eprintf "more than %d messages generated\n" max_meassages
