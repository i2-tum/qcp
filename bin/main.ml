open Logging

let usage_msg = "qcprop -c <config.yml> [<input.qasm>] [-o <output.qasm>]"
let tar_f = ref ""
let src_f = ref ""
let config_f = ref ""

let anon_fun fname =
  match !src_f with
  | "" -> src_f := fname
  | _ -> raise @@ Arg.Bad "Only one input file allowed"

let speclist =
  [
    ("-c", Arg.Set_string config_f, "configuration file in YAML format");
    ("-o", Arg.Set_string tar_f, "output to file, otherwise to stdout");
  ]

let () = Arg.parse speclist anon_fun usage_msg
let src = match !src_f with "" -> stdin | fname -> open_in fname
let tar = match !tar_f with "" -> stdout | fname -> open_out fname

let config =
  match !config_f with
  | "" -> raise @@ Arg.Bad "no config file specified"
  | fname ->
      let fname_json =
        String.sub fname 0 (String.length fname - 4)
        ^ "_"
        ^ string_of_int (Unix.getpid ())
        ^ ".json"
      in
      let _ = Sys.command ("yq -o json " ^ fname ^ " > " ^ fname_json) in
      let config = Yojson.Basic.from_file fname_json in
      let () = Sys.remove fname_json in
      config

let pass_of_string = function
  | "propagation", opts ->
      let nmax =
        match List.assoc_opt "nmax" opts with
        | Some nmax -> Yojson.Basic.Util.to_int nmax
        | None ->
            log_msg WARN "No nmax value provided, using default value 1024";
            1024
      in
      let alpha =
        match List.assoc_opt "alpha" opts with
        | Some alpha -> Yojson.Basic.Util.to_number alpha
        | None ->
            log_msg WARN "No alpha value provided, using default value 0.0";
            0.0
      in
      Propagation.Propagation.transform nmax alpha
  | p, _ -> failwith @@ "Error in config file, unknown pass" ^ p

let passes =
  let open Yojson.Basic.Util in
  config |> member "passes" |> to_assoc
  |> List.map (fun (k, v) -> (k, match v with `Null -> [] | _ -> to_assoc v))
  |> List.map pass_of_string

let () = Qcprop.process src tar passes
let () = match !src_f with "" -> () | _ -> close_in src
let () = match !tar_f with "" -> () | _ -> close_out tar
