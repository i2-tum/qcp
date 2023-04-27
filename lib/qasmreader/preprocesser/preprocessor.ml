(** The preprocessor resolves any includes of libraries. Those are searched in the directory `include/`. It basically
puts the content of each include file as is into the output file instead of the include statement. The output is
saved in a temporary file (it could but it is not yet deleted afterwards). The given [in_channel] is close and a new
[in_channel] from the generated output file is returned. 

Note: So far include statements in included libraries are not resolved. That could be included in a later step. In 
that case one would need to track already included libraries to recognize circular deendecies. *)

type source = {
  buf : bytes;
  mutable n : int;
  mutable line_start : bool;
  ic : in_channel;
}

let preprocess ic =
  let buf_len = 1024 in
  let sources =
    ref [ { buf = Bytes.create buf_len; n = 0; line_start = true; ic } ]
  in
  let clear () =
    let source = List.hd !sources in
    source.n <- 0
  in
  let shift n =
    let source = List.hd !sources in
    source.n <- source.n - n;
    Bytes.blit source.buf n source.buf 0 source.n
  in
  let write_out buf pos len =
    let source = List.hd !sources in
    if source.n <= len then (
      let s = source.n in
      Bytes.blit source.buf 0 buf pos s;
      clear ();
      pos + s)
    else (
      Bytes.blit source.buf 0 buf pos len;
      shift len;
      pos + len)
  in
  let rec f (buf : bytes) (pos : int) (len : int) : int =
    let source = List.hd !sources in
    let rec loop () =
      if source.n < 8 then
        let s = input source.ic source.buf source.n (buf_len - source.n) in
        if s > 0 then (
          (* EOF *)
          source.n <- source.n + s;
          loop ())
    in
    loop ();
    if source.n = 0 && List.compare_length_with !sources 1 = 0 then
      (* EOF main file *)
      pos
    else if source.n = 0 then (
      (* EOF include file *)
      close_in source.ic;
      sources := List.tl !sources;
      Bytes.set buf pos '\n';
      pos + 1)
    else if source.line_start && Bytes.sub_string source.buf 0 8 = "include "
    then (
      let src_file =
        match Bytes.index_from_opt source.buf 8 '\n' with
        | None ->
            let fname =
              Bytes.sub_string source.buf 8 (source.n - 8)
              ^ input_line source.ic
            in
            clear ();
            fname
        | Some k ->
            let fname = Bytes.sub_string source.buf 8 (k - 8) in
            shift (k + 1);
            fname
      in
      let src_file =
        "include/" ^ String.sub src_file 1 (String.length src_file - 3)
      in
      (* strip " and "; *)
      let ic = open_in src_file in
      source.line_start <- false;
      sources :=
        { buf = Bytes.create buf_len; n = 0; line_start = true; ic } :: !sources;
      f buf pos len)
    else
      match Bytes.index_from_opt source.buf 0 '\n' with
      | None ->
          source.line_start <- false;
          write_out buf pos len
      | Some k when k + 1 <= len ->
          source.line_start <- true;
          let new_pos = write_out buf pos (k + 1) in
          f buf new_pos (len + pos - new_pos)
      | Some _ ->
          source.line_start <- false;
          write_out buf pos len
  in
  fun buf n -> f buf 0 n
