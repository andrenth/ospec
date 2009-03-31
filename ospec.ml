(* http://pleac.sourceforge.net/pleac_ocaml/strings.html#AEN68 *)
let eval text =
  let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)

let run_specs file =
  ignore (Toploop.use_silently Format.std_formatter file);
  Pa_spec.report ()

let () =
  Sys.interactive := false;
  Toploop.initialize_toplevel_env ();
  eval "open Pa_spec;;";
  for i = 1 to (Array.length Sys.argv) - 1 do
    run_specs Sys.argv.(i);
    Printf.printf "\n"
  done
