open Printf

(* http://pleac.sourceforge.net/pleac_ocaml/strings.html#AEN68 *)
let eval text =
  let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)

let run_specs file report =
  ignore (Toploop.use_silently Format.std_formatter file);
  eval (sprintf "%s (Spec.specs ()); Spec.cleanup ();;" report)

let usage =
  sprintf "Usage: %s [options]" Sys.executable_name

let parse_args () =
  let fmt = ref "nested" in
  let files = ref [] in
  let arg_spec = [ "-format", Arg.Set_string fmt, "Report format" ] in
  let anon s = files := s :: !files in
  Arg.parse arg_spec anon usage;
  (!fmt, List.rev !files)

let report_function_name = function
  | "nested" -> "Report.nested"
  | "progress" -> "Report.progress"
  | s -> raise (Arg.Bad (sprintf "unkown format `%s' specified" s))

let rec run_files report = function
  | [] -> ()
  | [file] -> run_specs file report
  | file::rest ->
      run_specs file report;
      printf "\n";
      run_files report rest

let load_object_files files =
  let dir = Findlib.package_directory "ospec" in
  let fmt = Format.std_formatter in
  Topdirs.dir_directory dir;
  List.iter (fun file -> Topdirs.dir_load fmt (dir ^ "/" ^ file)) files

let () =
  Sys.interactive := false;
  Toploop.initialize_toplevel_env ();
  load_object_files ["spec_types.cmo"; "spec.cmo"; "helpers.cmo"];
  eval "open Helpers;;";
  let fmt, files = parse_args () in
  let report = report_function_name fmt in
  run_files report files
