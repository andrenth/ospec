open Spec_types
open Global

let report_failures () =
  if not (Queue.is_empty failures) then begin
    let report failure =
      let expected =
        match failure.expected with
        | Some e -> Printf.sprintf " %s" e
        | None -> "" in
      Printf.printf "%d) Expected `%s %s%s' to return %s\n"
        failure.id failure.operation failure.result expected
        (match failure.kind with
        | Positive -> "true"
        | Negative -> "false") in
    Queue.iter report failures
  end

let spec_footer spec =
  Printf.printf "\n%d examples, %d failures, %d pending\n\n"
                (Queue.length spec.examples)
                (Queue.length failures)
                !total_pending

let generic spec_header example_header results_footer example_footer
    message_of_result specs =
  let report_spec spec =
    spec_header spec;
    let report_example example =
      example_header example;
      let report_results res =
        Printf.printf "%s" (message_of_result res);
        try
          match res with
          | Ok -> ()
          | _ -> raise Exit
        with Exit -> () in
      Queue.iter report_results example.results;
      results_footer () in
    Queue.iter report_example spec.examples;
    example_footer ();
    spec_footer spec in
  Queue.iter report_spec specs;
  report_failures ();
  cleanup ()

let nested specs =
  let spec_header spec = Printf.printf "%s\n" spec.name in
  let example_header example = Printf.printf "  %s" example.description in
  let results_footer () = Printf.printf "\n" in
  let example_footer () = () in
  let message_of_result = function
    | Ok -> ""
    | Failed i -> Printf.sprintf " (FAILED - %d)" i
    | Pending -> " (Pending)" in
  generic spec_header example_header results_footer example_footer
          message_of_result specs

let progress specs =
  let spec_header _ = () in
  let example_header _ = () in
  let results_footer () = () in
  let example_footer () = Printf.printf "\n" in
  let message_of_result = function
    | Ok -> "."
    | Failed _ -> "F"
    | Pending -> "*" in
  generic spec_header example_header results_footer example_footer
          message_of_result specs
