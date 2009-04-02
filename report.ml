open Spec_types
open Spec

let report_failures spec =
  if Spec.num_failures spec > 0 then begin
    let report failure =
      let expected =
        match Spec.failure_expected_result failure with
        | Some e -> Printf.sprintf " %s" e
        | None -> "" in
      Printf.printf "%d) Expected `%s %s%s' to return %s\n"
        (Spec.failure_id failure) (Spec.failure_operation failure)
        (Spec.failure_actual_result failure) expected
        (if Spec.positive_failure failure then "true" else "false") in
    Spec.iter_failures report spec;
    Printf.printf "\n"
  end

let spec_footer spec =
  Printf.printf "%d examples, %d failures, %d pending\n\n"
                (Spec.num_examples spec) (Spec.num_failures spec)
                (Spec.num_pending ())

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
      Spec.iter_results report_results example;
      results_footer () in
    Spec.iter_examples report_example spec;
    example_footer ();
    report_failures spec;
    spec_footer spec in
  Queue.iter report_spec specs

let nested specs =
  let spec_header spec =
    Printf.printf "%s\n" (Spec.name spec) in
  let example_header example =
    Printf.printf "  %s" (Spec.example_description example) in
  let results_footer () = Printf.printf "\n" in
  let example_footer () = Printf.printf "\n" in
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
  let example_footer () = Printf.printf "\n\n" in
  let message_of_result = function
    | Ok -> "."
    | Failed _ -> "F"
    | Pending -> "*" in
  generic spec_header example_header results_footer example_footer
          message_of_result specs
