open Spec

let report_failures failures =
  if not (Queue.is_empty failures) then begin
    Printf.printf "\n";
    let report failure =
      let expected =
        match Spec.failure_expected_result failure with
        | Some e -> Printf.sprintf " %s" e
        | None -> "" in
      Printf.printf "%d) Expected `%s %s%s' to return %s\n"
        (Spec.failure_id failure) (Spec.failure_operation failure)
        (Spec.failure_actual_result failure) expected
        (if Spec.positive_failure failure then "true" else "false") in
    Queue.iter report failures
  end;
  Printf.printf "\n"

let spec_summary root =
  let failures = Spec.failures root in
  Printf.printf "%d examples, %d failures, %d pending\n\n"
                (Spec.num_examples root) (Queue.length failures)
                (Spec.num_pending root)

let generic spec_header example_header results_footer spec_footer
            message_of_result specs =
  let rec report_spec ?(level=0) spec =
    spec_header spec level;
    let report_example example =
      example_header example level;
      Printf.printf "%s" (message_of_result (Spec.example_result example));
      results_footer () in
    Spec.iter_examples report_example spec;
    Spec.iter_subspecs (report_spec ~level:(level+2)) spec in
  let report_spec_and_failures root =
    let spec = Spec.spec root in
    report_spec spec;
    spec_footer ();
    report_failures (Spec.failures root);
    spec_summary root in
  Queue.iter report_spec_and_failures specs

let nested specs =
  let spec_header spec level =
    let pad = String.make level ' ' in
    Printf.printf "%s%s\n" pad (Spec.name spec) in
  let example_header example level =
    let pad = String.make (level + 2) ' ' in
    Printf.printf "%s%s" pad (Spec.example_description example) in
  let results_footer () = Printf.printf "\n" in
  let spec_footer () = () in
  let message_of_result = function
    | Ok -> ""
    | Failed (i, j) ->
        if i = j then Printf.sprintf " (FAILED: %d)" i
                 else Printf.sprintf " (FAILED: %d-%d)" i j
    | Pending -> " (Pending)" in
  generic spec_header example_header results_footer spec_footer
          message_of_result specs

let progress specs =
  let spec_header _ _ = () in
  let example_header _ _ = () in
  let results_footer () = () in
  let spec_footer () = Printf.printf "\n" in
  let message_of_result = function
    | Ok -> "."
    | Failed _ -> "F"
    | Pending -> "*" in
  generic spec_header example_header results_footer spec_footer
          message_of_result specs
