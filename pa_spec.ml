(*
 * Copyright (c) 2009 Andre Nathan <andre@sneakymustard.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

open Camlp4.PreCast
open Syntax

type result = Ok | Failed of int | Pending

type example =
  { mutable description : string
  ; results             : result Queue.t
  }

type spec =
  { mutable name : string
  ; examples     : example Queue.t
  }

type expectation_kind = Positive | Negative (* should or should not *)

type failure =
  { id        : int
  ; operation : string
  ; result    : string
  ; expected  : string option
  ; kind      : expectation_kind
  }

(*
 * The string_of_* functions are taken from
 *   http://caml.inria.fr/pub/ml-archives/caml-list/2008/08/a6c9c42fbb20ce51984d26cc54b61c30.en.html
 *)

let printer =
  let module P = Camlp4.Printers.OCaml.Make(Syntax) in
  new P.printer ()

let format_to_string (f : Format.formatter -> 'a -> unit) (v : 'a) : string =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  f fmt v;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let string_of_expr : Ast.expr -> string = format_to_string printer#expr
let string_of_ident : Ast.ident -> string = format_to_string printer#ident
let string_of_patt : Ast.patt -> string = format_to_string printer#patt

(* XXX can we get rid of the globals? *)
let specs : spec Queue.t = Queue.create ()
let examples : example Queue.t = Queue.create ()
let results : result Queue.t = Queue.create ()
let failures : failure Queue.t = Queue.create ()
let failure_id = ref 1
let before_each = ref (fun () -> ())
let after_each = ref (fun () -> ())
let after_all = ref (fun () -> ())
let total_pending = ref 0

let new_spec name =
  { name = name; examples = Queue.create () }

let new_example description =
  { description = description; results = Queue.create () }

let incr_failure_id () =
  incr failure_id

let curr_failure_id () =
  !failure_id

let set_before_each hook =
  before_each := hook

let run_before_each_hook () =
  !before_each ()

let set_after_each hook =
  after_each := hook

let run_after_each_hook () =
  !after_each ()

let set_after_all hook =
  after_all := hook

let run_after_all_hook () =
  !after_all ()

let incr_total_pending () =
  incr total_pending

let cleanup () =
  Queue.clear specs;
  Queue.clear examples;
  Queue.clear results;
  Queue.clear failures;
  failure_id := 1;
  total_pending := 0

(*
 * Expectations.
 *)

let add_success () =
  Queue.push Ok results

let add_failure operation result expected kind =
  let id = curr_failure_id () in
  let failure =
    { id = id
    ; operation = operation
    ; result    = result
    ; expected  = expected
    ; kind      = kind
    } in
  Queue.push (Failed id) results;
  Queue.push failure failures;
  incr_failure_id ()

let infixop_expectation _loc op result expected =
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $op$ $result$ $expected$ then
      add_success ()
    else
      add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$) Positive
  >>

let ident_expectation _loc op result expected =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $id:op$ $result$ $expected$ then
      add_success ()
    else
      add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$) Positive
  >>

let one_arg_ident_expectation _loc op result =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  <:expr<
    if $id:op$ $result$ then
      add_success ()
    else
      add_failure $str:op_str$ $str:res_str$ None Positive
  >>

let fun_expectation _loc args op result expected =
  let args_str = string_of_patt args in
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  let fun_str = "(fun " ^ args_str ^ " -> " ^ op_str ^ ")" in
  <:expr<
    if (fun $args$ -> $op$) $result$ $expected$ then
      add_success ()
    else
      add_failure $str:fun_str$ $str:res_str$ (Some $str:exp_str$) Positive
  >>

(*
 * Unexpectations.
 *)

let infixop_unexpectation _loc op result expected =
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $op$ $result$ $expected$ then
      add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$) Negative
    else
      add_success ()
  >>

let ident_unexpectation _loc op result expected =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $id:op$ $result$ $expected$ then
      add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$) Negative
    else
      add_success ()
  >>

let one_arg_ident_unexpectation _loc op result =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  <:expr<
    if $id:op$ $result$ then
      add_failure $str:op_str$ $str:res_str$ None Negative
    else
      add_success ()
  >>

let fun_unexpectation _loc args op result expected =
  let args_str = string_of_patt args in
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  let fun_str = "(fun " ^ args_str ^ " -> " ^ op_str ^ ")" in
  <:expr<
    if (fun $args$ -> $op$) $result$ $expected$ then
      add_failure $str:fun_str$ $str:res_str$ (Some $str:exp_str$) Negative
    else
      add_success ()
  >>

(*
 * "it" blocks.
 *)

let example_group _loc descr seq =
  <:expr<
    do {
      run_before_each_hook ();
      do { $list:seq$ };
      let example = new_example $str:descr$;
      Queue.transfer results example.results;
      Queue.push example examples;
      run_after_each_hook ()
    }
  >>

let pending_example_group _loc descr =
  <:expr<
    do {
      let example = new_example $str:descr$;
      Queue.push Pending example.results;
      Queue.push example examples;
      incr_total_pending ()
    }
  >>

(*
 * "before/after each" blocks.
 *)

let set_before_each_hook _loc seq =
  <:expr< set_before_each (fun () -> do { $list:seq$ }) >>

let set_after_each_hook _loc seq =
  <:expr< set_after_each (fun () -> do { $list:seq$ }) >>

let set_after_all_hook _loc seq =
  <:expr< set_after_all (fun () -> do { $list:seq$ }) >>

(*
 * "describe" blocks.
 *)

let run_spec _loc name seq =
  <:expr<
    do {
      do { $list:seq$ };
      let spec = new_spec $str:name$;
      Queue.transfer examples spec.examples;
      Queue.push spec specs;
      run_after_all_hook ();
      set_before_each (fun () -> ());
      set_after_each (fun () -> ());
      set_after_all (fun () -> ())
    }
  >>

(*
 * Reporting functions.
 *)

module type Reports = sig
  val nested : unit -> unit
  val progress : unit -> unit
end

module Report : Reports = struct
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
      message_of_result =
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

  let nested () =
    let spec_header spec = Printf.printf "%s\n" spec.name in
    let example_header example = Printf.printf "  %s" example.description in
    let results_footer () = Printf.printf "\n" in
    let example_footer () = () in
    let message_of_result = function
      | Ok -> ""
      | Failed i -> Printf.sprintf " (FAILED - %d)" i
      | Pending -> " (Pending)" in
    generic spec_header example_header results_footer example_footer
            message_of_result

  let progress () =
    let spec_header _ = () in
    let example_header _ = () in
    let results_footer () = () in
    let example_footer () = Printf.printf "\n" in
    let message_of_result = function
      | Ok -> "."
      | Failed _ -> "F"
      | Pending -> "*" in
    generic spec_header example_header results_footer example_footer
            message_of_result
end

(*
 * Helpers.
 *)

let match_regexp str re =
  Str.string_match (Str.regexp re) str 0

let raise_exception f e =
  try
    ignore (f ());
    false
  with e' ->
    Printexc.to_string e = Printexc.to_string e'

let raise_an_exception f =
  try
    ignore (f ());
    false
  with _ -> true

EXTEND Gram
  expr: LEVEL "simple" [
    [ "describe"; descr = STRING; "do"; seq = LIST0 expr; "done" ->
      run_spec _loc descr seq

    | "before"; "all"; "do"; seq = LIST1 expr; "done" ->
        <:expr< do { $list:seq$ } >>
    | "before"; "each"; "do"; seq = LIST1 expr; "done" ->
        set_before_each_hook _loc seq

    | "after"; "all"; "do"; seq = LIST1 expr; "done" ->
        set_after_all_hook _loc seq
    | "after"; "each"; "do"; seq = LIST1 expr; "done" ->
        set_after_each_hook _loc seq

    | "it"; descr = STRING; "do"; seq = LIST1 expr; "done" ->
        example_group _loc descr seq
    | "it"; descr = STRING ->
        pending_example_group _loc descr

    | res = SELF; "should"; OPT "be"; op = infixop0; exp = SELF ->
        infixop_expectation _loc op res exp
    | res = SELF; "should"; OPT "be"; op = ident; exp = SELF ->
        ident_expectation _loc op res exp
    | res = SELF; "should"; OPT "be"; op = ident ->
        one_arg_ident_expectation _loc op res
    | res = SELF; "should"; OPT "be"; "("; "fun"; args = ipatt;
      op = fun_def; ")"; exp = SELF ->
        fun_expectation _loc args op res exp

    | res = SELF; "should"; "not"; OPT "be"; op = infixop0; exp = SELF ->
        infixop_unexpectation _loc op res exp
    | res = SELF; "should"; "not"; OPT "be"; op = ident; exp = SELF ->
        ident_unexpectation _loc op res exp
    | res = SELF; "should"; "not"; OPT "be"; op = ident ->
        one_arg_ident_unexpectation _loc op res
    | res = SELF; "should"; "not"; OPT "be"; "("; "fun"; args = ipatt;
      op = fun_def; ")"; exp = SELF ->
        fun_unexpectation _loc args op res exp
    ]
  ];
END
