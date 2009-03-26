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

type result = Ok | Failed | Error | Pending

type example =
  { mutable description : string
  ; results             : result Queue.t
  }

type spec =
  { mutable name : string
  ; examples     : example Queue.t
  }

(* XXX can we get rid of the globals? *)
let specs : spec Queue.t = Queue.create ()
let examples : example Queue.t = Queue.create ()
let results : result Queue.t = Queue.create ()

let new_spec name =
  { name = name; examples = Queue.create () }

let new_example description =
  { description = description; results = Queue.create () }

let cleanup () =
  Queue.clear specs;
  Queue.clear examples;
  Queue.clear results

(*
 * Expectations.
 *)

let infixop_expectation _loc op result expected =
  <:expr<
    Queue.push
      (if $op$ $result$ $expected$
        then Ok
        else Failed)
      results
  >>

let ident_expectation _loc op result expected =
  <:expr<
    Queue.push
      (if $id:op$ $result$ $expected$ then Ok else Failed)
      results
  >>

let one_arg_ident_expectation _loc op result =
  <:expr<
    Queue.push
      (if $id:op$ $result$ then Ok else Failed)
      results
  >>

let fun_expectation _loc args op result expected =
  <:expr<
    Queue.push
      (if (fun $args$ -> $op$) $result$ $expected$
        then Ok
        else Failed)
      results
  >>

(*
 * Unexpectations.
 *)

let infixop_unexpectation _loc op result expected =
  <:expr<
    Queue.push
      (if $op$ $result$ $expected$
        then Failed
        else Ok)
      results
  >>

let ident_unexpectation _loc op result expected =
  <:expr<
    Queue.push
      (if $id:op$ $result$ $expected$ then Failed else Ok)
      results
  >>

let one_arg_ident_unexpectation _loc op result =
  <:expr<
    Queue.push
      (if $id:op$ $result$ then Failed else Ok)
      results
  >>

let fun_unexpectation _loc args op result expected =
  <:expr<
    Queue.push
      (if (fun $args$ -> $op$) $result$ $expected$
        then Failed
        else Ok)
      results
  >>

(*
 * "it" blocks.
 *)

let example_group _loc descr seq =
  <:expr<
    do {
      do { $list:seq$ };
      let example = new_example $str:descr$ in
      Queue.transfer results example.results;
      Queue.push example examples
    }
  >>

let pending_example_group _loc descr =
  <:expr<
    do {
      let example = new_example $str:descr$ in
      Queue.push Pending example.results;
      Queue.push example examples
    }
  >>

(*
 * "define" blocks.
 *)

let run_spec _loc name seq =
  <:expr<
    do {
      do { $list:seq$ };
      let spec = new_spec $str:name$ in
      Queue.transfer examples spec.examples;
      Queue.push spec specs
    }
  >>

(*
 * Reporting functions.
 *)

let message_of_result = function
  | Ok -> ""
  | Failed -> "(FAILED)"
  | Error -> "(ERROR)"
  | Pending -> "(Pending)"

let report () =
  let report_spec spec =
    Printf.printf "%s\n" spec.name;
    let report_example example =
      Printf.printf "  %s" example.description;
      let report_results res =
        Printf.printf " %s" (message_of_result res);
        try
          match res with
          | Ok -> ()
          | _ -> raise Exit
        with Exit -> () in
      Queue.iter report_results example.results;
      Printf.printf "\n" in
    Queue.iter report_example spec.examples in
  Queue.iter report_spec specs;
  cleanup ()

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
