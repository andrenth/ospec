open Camlp4.PreCast
open Syntax

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

(*
 * Expectations.
 *)

let infixop_expectation _loc op result expected =
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $op$ $result$ $expected$ then
      Spec.add_success ()
    else
      Spec.add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$)
                       Spec.Positive
  >>

let ident_expectation _loc op result expected =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $id:op$ $result$ $expected$ then
      Spec.add_success ()
    else
      Spec.add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$)
                       Spec.Positive
  >>

let one_arg_ident_expectation _loc op result =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  <:expr<
    if $id:op$ $result$ then
      Spec.add_success ()
    else
      Spec.add_failure $str:op_str$ $str:res_str$ None Spec.Positive
  >>

let fun_expectation _loc args op result expected =
  let args_str = string_of_patt args in
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  let fun_str = "(fun " ^ args_str ^ " -> " ^ op_str ^ ")" in
  <:expr<
    if (fun $args$ -> $op$) $result$ $expected$ then
      Spec.add_success ()
    else
      Spec.add_failure $str:fun_str$ $str:res_str$ (Some $str:exp_str$)
                       Spec.Positive
  >>

let match_expectation _loc result pattern =
  let res_str = string_of_expr result in
  let patt_str = string_of_patt pattern in
  <:expr<
    match $result$ with
    [ $pattern$ -> Spec.add_success ()
    | _ -> Spec.add_failure "match" $str:res_str$ (Some $str:patt_str$)
                            Spec.Positive ]
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
      Spec.add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$)
                       Spec.Negative
    else
      Spec.add_success ()
  >>

let ident_unexpectation _loc op result expected =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  <:expr<
    if $id:op$ $result$ $expected$ then
      Spec.add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$)
                       Spec.Negative
    else
      Spec.add_success ()
  >>

let one_arg_ident_unexpectation _loc op result =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  <:expr<
    if $id:op$ $result$ then
      Spec.add_failure $str:op_str$ $str:res_str$ None Spec.Negative
    else
      Spec.add_success ()
  >>

let fun_unexpectation _loc args op result expected =
  let args_str = string_of_patt args in
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  let fun_str = "(fun " ^ args_str ^ " -> " ^ op_str ^ ")" in
  <:expr<
    if (fun $args$ -> $op$) $result$ $expected$ then
      Spec.add_failure $str:fun_str$ $str:res_str$ (Some $str:exp_str$)
                       Spec.Negative
    else
      Spec.add_success ()
  >>

let match_unexpectation _loc result pattern =
  let res_str = string_of_expr result in
  let patt_str = string_of_patt pattern in
  <:expr<
    match $result$ with
    [ $pattern$ -> Spec.add_failure "match" $str:res_str$ (Some $str:patt_str$)
                                    Spec.Negative
    | _ -> Spec.add_success () ]
  >>

(*
 * "it" blocks.
 *)

let example_group _loc descr seq =
  <:expr<
    do {
      Spec.run_before_each_hook ();
      do { $list:seq$ };
      let example = Spec.new_example $str:descr$;
      Spec.transfer_results example;
      Spec.add_example example;
      Spec.run_after_each_hook ()
    }
  >>

let pending_example_group _loc descr =
  <:expr<
    do {
      let example = Spec.new_example $str:descr$;
      Spec.add_result Spec.Pending example;
      Spec.add_example example;
      Spec.incr_total_pending ()
    }
  >>

(*
 * "before/after each" blocks.
 *)

let set_before_each_hook _loc seq =
  <:expr< Spec.set_before_each (fun () -> do { $list:seq$ }) >>

let set_after_each_hook _loc seq =
  <:expr< Spec.set_after_each (fun () -> do { $list:seq$ }) >>

let set_after_all_hook _loc seq =
  <:expr< Spec.set_after_all (fun () -> do { $list:seq$ }) >>

(*
 * "describe" blocks.
 *)

let run_spec _loc name seq =
  <:expr<
    do {
      do { $list:seq$ };
      let spec = Spec.new_spec $str:name$;
      Spec.transfer_examples spec;
      Spec.transfer_failures spec;
      Spec.add_spec spec;
      Spec.run_after_all_hook ();
      Spec.set_before_each (fun () -> ());
      Spec.set_after_each (fun () -> ());
      Spec.set_after_all (fun () -> ())
    }
  >>

(* From Camlp4OCamlRevisedParser.ml *)
let mksequence _loc = function
  | <:expr< $_$; $_$ >> as e -> <:expr< do { $e$ } >>
  | e -> e

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

    | "match"; e = sequence; "with"; a = match_case ->
        <:expr< match $mksequence _loc e$ with [ $a$ ] >>
    | res = SELF; "should"; "match"; patt = ipatt ->
        match_expectation _loc res patt
    | res = SELF; "should"; "not"; "match"; patt = ipatt ->
        match_unexpectation _loc res patt
    ]
  ];
END
