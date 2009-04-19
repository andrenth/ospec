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

let positive =
  let _loc = Loc.ghost in
  <:expr< Spec.Positive >>

let negative =
  let _loc = Loc.ghost in
  <:expr< Spec.Negative >>

let kind_test_modifier kind =
  let _loc = Loc.ghost in
  match kind with
  | <:expr< Spec.Positive >> -> <:expr< fun x -> not x >>
  | <:expr< Spec.Negative >> -> <:expr< fun x -> x >>
  | _ -> failwith "kind_test_modifier: invalid expectation kind"

let infixop_expectation _loc op result expected kind =
  let op_str = string_of_expr op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  let f = kind_test_modifier kind in
  <:expr<
    try
      if $f$ ($op$ $result$ $expected$) then
        Spec.add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$) $kind$
      else ()
    with e -> Spec.add_error (Printexc.to_string e)
  >>

let ident_expectation _loc op result expected kind =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  let exp_str = string_of_expr expected in
  let f = kind_test_modifier kind in
  <:expr<
    try
      if $f$ ($id:op$ $result$ $expected$) then
        Spec.add_failure $str:op_str$ $str:res_str$ (Some $str:exp_str$) $kind$
      else ()
    with e -> Spec.add_error (Printexc.to_string e)
  >>

let one_arg_ident_expectation _loc op result kind =
  let op_str = string_of_ident op in
  let res_str = string_of_expr result in
  let f = kind_test_modifier kind in
  <:expr<
    try
      if $f$ ($id:op$ $result$) then
        Spec.add_failure $str:op_str$ $str:res_str$ None $kind$
      else ()
    with e -> Spec.add_error (Printexc.to_string e)
  >>

let rec mkfun _loc patts e =
  match patts with
  | p::ps -> <:expr< fun $p$ -> $mkfun _loc ps e$ >>
  | [] -> e

let fun_expectation _loc args body result expected kind =
  let op = mkfun _loc args body in
  let res_str = string_of_expr result in
  let fun_str = "(" ^ string_of_expr op ^ ")" in
  let f = kind_test_modifier kind in
  let value, exp_str_expr =
    match expected with
    | Some e ->
        let exp_str = string_of_expr e in
        <:expr< $op$ $result$ $e$ >>, <:expr< Some $str:exp_str$ >>
    | None ->
        <:expr< $op$ $result$ >>, <:expr< None >> in
  <:expr<
    try
      if $f$ $value$ then
        Spec.add_failure $str:fun_str$ $str:res_str$ $exp_str_expr$ $kind$
      else ()
    with e -> Spec.add_error (Printexc.to_string e)
  >>

let match_expectation _loc result pattern =
  let res_str = string_of_expr result in
  let patt_str = string_of_patt pattern in
  <:expr<
    try
      match $result$ with
      [ $pattern$ -> ()
      | _ -> Spec.add_failure "match" $str:res_str$ (Some $str:patt_str$)
                              Spec.Positive ]
    with e -> Spec.add_error (Printexc.to_string e)
  >>

let match_unexpectation _loc result pattern =
  let res_str = string_of_expr result in
  let patt_str = string_of_patt pattern in
  <:expr<
    try
      match $result$ with
      [ $pattern$ -> Spec.add_failure "match" $str:res_str$
                                      (Some $str:patt_str$) Spec.Negative
      | _ -> () ]
    with e -> Spec.add_error (Printexc.to_string e)
  >>

(*
 * "it" blocks.
 *)

let example_group _loc descr seq =
  <:expr<
    do {
      Spec.run_before_each_hooks ();
      do { $list:seq$ };
      let example = Spec.new_example $str:descr$;
      Spec.add_example example;
      Spec.run_after_each_hooks ()
    }
  >>

let pending_example_group _loc descr =
  <:expr<
    Spec.add_example (Spec.new_pending_example $str:descr$)
  >>

(*
 * "before/after each" blocks.
 *)

let add_before_each_hook _loc seq =
  <:expr< Spec.add_before_each (fun () -> do { $list:seq$ }) >>

let add_after_each_hook _loc seq =
  <:expr< Spec.add_after_each (fun () -> do { $list:seq$ }) >>

let add_after_all_hook _loc seq =
  <:expr< Spec.add_after_all (fun () -> do { $list:seq$ }) >>

(*
 * "describe" blocks.
 *)

let run_spec _loc name seq =
  <:expr<
    do {
      let spec = Spec.new_spec $str:name$;
      let parent = Spec.start spec;
      do { $list:seq$ };
      Spec.run_after_all_hooks ();
      Spec.remove_spec spec parent
    }
  >>

(*
 * Property testing.
 *)

let expr_from_option loc opt =
  match opt with
  | Some e -> e
  | None -> Loc.raise loc (Stream.Error "expected expression after \"->\"")

let property _loc gen instances var impl e1 e2opt =
  let n =
    match instances with
    | Some x -> <:expr< Some $int:x$ >>
    | None -> <:expr< None >> in
  match impl with
  | Some _ ->
      let e2 = expr_from_option _loc e2opt in
      let constr = <:expr< fun $var$ -> $e1$ >> in
      let prop = <:expr< fun $var$ -> $e2$ >> in
      <:expr< Prop.for_all $gen$ $n$ (Some $constr$) $prop$ >>
  | None ->
      let prop = <:expr< fun $var$ -> $e1$ >> in
      <:expr< Prop.for_all $gen$ $n$ None $prop$ >>

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
        add_before_each_hook _loc seq

    | "after"; "all"; "do"; seq = LIST1 expr; "done" ->
        add_after_all_hook _loc seq
    | "after"; "each"; "do"; seq = LIST1 expr; "done" ->
        add_after_each_hook _loc seq

    | "it"; descr = STRING; "do"; seq = LIST1 expr; "done" ->
        example_group _loc descr seq
    | "it"; descr = STRING ->
        pending_example_group _loc descr

    | res = SELF; "should"; OPT "be"; op = infixop0; exp = SELF ->
        infixop_expectation _loc op res exp positive
    | res = SELF; "should"; OPT "be"; op = ident; exp = SELF ->
        ident_expectation _loc op res exp positive
    | res = SELF; "should"; OPT "be"; op = ident ->
        one_arg_ident_expectation _loc op res positive
    | res = SELF; "should"; OPT "be"; "("; "fun"; args = LIST1 ipatt;
      "->"; op = expr; ")"; exp = OPT expr LEVEL "top" ->
        fun_expectation _loc args op res exp positive

    | res = SELF; "should"; "not"; OPT "be"; op = infixop0; exp = SELF ->
        infixop_expectation _loc op res exp negative
    | res = SELF; "should"; "not"; OPT "be"; op = ident; exp = SELF ->
        ident_expectation _loc op res exp negative
    | res = SELF; "should"; "not"; OPT "be"; op = ident ->
        one_arg_ident_expectation _loc op res negative
    | res = SELF; "should"; "not"; OPT "be"; "("; "fun"; args = LIST1 ipatt;
      "->"; op = expr; ")"; exp = OPT expr LEVEL "top" ->
        fun_expectation _loc args op res exp negative

    | "match"; e = sequence; "with"; a = match_case ->
        <:expr< match $mksequence _loc e$ with [ $a$ ] >>
    | res = SELF; "should"; "match"; patt = ipatt ->
        match_expectation _loc res patt
    | res = SELF; "should"; "not"; "match"; patt = ipatt ->
        match_unexpectation _loc res patt

    | "forall"; n = OPT a_INT; gen = ident; var = ipatt; ".";
      e1 = expr LEVEL "top"; impl = OPT "->"; e2 = OPT expr LEVEL "top" ->
        property _loc <:expr< $id:gen$ >> n var impl e1 e2
    | "forall"; n = OPT a_INT; "("; gen = expr; ")"; var = ipatt; ".";
      e1 = expr LEVEL "top"; impl = OPT "->"; e2 = OPT expr LEVEL "top" ->
        property _loc gen n var impl e1 e2
    ]
  ];
END
