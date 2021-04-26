(* vim: set foldmethod=marker: *)

open OUnit2;;
open Formula;;

let fm_to_list fm = Formula.fold_backward (fun en acc -> en::acc) fm []
let cmp_fm a b = (fm_to_list a) = (fm_to_list b)


(** asserts that two formulas are equal *)
let eq_fm expected actual ctxt = assert_equal ~ctxt ~cmp:cmp_fm ~printer:(fun fm -> Format.asprintf "%a" Formula_pp.pp_formula fm) expected actual

(** asserts that two bv terms are equal *)
let eq_bv expected actual ctxt = assert_equal ~ctxt ~printer:(fun bv -> Format.asprintf "%a" Formula.Printing.p_bvterm bv) expected actual
let eq_bl expected actual ctxt = assert_equal ~ctxt ~printer:(fun bl -> Format.asprintf "%a" Formula.Printing.p_blterm bl) expected actual

(** asserts that two sets of variables are equal *)
let eq_varset expected actual ctxt = assert_equal ~ctxt ~printer:(fun set -> Format.asprintf "%a" Formula_pp.pp_varset set) expected actual

let tests = [];;

(** makes a formula from a list of entries *)
let from_list l = List.fold_left (fun fm entry -> push_front entry fm) empty l

(** makes a formula from a smt string
 * NOTE: Does all sorts of simlifications like splitting (assert (and a b)) in
 * (assert a) (assert b) or simplifying constant operations. *)
let from_string s =
  let smt =
    try
      Parse_utils.read_string
      ~parser:Smtlib_parser.script
      ~lexer:Smtlib_lexer.token
      ~string:s
    with Failure(s) as e -> Format.eprintf "Failure: %s" s; raise e
  in
  Smtlib_to_formula.script smt

(* actual tests start here *)

(* {{{ Smart constructors *)

let term =
  let a = mk_ax_var (ax_var "a" 2 2) in
  let b = mk_bv_var (bv_var "b" 2) in
  mk_bv_xor (mk_select 2 a b) (mk_bv_concat
                                 (mk_select 1 a (mk_bv_add_int b 1))
                                 (mk_select 1 a b)
                              );;

let expected = mk_bv_cst (Bitvector.zeros 4);;

let tests = ("xor x x  = 0 and coalescing concat(select x, select x+1)">::(eq_bv expected term))::tests;;

(* }}} *)

(* {{{ Term rewriting *)

(* shortcuts *)
module C = struct
  let a = mk_bv_var (bv_var "a" 32)
  let b = mk_bv_var (bv_var "b" 32)
  let (+) = mk_bv_add
  let (-) = mk_bv_sub
  let c x = mk_bv_cst (Bitvector.of_int ~size:32 x)
  let (<) = mk_bv_slt
  let (>) = mk_bv_sgt
  let (<=) = mk_bv_sle
  let (>=) = mk_bv_sge
  let (=) = mk_bv_equal
  let (<>) = mk_bv_distinct
end

let term = let open! C in a + b - a + (c 4);;

let expected = let open! C in (b + (c 4));;

let tests = ("addition rewrite 1">::(eq_bv expected (Term_transformation.simplify_additions term)))::tests;;

let term = let open! C in a + b - (a + (c 4));;

let expected = let open! C in (b + (c (-4)));;

let tests = ("addition rewrite 2">::(eq_bv expected (Term_transformation.simplify_additions term)))::tests;;

let term = let open! C in a + b - ((c 7) - (a + (c 4)));;

let expected = let open! C in (b + (a + a)) - (c 3);;

let tests = ("addition rewrite 3">::(eq_bv expected (Term_transformation.simplify_additions term)))::tests;;

let term = let open! C in (a + (c 1)) = (b + a);;
let expected = let open! C in (c 1) = b;;

let tests = ("comparison rewrite 1">::(eq_bl expected (Term_transformation.simplify_comparison term)))::tests;;

let term = let open! C in (a - b) <> (c 0);;
let expected = let open! C in a <> b;;

let tests = ("comparison rewrite 2">::(eq_bl expected (Term_transformation.simplify_comparison term)))::tests;;

let term = let open! C in (a - (c 4)) = (c 1);;
let expected = let open! C in a = (c 5);;

let tests = ("comparison rewrite 3">::(eq_bl expected (Term_transformation.simplify_comparison term)))::tests;;

let term = let open! C in (a - (c 4)) >= (c 1);;

let tests = ("comparison rewrite regression test">::(eq_bl term (Term_transformation.simplify_comparison term)))::tests;;
(* }}} *)

(* {{{ Checking bidings *)

let must_raise name v fm =
  name>::(fun _ctx -> assert_raises (Formula_transformation.CheckBindings.UnboundVariable v) (fun () -> Formula_transformation.check_bindings fm |> ignore))

let formula = from_list [
    (mk_assert (mk_bl_var (bl_var "foo")))
  ]

let tests = (must_raise "just assert" (BlVar (bl_var "foo")) formula)::tests

let formula = from_list [
    (mk_assume (mk_bl_var (bl_var "foo")))
  ]
let tests = (must_raise "just assume" (BlVar (bl_var "foo")) formula)::tests

let formula = from_list [
    (mk_define (mk_bl_def (bl_var "foo") [] (mk_bl_var (bl_var "bar"))));
    (mk_assume (mk_bl_var (bl_var "foo")))
  ]

let tests = (must_raise "inside def" (BlVar (bl_var "bar")) formula)::tests

let formula = from_list [
    (mk_assert (mk_bl_let
                  [ (mk_bl_def (bl_var "foo") [] (mk_bl_var (bl_var "bar")))]
                  (mk_bl_var (bl_var "foo"))))
  ]

let tests = (must_raise "inside let" (BlVar (bl_var "bar")) formula)::tests


(* }}} *)

(* {{{ Prune and inline *)
let formula = from_string "(declare-fun useless () Bool)"

let tests = ("prune useless decl">::(eq_fm empty (Formula_transformation.prune_and_inline formula)))::tests;;

let formula = [mk_define(mk_bl_def (bl_var "pa") [] mk_bl_true)
              ;mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_define(mk_bl_def (bl_var "pa2") [] (mk_bl_and (mk_bl_var (bl_var "pa")) (mk_bl_var (bl_var "x"))))
              ;mk_assert(mk_bl_var(bl_var "pa2"))]
            |> from_list
let expected = [mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_assert(mk_bl_var(bl_var "x"))]
            |> from_list
let tests = ("inline true def">::(eq_fm expected (Formula_transformation.prune_and_inline formula)))::tests;;

let formula = [mk_define(mk_bl_def (bl_var "pa") [] mk_bl_true)
              ;mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_define(mk_bl_def (bl_var "pa2") [] (mk_bl_and (mk_bl_var (bl_var "pa")) (mk_bl_var (bl_var "x"))))
              ;mk_define(mk_bl_def (bl_var "pa3") [] (mk_bl_or (mk_bl_var (bl_var "pa2")) (mk_bl_var (bl_var "x"))))
              ;mk_assert(mk_bl_var(bl_var "pa2"))]
            |> from_list
let expected = [mk_declare(mk_bl_decl (bl_var "x") [])
              ;mk_assert(mk_bl_var(bl_var "x"))]
            |> from_list
let tests = ("inline true def and prune irrelevant">::(eq_fm expected (Formula_transformation.prune_and_inline formula)))::tests;;

(* }}} *)

(* {{{ Abstract interpretation *)

let formula = from_string "\
    (declare-fun a () Bool)\
    (declare-fun b () Bool)\
    (assert (not (or a (not (or a b)))))\
    (assert (or a b))\
    (define-fun c () Bool b)\
    "

let expected = from_string "\
    (declare-fun a () Bool)\
    (declare-fun b () Bool)\
    (assert (not (or a (not (or a b)))))\
    (assert b)\
    (define-fun c () Bool true)\
    "
let tests = ("simple boolean AI">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string "\
    (declare-fun a () (_ BitVec 32))\
    (assert (= a (_ bv10 32)))\
    (define-fun c () Bool (bvsle (_ bv0 32) a))\
    "

let expected = from_string "\
    (declare-fun a () (_ BitVec 32))\
    (assert (= a (_ bv10 32)))\
    (define-fun c () Bool true)\
    "
let tests = ("simple constant propagation AI">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvand a (_ bv15 32)) (_ bv0 32)))
    (define-fun c () Bool (= (bvand (bvadd a (_ bv32 32)) (_ bv3 32)) (_ bv0 32)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvand a (_ bv15 32)) (_ bv0 32)))
    (define-fun c () Bool true)
    |}
let tests = ("simple bitvector backward bitwise and propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 16))
    (assert (= (bvor a (_ bv4 16)) (_ bv4 16)))
    (define-fun c () Bool (bvule a (_ bv12 16)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 16))
    (assert (= (bvor a (_ bv4 16)) (_ bv4 16)))
    (define-fun c () Bool true)
    |}
let tests = ("simple bitvector backward bitwise or propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvnot a) (_ bv4 32)))
    (define-fun c () (_ BitVec 32) a)
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvnot a) (_ bv4 32)))
    (define-fun c () (_ BitVec 32) (_ bv4294967291 32))
    |}

let tests = ("simple bitvector backward bitwise not propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvneg a) (_ bv4 32)))
    (define-fun c () (_ BitVec 32) a)
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvneg a) (_ bv4 32)))
    (define-fun c () (_ BitVec 32) (_ bv4294967292 32))
    |}

let tests = ("simple bitvector backward negation propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvnand a (_ bv15 32)) (_ bv4294967295 32)))
    (define-fun c () (_ BitVec 32) (bvand a (_ bv4 32)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (assert (= (bvnand a (_ bv15 32)) (_ bv4294967295 32)))
    (define-fun c () (_ BitVec 32) (_ bv0 32))
    |}

let tests = ("simple bitvector backward nand propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (assert (bvslt a (_ bv0 32)))
    (assert (bvsgt b (_ bv0 32)))
    (declare-fun c () Bool)
    (define-fun d () (_ BitVec 32) (ite c a b))
    (define-fun e () Bool (= d (_ bv0 32)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (assert (bvslt a (_ bv0 32)))
    (assert (bvsgt b (_ bv0 32)))
    (declare-fun c () Bool)
    (define-fun d () (_ BitVec 32) (ite c a b))
    (define-fun e () Bool false)
    |}

let tests = ("simple bitvector forward ite propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula false formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 16))
    (define-fun b () (_ BitVec 32) (concat (_ bv8 16) a))
    (define-fun e () Bool (bvuge b (_ bv524288 32)))
    (define-fun f () Bool (bvult b (_ bv589824 32)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 16))
    (define-fun b () (_ BitVec 32) (concat (_ bv8 16) a))
    (define-fun e () Bool true)
    (define-fun f () Bool true)
    |}

let tests = ("simple bitvector forward concat propagation">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_list [
    mk_declare (mk_ax_decl (ax_var "a" 32 8) []);
    mk_assert (mk_bv_equal
                 (mk_bv_cst (Bitvector.of_int ~size:16 1024))
                 (mk_select 2 (mk_ax_var (ax_var "a" 32 8)) (mk_bv_cst (Bitvector.zeros 32)))
              ) ]

(* should not simplify *)
let tests = ("forward select propagation regression test">::(eq_fm formula (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (define-fun a () Bool true)
    (define-fun b () Bool (let ((a false)) a))
    |}

let expected = from_string {|
    (define-fun a () Bool true)
    (define-fun b () Bool false)
    |}
let tests = ("check forward AI is not confused by let bindings and shadowing">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () Bool)
    (declare-fun c () Bool)
    (assert (let ((a c)) a))
    (define-fun a2 () Bool a)
    |}

let expected = from_string {|
    (declare-fun a () Bool)
    (declare-fun c () Bool)
    (assert (let ((a c)) a))
    (define-fun a2 () Bool a)
    |}

let tests = ("check backward AI is not confused by let bindings and shadowing">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (define-fun zero () (_ BitVec 1) #b0)
    (assert (distinct (bvcomp a (_ bv42 32)) zero))
    (define-fun a2 () (_ BitVec 32) a)
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (define-fun zero () (_ BitVec 1) #b0)
    (assert (= a (_ bv42 32))) ; this is simplified by constant propagation and smart constructors
    (define-fun a2 () (_ BitVec 32) (_ bv42 32))
    |}
let tests = ("check backward bvcmp">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun c () Bool)
    (assert (= (ite c a (bvand a (_ bv1 32))) (_ bv2 32)))
    (define-fun a2 () (_ BitVec 32) a)
    (define-fun c2 () Bool c)
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun c () Bool)
    (assert (= (ite c a (bvand a (_ bv1 32))) (_ bv2 32)))
    (define-fun a2 () (_ BitVec 32) (_ bv2 32))
    (define-fun c2 () Bool true)
    |}

let tests = ("check backward bv ite">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
(declare-fun bs_unknown1_4 () (_ BitVec 32))
(assert
(=
(bvand (bvadd (bvand bs_unknown1_4 (_ bv4294967292 32)) (_ bv32 32))
(_ bv3 32)) (_ bv0 32)))
    |}

let expected = from_string {|
(declare-fun bs_unknown1_4 () (_ BitVec 32))
(assert true)
    |}

let tests = ("from real formula">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

(* 240 = 0xf0 *)
let formula = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvand a (_ bv240 8)))
    (define-fun a3 () (_ BitVec 8) (bvand a2 (_ bv240 8)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvand a (_ bv240 8)))
    (define-fun a3 () (_ BitVec 8) a2)
    |}

let tests = ("idempotent and">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvmul (_ bv48 8) a))
    (define-fun a3 () (_ BitVec 8) (bvand a2 (_ bv240 8)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvmul (_ bv48 8) a))
    (define-fun a3 () (_ BitVec 8) a2)
    |}

let tests = ("idempotent and, non power of two congruence">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvmul (_ bv24 8) a))
    (define-fun a3 () (_ BitVec 8) (bvand a2 (_ bv240 8)))
    |}

let tests = ("idempotent and, wrong congruence">::(eq_fm formula (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvmul (_ bv16 8) a))
    (define-fun a3 () (_ BitVec 8) (bvand a2 (_ bv176 8)))
    |}

let tests = ("idempotent and, wrong constant">::(eq_fm formula (Formula_transformation.AI.simplify_formula true formula)))::tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvor a (_ bv63 8)))
    (define-fun a3 () (_ BitVec 8) (bvor a2 (_ bv60 8)))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 8))
    (define-fun a2 () (_ BitVec 8) (bvor a (_ bv63 8)))
    (define-fun a3 () (_ BitVec 8) a2)
    |}

let tests = ("idempotent or">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))::tests;;
 (* }}} *)

(* {{{ Remove Array Ite *)

let formula = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (ite c mem2 mem))
    |}
let expected = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun
      mem3 () (Array (_ BitVec 32) (_ BitVec 8))
      (store mem (_ bv0 32) (ite c (_ bv42 8) (select mem (_ bv0 32)))))

    |}

let tests = ("simple ax ite left">::(eq_fm expected (Formula_transformation.remove_ax_ite formula)))::tests;;

let formula = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (ite c mem mem2))
    |}
let expected = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun
      mem3 () (Array (_ BitVec 32) (_ BitVec 8))
      (store mem (_ bv0 32) (ite c (select mem (_ bv0 32)) (_ bv42 8))))

    |}

let tests = ("simple ax ite right">::(eq_fm expected (Formula_transformation.remove_ax_ite formula)))::tests;;

let formula = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun pc () (_ BitVec 32) (select
    (ite c mem2 mem)
    (_ bv42 32)
    ))
    |}
let expected = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun pc () (_ BitVec 32)
      (ite c (select mem2 (_ bv42 32)) (select mem (_ bv42 32))))
    |}

let tests = ("ax ite left under select">::(eq_fm expected (Formula_transformation.remove_ax_ite formula)))::tests;;

let formula = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (_ bv0 32) (_ bv43 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (ite c mem3 mem))
    |}
let expected = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (_ bv0 32) (_ bv43 8)))
    (define-fun
      mem4 () (Array (_ BitVec 32) (_ BitVec 8))
      (store (store mem (_ bv0 32) (ite c (_ bv42 8) (select mem (_ bv0 32))))
      (_ bv0 32) (ite c (_ bv43 8) (select mem (_ bv0 32)))))


    |}

let tests = ("simple ax ite twice left">::(eq_fm expected (Formula_transformation.remove_ax_ite formula)))::tests;;
let formula = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem2b () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv1 32) (_ bv43 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (ite c mem2 mem2b))
    |}
let expected = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem2b () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv1 32) (_ bv43 8)))
    (define-fun
      mem3 () (Array (_ BitVec 32) (_ BitVec 8))
      (store
        (store mem (_ bv1 32)
          (ite c (select mem (_ bv1 32)) (_ bv43 8)))
      (_ bv0 32)
      (ite c (_ bv42 8)
        (select (store mem (_ bv1 32) (_ bv43 8)) (_ bv0 32)))))
    |}

let tests = ("simple ax ite both">::(eq_fm expected (Formula_transformation.remove_ax_ite formula)))::tests;;

let formula = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem2b () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv1 32) (_ bv43 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (_ bv4 32) (_ bv46 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (ite c mem3 mem2b))
    |}
let expected = from_string {|
    (declare-fun c () Bool)
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem2b () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv1 32) (_ bv43 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (_ bv4 32) (_ bv46 8)))
    (define-fun
      mem4 () (Array (_ BitVec 32) (_ BitVec 8))
      (store
      (store (store mem (_ bv0 32) (ite c (_ bv42 8) (select mem (_ bv0 32))))
      (_ bv1 32) (ite c (select mem2 (_ bv1 32)) (_ bv43 8))) (_ bv4 32)
      (ite c (_ bv46 8) (select (store mem (_ bv1 32) (_ bv43 8)) (_ bv4 32)))))
    |}

let tests = ("ax ite both with 1 intermediary">::(eq_fm expected (Formula_transformation.remove_ax_ite formula)))::tests;;

(* }}} *)

(* {{{ Function inversion *)


let test_inv ~complete name expected fm =
  let declared fm = Formula.fold_forward (fun en acc ->
      match en.entry_desc with
      | Declare d ->
        let v = match d.decl_desc with
          | BlDecl(v, _) -> BlVar v
          | BvDecl(v, _) -> BvVar v
          | AxDecl(v, _) -> AxVar v
        in
        VarSet.add v acc
      | _ -> acc)
      fm VarSet.empty
  in
  let var_starts_with substring v =
    let name = match v with
      BlVar v -> v.bl_name
      | BvVar v -> v.bv_name
      | AxVar v -> v.ax_name
    in
    String_utils.starts_with ~substring name
  in
  let is_param = var_starts_with "p_" in
  let is_controlled = var_starts_with "c_" in
  let declared_before = declared fm in
  let res, selectors, is_controlled' = Formula_transformation.synthetise_solutions_with_function_inversion ~complete is_param is_controlled fm in
  let declared_after = declared res in
  let uncontrolled selectors is_controlled declared =
    VarSet.filter (fun v -> not (is_controlled v || is_param v)) (VarSet.diff declared selectors)
  in
  [
    (name^": same param variables")>::(eq_varset (VarSet.filter is_param declared_before) (VarSet.filter is_param declared_after));
    (name^": same uncontrolled variables")>::(eq_varset (uncontrolled VarSet.empty is_controlled declared_before) (uncontrolled selectors is_controlled' declared_after));
    (name^": formula")>::(eq_fm expected res)
  ]


let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    |}

let expected = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun _c_sol_synthesis_base () (_ BitVec 32))
    (declare-fun _selector_1 () Bool)
    (define-fun
      c_sol () (_ BitVec 32)
      (ite _selector_1 (bvxor p_target (_ bv42 32)) _c_sol_synthesis_base))
    (assert (ite _selector_1 true (= (bvxor c_sol (_ bv42 32)) p_target)))
    |}

let tests = (test_inv ~complete:true "invert xor" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (declare-fun u_useless () (_ BitVec 32))
    (assert (= (bvadd c_sol (_ bv42 32)) p_target))
    |}

let expected = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun _c_sol_synthesis_base () (_ BitVec 32))
    (declare-fun _selector_2 () Bool)
    (declare-fun u_useless () (_ BitVec 32))
    (define-fun
      c_sol () (_ BitVec 32)
      (ite _selector_2 (bvsub p_target (_ bv42 32)) _c_sol_synthesis_base))
    (assert (ite _selector_2 true (= (bvadd c_sol (_ bv42 32)) p_target)))
    |}

let tests = (test_inv ~complete:true "invert add + useless var" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (declare-fun u_useless () (_ BitVec 32))
    (assert (or (= (bvsub c_sol (_ bv5 32)) u_useless) (and (= (bvadd (bvxor c_sol (_ bv1 32)) (_ bv2 32)) p_target) (= (bvsub c_sol (_ bv42 32)) p_target))))
    |}

let expected = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun _c_sol_synthesis_base () (_ BitVec 32))
    (declare-fun _selector_3 () Bool)
    (declare-fun _selector_4 () Bool)
    (declare-fun u_useless () (_ BitVec 32))
    (define-fun
      c_sol () (_ BitVec 32)
      (ite _selector_3 (bvxor (bvsub p_target (_ bv2 32)) (_ bv1 32))
      (ite _selector_4 (bvadd p_target (_ bv42 32)) _c_sol_synthesis_base)))
    (assert
    (or (= (bvsub c_sol (_ bv5 32)) u_useless)
    (and
    (ite _selector_3 true
    (= (bvadd (bvxor c_sol (_ bv1 32)) (_ bv2 32)) p_target))
    (ite (and (not _selector_3) _selector_4) true
    (= (bvsub c_sol (_ bv42 32)) p_target)))))
    |}

let tests = (test_inv ~complete:true "invert several equalities" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 8))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (= ((_ extract 7 0) c_sol) p_target))
    |}

let expected = from_string {|
(declare-fun p_target () (_ BitVec 8))
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(declare-fun _selector_5 () Bool)
(define-fun
  c_sol () (_ BitVec 32)
  (ite _selector_5 (concat ((_ extract 31 8) _c_sol_synthesis_base) p_target)
  _c_sol_synthesis_base))
(assert (ite _selector_5 true (= ((_ extract 7 0) c_sol) p_target)))
    |}

let tests = (test_inv ~complete:true "invert extract" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 8))
    (assert (= ((_ zero_extend 24) c_sol) p_target))
    |}


let expected = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun _c_sol_synthesis_base () (_ BitVec 8))
    (declare-fun _selector_6 () Bool)
    (define-fun
      c_sol () (_ BitVec 8)
      (ite _selector_6 ((_ extract 7 0) p_target) _c_sol_synthesis_base))
    (assert
    (ite _selector_6 (= ((_ extract 31 8) p_target) (_ bv0 24))
    (= ((_ zero_extend 24) c_sol) p_target)))
    |}

let tests = (test_inv ~complete:true "invert zero extend" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 8))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (= ((_ extract 15 8) c_sol) p_target))
    |}

let expected = from_string {|
    (declare-fun p_target () (_ BitVec 8))
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(declare-fun _selector_7 () Bool)
(define-fun
  c_sol () (_ BitVec 32)
  (ite _selector_7
  (concat ((_ extract 31 16) _c_sol_synthesis_base)
  (concat p_target ((_ extract 7 0) _c_sol_synthesis_base)))
  _c_sol_synthesis_base))
(assert (ite _selector_7 true (= ((_ extract 15 8) c_sol) p_target)))

    |}

let tests = (test_inv ~complete:true "invert extract high" expected formula) @ tests;;


let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    |}

let expected = from_string {|
    (declare-fun p_target () (_ BitVec 32))
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(define-fun c_sol () (_ BitVec 32) (bvxor p_target (_ bv42 32)))
(define-fun _selector_8 () Bool true)
(assert (ite _selector_8 true (= (bvxor c_sol (_ bv42 32)) p_target)))
    |}

let tests = (test_inv ~complete:false "incomplete invert xor" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (or
    (= (bvxor c_sol (_ bv42 32)) p_target)
    (or
    (= (bvxor c_sol (_ bv43 32)) p_target)
    (= (bvxor c_sol (_ bv44 32)) p_target)
    )
    ))
    |}

let expected = from_string {|
(declare-fun p_target () (_ BitVec 32))
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(declare-fun _selector_9 () Bool)
(declare-fun _selector_10 () Bool)
(define-fun
  c_sol () (_ BitVec 32)
  (ite _selector_9 (bvxor p_target (_ bv42 32))
  (ite _selector_10 (bvxor p_target (_ bv43 32))
  (bvxor p_target (_ bv44 32)))))
(define-fun _selector_11 () Bool true)
(assert
(or (ite _selector_9 true (= (bvxor c_sol (_ bv42 32)) p_target))
(or
(ite (and (not _selector_9) _selector_10) true
(= (bvxor c_sol (_ bv43 32)) p_target))
(ite (and (not _selector_9) (and (not _selector_10) _selector_11)) true
(= (bvxor c_sol (_ bv44 32)) p_target)))))
    |}

let tests = (test_inv ~complete:false "incomplete invert 3 xor" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    |}

let expected = from_string {|
(declare-fun p_target () (_ BitVec 32))
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(declare-fun _selector_12 () Bool)
(define-fun c_sol () (_ BitVec 32)
  (ite _selector_12 (bvxor p_target (_ bv42 32)) _c_sol_synthesis_base))
(assert (ite _selector_12 true (= (bvxor c_sol (_ bv42 32)) p_target)))
(assert (ite _selector_12 true (= (bvxor c_sol (_ bv42 32)) p_target)))
    |}

let tests = (test_inv ~complete:true "duplicated invert xor" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun p_target () (_ BitVec 32))
    (declare-fun c_sol () (_ BitVec 32))
    (assert (= (bvxor c_sol (_ bv43 32)) p_target))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    (assert (= (bvxor c_sol (_ bv44 32)) p_target))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    |}

let expected = from_string {|
(declare-fun p_target () (_ BitVec 32))
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(declare-fun _selector_13 () Bool)
(declare-fun _selector_14 () Bool)
(declare-fun _selector_15 () Bool)
(define-fun c_sol () (_ BitVec 32)
(ite _selector_13 (bvxor p_target (_ bv43 32))
(ite _selector_14 (bvxor p_target (_ bv42 32))
(ite _selector_15 (bvxor p_target (_ bv44 32)) _c_sol_synthesis_base))))
(assert (ite _selector_13 true (= (bvxor c_sol (_ bv43 32)) p_target)))
(assert (ite (and (not _selector_13) _selector_14) true
  (= (bvxor c_sol (_ bv42 32)) p_target)))
(assert (ite (and (not _selector_13) (and (not _selector_14) _selector_15)) true
  (= (bvxor c_sol (_ bv44 32)) p_target)))
(assert (ite (and (not _selector_13) _selector_14) true
  (= (bvxor c_sol (_ bv42 32)) p_target)))
    |}

let tests = (test_inv ~complete:true "duplicated invert xor and other" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun c_sol () (_ BitVec 32))
    (declare-fun p_target () (_ BitVec 32))
    (assert (= (bvxor c_sol (_ bv42 32)) p_target))
    |}

let expected = from_string {|
(declare-fun _c_sol_synthesis_base () (_ BitVec 32))
(declare-fun _selector_16 () Bool)
(declare-fun p_target () (_ BitVec 32))
(define-fun c_sol () (_ BitVec 32)
(ite _selector_16 (bvxor p_target (_ bv42 32)) _c_sol_synthesis_base))
(assert (ite _selector_16 true (= (bvxor c_sol (_ bv42 32)) p_target)))
    |}

let tests = (test_inv ~complete:true "invert xor solution declared before unknown" expected formula) @ tests;;
(* }}} *)

(* {{{ Read over write *)
let params = Formula_transformation.{ lst= None; itv= true; rbs= true; unfold=true; };;
let test_row name expected formula = [
  (name^" with vanilla row")>::(eq_fm expected (Formula_transformation.ReadOverWrite.simplify_formula params formula));
  (name^" with AI row">::(eq_fm expected (Formula_transformation.AI.simplify_formula true formula)))
]

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem3 a))
    |}

let tests = (test_row "check row noop (regression test)" formula formula) @ tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (assert (bvule a (_ bv42 32)))
    (assert (bvuge b (_ bv4242 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem3 a))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (assert (bvule a (_ bv42 32)))
    (assert (bvuge b (_ bv4242 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (_ bv42 8))
    |}

let tests = (test_row "check row itv" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (declare-fun c () (_ BitVec 32))
    (assert (bvule a (_ bv42 32)))
    (assert (bvule c (_ bv42 32)))
    (assert (bvuge b (_ bv4242 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem3 c))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (declare-fun c () (_ BitVec 32))
    (assert (bvule a (_ bv42 32)))
    (assert (bvule c (_ bv42 32)))
    (assert (bvuge b (_ bv4242 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem2 c))
    |}

let tests = (test_row "check row select unfolding" expected formula) @ tests;;


let formula = from_string {|
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv15 32) (_ bv15 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (_ bv16 32) (_ bv16 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3 (_ bv17 32) (_ bv17 8)))
    (define-fun mem5 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem4 (_ bv17 32) (_ bv18 8)))
    (define-fun a () (_ BitVec 32) (select mem5 (_ bv16 32)))
    |}

let expected = from_string {|
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv15 32) (_ bv15 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (_ bv16 32) (_ bv16 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3 (_ bv17 32) (_ bv17 8)))
    (define-fun mem5 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem4 (_ bv17 32) (_ bv18 8)))
    (define-fun a () (_ BitVec 32) (_ bv16 8))
    |}

let tests = (test_row "check row constants (regression test)" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    |}
let formula = push_front_define
    (mk_bv_def (bv_var "a2" 16) []
        (mk_select 2
           (mk_ax_var (ax_var "mem2" 32 8))
           (mk_bv_var (bv_var "a" 32))
        )
    )
    formula

let tests = (test_row "regression test: row in case only part of the select can be resolved" formula formula) @ tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (assert (bvuge a (_ bv42 32)))
    (assert (bvuge b (_ bv42 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv1 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 a (_ bv43 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3 (_ bv2 32) (_ bv44 8)))
    (define-fun c () (_ BitVec 8) (select mem4 b))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (assert (bvuge a (_ bv42 32)))
    (assert (bvuge b (_ bv42 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv1 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 a (_ bv43 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3 (_ bv2 32) (_ bv44 8)))
    (define-fun c () (_ BitVec 8) (select mem3 b))
    |}

let tests = (test_row "regression test: the order of stores must be preserved" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun b () (_ BitVec 32))
    (assert (bvuge b (_ bv42 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem3 (_ bv5 32)))
    |}

let expected = from_string {|
    (declare-fun b () (_ BitVec 32))
    (assert (bvuge b (_ bv42 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem (_ bv5 32)))
    |}

let tests = (test_row "check row select unfolding with a layer with empty results" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun b () (_ BitVec 32))
    (declare-fun a () (_ BitVec 32))
    (define-fun b2 () (_ BitVec 32) (bvadd (bvadd b a) (_ bv5 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (bvsub b2 b) (_ bv42 8)))
    (define-fun a2 () (_ BitVec 32) (select mem2 (bvadd a (_ bv5 32))))
    |}

let expected = from_string {|
    (declare-fun b () (_ BitVec 32))
    (declare-fun a () (_ BitVec 32))
    (define-fun b2 () (_ BitVec 32) (bvadd (bvadd b a) (_ bv5 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (bvadd a (_ bv5 32)) (_ bv42 8)))
    (define-fun a2 () (_ BitVec 32) (_ bv42 8))
    |}

let tests = (test_row "check row rebasing with b + a -b + 5 as index" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (declare-fun c () (_ BitVec 32))
    (assert (bvule a (_ bv42 32)))
    (assert (bvule c (_ bv42 32)))
    (assert (bvuge b (_ bv4242 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem2p () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (bvadd a (_ bv4 32)) (_ bv43 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2p b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem3 c))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (declare-fun b () (_ BitVec 32))
    (declare-fun c () (_ BitVec 32))
    (assert (bvule a (_ bv42 32)))
    (assert (bvule c (_ bv42 32)))
    (assert (bvuge b (_ bv4242 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun mem2p () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 (bvadd a (_ bv4 32)) (_ bv43 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2p b (_ bv31 8)))
    (define-fun a2 () (_ BitVec 32) (select mem2p c))
    |}

let tests = (test_row "check row select unfolding with several layers" expected formula) @ tests;;

let formula = from_string {|
    (declare-fun a () (_ BitVec 32))
    (define-fun a2 () (_ BitVec 32) (bvadd a (_ bv4 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun a3 () (_ BitVec 32) (select mem2 (bvsub a2 (_ bv4 32))))
    |}

let expected = from_string {|
    (declare-fun a () (_ BitVec 32))
    (define-fun a2 () (_ BitVec 32) (bvadd a (_ bv4 32)))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem a (_ bv42 8)))
    (define-fun a3 () (_ BitVec 32) (_ bv42 8))
    |}

let tests = (test_row "check row symbolic base rewriting" expected formula) @ tests;;

(* }}} *)


(* {{{ Taint2 *)

(* esp \in 1e3 1e6 *)
let taint ?(inputlow=0) ?(inputhigh=10000) ?(esplow=1000) ?(esphigh=100000) fm =
  let input = bv_var "input" 32 in
  let esp = bv_var "esp" 32 in
  let fm = push_front_assume (mk_bv_uge (mk_bv_var esp) (mk_bv_cst (Bitvector.of_int ~size:32 esplow))) fm in
  let fm = push_front_assume (mk_bv_ule (mk_bv_var esp) (mk_bv_cst (Bitvector.of_int ~size:32 esphigh))) fm in
  let fm = push_front_assume (mk_bv_ule (mk_bv_var input) (mk_bv_cst (Bitvector.of_int ~size:32 inputhigh))) fm in
  let fm = push_front_assume (mk_bv_uge (mk_bv_var input) (mk_bv_cst (Bitvector.of_int ~size:32 inputlow))) fm in
  let is_controlled = function
    | BvVar x -> String_utils.starts_with ~substring:"input" x.bv_name
    | _ -> false
  in
  let optimize = fun fm -> Formula_transformation.optimize ~ai:true fm in
  optimize fm
  |> Formula_transformation.to_universal ~taintrow:true optimize is_controlled (fun _ -> false)

let solve fm =
  let open Solver.Session in
  let session = create Formula_options.Z3 in
  List.iter (fun command -> run session command |> ignore) fm.Smtlib.script_commands;
  let res = check_sat session in
  destroy session;
  res

let expect_status status fm ctxt =
  let printer status = Format.asprintf "Formula is %a:@. %a"
      Formula_pp.pp_status status
      Smtlib_pp.pp fm
  in
  assert_equal ~ctxt ~printer status (solve fm)

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv32 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 esp (_ bv0 8)))
    (define-fun mem3b () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3 (bvadd esp (_ bv1 32)) (_ bv0 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3b (_ bv33 32) (_ bv43 8)))
    (define-fun a2 () (_ BitVec 8) (select mem4 (bvadd esp input)))
    (assert (= a2 (_ bv0 8)))
    |}

let tests = ("esp+input in cte::esp::cte taint2">::(expect_status SAT (taint ~inputhigh:1 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8)))
    (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem (_ bv0 32) (_ bv42 8)))
    (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem2 esp (_ bv0 8)))
    (define-fun mem3b () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3 (bvadd esp (_ bv1 32)) (_ bv0 8)))
    (define-fun mem4 () (Array (_ BitVec 32) (_ BitVec 8))
    (store mem3b (_ bv1 32) (_ bv0 8)))
    (define-fun a2 () (_ BitVec 8) (select mem4 input))
    (assert (distinct a2 (_ bv0 8)))
    |}

let tests = ("input in cte::esp::cte taint2">::(expect_status SAT (taint ~inputhigh:100 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (bvule input esp))
    |}

let tests = ("taint2 of input <= esp">::(expect_status SAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun other () (_ BitVec 32))
    (assert (bvule other esp))
    |}

let tests = ("taint2 of uncontrolled <= esp">::(expect_status UNSAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (distinct input esp))
    |}

let tests = ("taint2 of input != esp">::(expect_status SAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (= input esp))
    |}

let tests = ("taint2 of input == esp">::(expect_status UNSAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun other () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun c () Bool)
    (define-fun foo () (_ BitVec 32) (ite c (bvmul esp (_ bv2 32)) (bvmul other (_ bv4 32))))
    (assert (distinct (_ bv0 32) input))
    (assert (= input2 (bvand input foo)))
    |}

let tests = ("taint2 of input & esp when esp%2 = 0">::(expect_status SAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (distinct (_ bv0 32) input))
    (assert (= input2 (bvand input esp)))
    |}

let tests = ("taint2 of input & esp when esp and input != 0">::(expect_status UNSAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (= input2 (bvand input esp)))
    |}

let tests = ("taint2 of input & esp when esp">::(expect_status SAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (= input2 (bvand input (_ bv120 32))))
    (assert (distinct input  (_ bv0 32)))
    (assert (distinct input2 (_ bv0 32)))
    |}

let tests = ("taint2 of input & cst">::(expect_status SAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (= input2 (bvor input esp)))
    |}

let tests = ("taint2 of input | esp when esp">::(expect_status SAT (taint ~inputhigh:0xffffffff  formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (= input2 (bvor (bvmul input (_ bv6 32)) (bvmul esp (_ bv4 32)))))
    |}

let tests = ("taint2 of 6input | 4esp when esp">::(expect_status SAT (taint ~inputhigh:0xffffffff  formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (= input2 (bvor (bvmul input (_ bv2 32)) esp)))
    |}

let tests = ("taint2 of 2input | esp when esp">::(expect_status UNSAT (taint ~inputhigh:0xffffffff  formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (bvule esp (bvadd esp input)))
    |}

let tests = ("taint2 of esp <= esp + input with small input">::(expect_status SAT (taint ~inputhigh:0xff  formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (bvule esp (bvadd esp input)))
    |}

let tests = ("taint2 of esp <= esp + input with overflow">::(expect_status UNSAT (taint ~inputhigh:0xffffffff ~esphigh:0xffffffff formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (bvule (bvadd esp (_ bv1 32)) (bvadd esp input)))
    |}

let tests = ("taint2 of esp + 1 <= esp + input without overflow">::(expect_status SAT (taint ~inputlow:0 ~inputhigh:0xff ~esplow:0xff000000 ~esphigh:0xffff0000 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (not (or (= input2 input) (bvugt (bvadd esp (_ bv1 32)) (bvadd esp input)))))
    |}

let tests = ("taint2 of esp + 1 <= esp + input without overflow with negation">::(expect_status SAT (taint ~inputlow:0 ~inputhigh:0xff ~esplow:0xff000000 ~esphigh:0xffff0000 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun buf () (_ BitVec 32))
    (assert (bvule (bvadd esp buf) (bvadd esp input)))
    |}

let tests = ("taint2 of esp + buf <= esp + input without overflow but simplified comparison is uncontrolled">::(expect_status UNSAT (taint ~inputlow:0 ~inputhigh:0xff ~esplow:0xff000000 ~esphigh:0xffff0000 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun uncontrolled2 () (_ BitVec 4))
    (define-fun uncontrolled3 () (_ BitVec 32) (concat (_ bv0 28) uncontrolled2))
    (assert (bvule (bvadd (bvadd uncontrolled3 esp) (_ bv4 32)) (bvadd (bvadd uncontrolled3 esp) input)))
    |}

let tests = ("taint2 of esp + buf + 1 <= esp + buf + input without overflow (to check that simplification is recursive)">::(expect_status SAT (taint ~inputlow:0 ~inputhigh:0xff ~esplow:0xff000000 ~esphigh:0xffff0000 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun input2 () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (not (or (= input2 input) (= esp input))))
    |}

let tests = ("taint2 of (= esp input) with negation">::(expect_status SAT (taint ~inputlow:0 ~inputhigh:0xfff00000 ~esplow:0xff000000 ~esphigh:0xffff0000 formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (declare-fun esp2 () (_ BitVec 32))
    (assert (bvule esp esp2))
    |}

let tests = ("taint2 of uncontrolled inequality">::(expect_status UNSAT (taint formula)))::tests;;

let formula = from_string {|
    (declare-fun input () (_ BitVec 32))
    (declare-fun esp () (_ BitVec 32))
    (assert (bvule esp input))
    |}

let tests = ("taint2 of inequality with insufficient AI">::(expect_status UNSAT (taint ~inputlow:10  ~inputhigh:20 ~esplow:0 ~esphigh:30 formula)))::tests;;

(* let formula = from_string {| *)
(*     (declare-fun input () (_ BitVec 8)) *)
(*     (declare-fun mem () (Array (_ BitVec 32) (_ BitVec 8))) *)
(*     (define-fun mem2 () (Array (_ BitVec 32) (_ BitVec 8)) *)
(*     (store mem (_ bv0 32) input)) *)
(*     (declare-fun esp () (_ BitVec 32)) *)
(*     (define-fun mem3 () (Array (_ BitVec 32) (_ BitVec 8)) *)
(*     (store mem2 esp (_ bv2 8))) *)
(*     (assert (= (select mem3 (_ bv0 32)) (_ bv0 8))) *)
(*     |} *)

(* FIXME this test does not pass because of check_bindings *)
(* let tests = ("taint2 of uninitialized memory">::(expect_status UNSAT (taint ~inputlow:0 ~inputhigh:0xffffffff  ~esplow:0 ~esphigh:0xffffffff formula)))::tests;; *)
(* }}} *)

(* run the tests *)
let () = run_test_tt_main ("formula simplifications">:::tests)


