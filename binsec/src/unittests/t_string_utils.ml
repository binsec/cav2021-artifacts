open OUnit2;;
open String_utils;;

let eq_bool expected actual ctxt = assert_equal ~ctxt ~printer:string_of_bool expected actual
let tests = [];;

(* actual tests start here *)

let tests = ("starts_with empty">::(eq_bool true (starts_with ~substring:"" "foo")))::tests;;
let tests = ("starts_with longer">::(eq_bool false (starts_with ~substring:"loooong" "foo")))::tests;;
let tests = ("starts_with distinct">::(eq_bool false (starts_with ~substring:"foz" "foo")))::tests;;
let tests = ("starts_with self">::(eq_bool true (starts_with ~substring:"foo" "foo")))::tests;;
let tests = ("starts_with prefix">::(eq_bool true (starts_with ~substring:"foo" "foobar")))::tests;;


(* run the tests *)
let () = run_test_tt_main ("String_utils">:::tests)


