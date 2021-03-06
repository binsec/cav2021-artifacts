(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Codex plugin of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* This file defines a set of static and dynamic parameters. 

   Static parameters, if muted, are muted very early: this file should
   be the first executed, and it depends on nothing else. After that
   the configuration does not change: for the rest of the application,
   the parameters are considered as being static.

   This simplifies adding configuration options to the rest of the
   code; for instance it generally allows using local modules instead
   of functors. Also, we can use a purely static configuration here,
   to generate an optimized version of Codex.

   Dynamic parameters are supposed to be changed dynamically, at any
   time (e.g. during an interactive session). *)

(* All parameters are function from unit to their result, in case they
   become dynamic later. *)


(** Domains options *)

let r_ptr_size = ref @@ -333;;
let set_ptr_size x = r_ptr_size := x;;
let ptr_size () = !r_ptr_size;;

(* Dummy size when size of functions is required (e.g. allocation of a dummy base). *)
let function_size () = 0;;


(* If true, record the parents of a variable (i.e. set of variables
   whose defs immediately depends on the variable). Necessary for
   re-forward constraint propagation. *)
let constraints_register_parents() = true;;



(** Constraints generation options.  *)

(* When true, we put an assume whenever there is an alarm. This makes
   the analysis more precise, but also slower; especially the set of
   conditions on which we depend (represented as a BDD) can become
   much larger. *)
let assume_alarms() = true;;


(** Debugging options *)

(* When dumping a term to a SMT solver, dump the input to the SMT solver. *)
let print_smt_input() = false (* true *)(* false *)

(** Goal-oriented options *)

(* Should we try to prove unproved alarms using goal-oriented procedures. *)
let try_hard_on_alarms() = false;;

(* Should we try to prove unproved user assertions using goal-oriented procedures. *)
let try_hard_on_assertions() = true;;

(* Should goal-oriented procedures attempt to perform deductive verification? *)
let try_hard_with_deductive_verification() = true

(* Should goal-oriented procedures attempt to perform symbolic execution? *)
let try_hard_with_symbolic_execution() = true

(* Should goal-oriented procedures attempt to perform software model checking with muz? *)
let try_hard_with_muz() = true

(* Which muz engine to use. Valid values include clp for symbolic
   execution, and pdr for property-directed reachability. *)
(* Now it has spacer too. *)
let muz_engine() = "pdr"
(* let muz_engine() = "clp" *)
let muz_engine() = "spacer"

(* Whether to also check assertions that have been proved by abstract
   interpretation with the goal-oriented procedures. This is mainly
   used for debugging. *)
let try_hard_double_check() = false

(* Number of seconds before the SMT solver times out. *)
let smt_timeout() = 10;;


let r_max_valid_absolute_address = ref Z.minus_one;;
let set_max_valid_absolute_address x = r_max_valid_absolute_address := x;;
let max_valid_absolute_address() = !r_max_valid_absolute_address;;

let r_show_memory_on_exit = ref true
let show_memory_on_exit() = !r_show_memory_on_exit
let set_show_memory_on_exit bool = r_show_memory_on_exit := bool
let _ = at_exit (fun () ->
    if show_memory_on_exit() then
      let minor,promoted,major = Gc.counters() in
      let allocated = minor +. major -. promoted in
      let l1 = String.length (Printf.sprintf "%.0f" allocated) in
      Printf.eprintf "minor_words:     %*.0f\n\
                      major_words:     %*.0f\n\
                      promoted_words:  %*.0f\n\
                      total_allocated: %.0f\n"
        l1 minor l1 major l1 promoted allocated
  )
;;


