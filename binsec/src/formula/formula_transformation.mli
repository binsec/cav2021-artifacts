(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2019                                               *)
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

open Formula

module Stats: sig
  val pp: Format.formatter -> unit -> unit
  val pp_json: Format.formatter -> unit -> unit
end

module CheckBindings: sig
  type env
  exception UnboundVariable of var

  val empty: env
  val process_entry: entry -> env -> env
end

val check_bindings: formula -> formula

module type INCREMENTAL_SIMPLIFICATION = sig
  type env
  type env_params

  (** int: expected size of the formula *)
  val create: env_params -> env

  (** processes one formula entry and returns either None if the formula
   * needs not be appended, or Some(x) when x needs to be appended (push_front)
   * to the formula *)
  val process_entry: env -> entry -> env*entry option

  (* simplify a full formula, takes the same params as create *)
  val simplify_formula: env_params -> formula -> formula
end

(* constant propagations while preserving the definition of a set of variables,
 * or all if None *)
module ConstantPropagation: INCREMENTAL_SIMPLIFICATION with type env_params=VarSet.t option

type row_params = {
  lst: int option; (** If [Some i] Use [ListEnv] with max depth [i] *)
  itv: bool; (** use interval domain, ignored if [lst < None] *)
  rbs: bool; (** rebase indices *)
  unfold: bool; (** remove irrelevant store prefix in read over write *)
}
module ReadOverWrite: INCREMENTAL_SIMPLIFICATION with type env_params=row_params
module AI: INCREMENTAL_SIMPLIFICATION with type env_params=bool

val rename_bl_var : (string -> string) -> bl_var -> bl_var
val rename_bv_var : (string -> string) -> bv_var -> bv_var
val rename_ax_var : (string -> string) -> ax_var -> ax_var

val rename_bl_term : (string -> string) -> bl_term -> bl_term
val rename_bv_term : (string -> string) -> bv_term -> bv_term
val rename_ax_term : (string -> string) -> ax_term -> ax_term

val replace_bl_term : def -> bl_term -> bl_term
val replace_bv_term : def -> bv_term -> bv_term
val replace_ax_term : def -> ax_term -> ax_term

val prune_and_inline     : ?keep:VarSet.t -> formula -> formula
val static_single_assignment : formula -> formula
val remove_ax_ite : formula -> formula

val taint : (var -> bool) -> formula -> formula
val taint2 : unfold:bool -> (var -> bool) -> formula -> formula

val synthetise_solutions_with_function_inversion:
  complete:bool
  -> (var -> bool) (* is param *)
  -> (var -> bool) (* is_controlled *)
  -> formula
  -> (formula * VarSet.t (* selectors *) * (var -> bool) (* fixed is_controlled *) )

(** simplify the formula to an equivalent one. Some variables can be inlined or pruned, 
 * except those in [keep] *)
val optimize :
  ?keep:VarSet.t ->
  ?lst:int -> ?cst:bool -> ?itv:bool -> ?prn:bool -> ?rbs:bool -> ?row:bool ->
  ?ssa:bool -> ?ai:bool -> ?unfold:bool ->
  formula -> formula

(** like optimize, but default values are taken from options *)
val optimize_from_options : ?keep:VarSet.t -> formula -> formula

(** converts \exists a, x P(a, x) to a formula implying \exists a \forall x P(a, x)
 * where a is the set of all variables where [is_controlled] is true *)
val to_universal : ?control_mem:bool -> ?control_mem_approx:bool -> ?quantifier:bool -> ?taintrow:bool -> (formula -> formula) -> (VarSet.elt -> bool) -> (VarSet.elt -> bool) -> formula -> Smtlib.script

(** like [to_universal] but uses default values from options simplifies the
 * formula with [optimize_from_options] *)
val to_universal_from_options :
  ?keep:VarSet.t -> (VarSet.elt -> bool) -> (VarSet.elt -> bool) -> formula -> Smtlib.script
