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

module Robust_status : sig
  open Formula
  type t = {
    universal: status; (** status of the universally quantified query *)
    existential: status; (** status of the existentially quantified query *)
  }

  (** returns whether this the formula is satisfiable in terms of universal or existential *)
  val translate: universal:bool -> t -> bool option

  (** returns whether this branch should be explored further. None if the solver returned UNKNOWN or similar *)
  val continue_exploration: t -> bool option

  (** returns whether the goal was successfully reached. None if the solver returned UNKNOWN or similar *)
  val accept_reach: t -> bool option

  val pp: Format.formatter -> t -> unit

  (** constructs { robust = arg; existental = arg; } *)
  val const: status -> t
end

module Query_stats : sig
  val pp : Format.formatter -> unit -> unit
  val pp_json : Format.formatter -> unit -> unit
end

module Solver : sig

  (** explore indicates wether the checks is for exploring a branch or reaching a goal *)
  val check_satistifiability :
    explore:bool ->
    Sse_types.Path_state.t -> Robust_status.t * Sse_types.Path_state.t

  val get_model : universal:bool -> Sse_types.Path_state.t -> Smt_model.t option

  val enumerate_values :
    int -> Formula.bv_term -> Sse_types.Path_state.t
    -> Bitvector.t list * Sse_types.Path_state.t
end

module Translate : sig
  (** missing bitvectors are implicitely declared so this returns a new symbolic state *)
  val expr : Sse_symbolic.State.t -> Dba.Expr.t -> Formula.bv_term * Sse_symbolic.State.t

  (** wrapper over expr for Path state *)
  val expr' : Sse_types.Path_state.t -> Dba.Expr.t -> Formula.bv_term * Sse_types.Path_state.t

  val assign :
    Dba.LValue.t -> Dba.Expr.t -> Sse_symbolic.State.t -> Sse_symbolic.State.t

  val assignment :
    Dba.LValue.t -> Dba.Expr.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t

  val nondet:
    ?naming_hint:string ->
    controlled:Dba.Non_deterministic_tag.control ->
    Dba.LValue.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t

  val assume:
    Dba.Expr.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t


  val assertion:
    Dba.Expr.t -> Sse_types.Path_state.t -> Sse_types.Path_state.t
end
