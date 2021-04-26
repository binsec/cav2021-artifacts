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

(** Symbolic state *)

module State : sig
  type t

  val initializations : t -> int Bitvector.Collection.Map.t

  val create : unit -> t

  (** if true, then the formula is unsat (by-product of simplifications, no smt involved *)
  val unsat : t -> bool

  val assign :  string -> Formula.sort -> Formula.term -> t -> t
  val declare : controlled:Dba.Non_deterministic_tag.control -> string -> Formula.sort -> t -> t
  (** if [wild] is set, then the variable is appended to uncontroled *)

  val constrain : ?assumption:bool -> Formula.bl_term -> t -> t
  (** [constrain c s] adds constraint [c] to state [s], as an assertion *)

  val comment : string -> t -> t
  (** [comment cmt s] *)

  val formula : t -> Formula.formula

  val memory_term : Formula.ax_term -> string * Formula.sort * Formula.term

  val get_memory : t -> Formula.ax_term

  val get_path_constraint : t -> Formula.bl_term

  (** automatically declares missing variables, thus returns [t] *)
  val get_bv : string -> Size.Bit.t -> t -> Formula.bv_term * t

  (** retroactively do as if we had loaded memory from the image at this address from the beginning *)
  val init_mem_at : addr:Bitvector.t -> size:int -> t -> t

(** @[addr, size] := value of the image at this address
 * contrary to init_mem_at, this is not retroactive *)
  val assign_from_image : addr:Bitvector.t -> size:int -> t -> t

  (** returns the smt name of variables declared as controlled. Defined variables are omitted. *)
  val controlled_vars : t -> Formula.VarSet.t

  (** returns the smt name of variables declared as uncontrolled. Defined variables are omitted. *)
  val uncontrolled_vars : t -> Formula.VarSet.t

  (** returns the smt name of variables declared as universal parameter. Defined variables are omitted. *)
  val parameter_vars : t -> Formula.VarSet.t

  val pp : Format.formatter -> t -> unit

  (** undeclare all variables except the path constraint *)
  val strip : t -> t

  (** merge two states as in path merging *)
  val merge : t -> t -> t
end
