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

module Addition: sig

  (** represents linear combination of bv terms.
   * The value associated with an env [foo] is:
   * foo.constant + \sum_{(key, value) \in foo.symbolic} key*value
   *)
  type env = {
    symbolic : int BvTermHashamt.t;
    constant : Bitvector.t;
  }

  (** represents zero of the specified size *)
  val zero : int -> env

  (** converts an env to a bv_term *)
  val bv_term_of_env : env -> bv_term

  (** converts a bvterm to an env *)
  val env_of_bv_term : bv_term -> env
end


(** simplifies x + y -x + 4 in y + 4 *)
val simplify_additions: bv_term -> bv_term

(** simplifies x+1 = x+y in 1 = y *)
val simplify_comparison: bl_term -> bl_term
