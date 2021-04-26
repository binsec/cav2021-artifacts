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
open Formula_utils

module Addition = struct
  (** represents a bv term.
   * The value associated with an env [foo] is:
   * foo.constant + \sum_{(key, value) \in foo.symbolic} key*value
   *)
  type env = {
    symbolic: int BvTermHashamt.t;
    constant: Bitvector.t;
  }

  (** represents 0 *)
  let zero size = { symbolic = BvTermHashamt.empty; constant = Bitvector.zeros size; }
  
  (** returns a term representing bv*n, where n >= 1 *)
  let term_times n bv =
    assert (n > 0);
    let rec recterm_times n acc =
      if n == 1 then
        acc
      else
        recterm_times (n-1) (mk_bv_add bv acc)
    in
    recterm_times n bv

  (* returns the value of the environment *)
  let bv_term_of_env env =
    let cst = mk_bv_cst env.constant in
    (* we add the constant last for row, so we accumulate on zero.
     * We rely on smart constructor to remove spurious zero *)
    let zero = mk_bv_zeros (bv_size cst) in
    let sym = BvTermHashamt.fold (fun key value acc -> match value with
        | 0 -> acc
        | n when n > 0 -> mk_bv_add acc (term_times n key)
        | n -> mk_bv_sub acc (term_times (-n) key)
      ) env.symbolic zero
    in
    mk_bv_add sym cst
        
  (** [visit_bv_term multiplicand env bv = foo]
   * the value of [foo] is [if multiplicand then (env+bv) else (env-bv)]
   * invariants:
   * *visit_bv_term calls itself only on bv terms of the same size
   * *multiplicand is true iff we traversed an even number of negations
   *)
  let rec visit_bv_term multiplicand env bv = match bv.bv_term_desc with
    | BvCst x ->
      let x = if multiplicand then x else Bitvector.neg x in
      { env with constant = Bitvector.add env.constant x; }
    | BvUnop(BvNeg, x) ->
      visit_bv_term (not multiplicand) env x
    | BvBnop(BvAdd, x, y) -> 
      visit_bv_term multiplicand (visit_bv_term multiplicand env x) y
    | BvBnop(BvSub, x, y) -> 
      visit_bv_term (not multiplicand) (visit_bv_term multiplicand env x) y
    | _ ->
      let offset = if multiplicand then 1 else -1 in
      let old = try BvTermHashamt.find bv env.symbolic with Not_found -> 0 in
      let symbolic = BvTermHashamt.add bv (old+offset) env.symbolic in
      { env with symbolic }
  
  let env_of_bv_term bv =
    let env = zero (bv_size bv) in
    visit_bv_term true env bv

  (** returns (e, e2) such that env = e - e2 and all symbolic terms have a
   * positive coefficient *)
  let split env =
    let env2 = zero (Bitvector.size_of env.constant) in
    let env, env2 = BvTermHashamt.fold
      (fun term times (env, env2) ->
         let env2 = if times < 0 then
             { env2 with symbolic = BvTermHashamt.add term (-times) env2.symbolic }
           else env2 in
         let env = if times <= 0 then
             { env with symbolic = BvTermHashamt.remove term env.symbolic } else env in
         (env, env2))
      env.symbolic (env, env2)
    in
    (* the constant is on env *)
    if BvTermHashamt.is_empty env2.symbolic then
      { env with constant = env2.constant }, { env2 with constant = Bitvector.neg env.constant }
    else
      env, env2
end

(* simplifies x - (y + x) into y *)
let simplify_additions bv = 
  let open Addition in
  match bv.bv_term_desc with
  | BvUnop(BvNeg, _) | BvBnop(BvAdd, _, _) | BvBnop(BvSub, _, _) ->
    let env = env_of_bv_term bv in
    bv_term_of_env env
  | _ -> bv


let simplify_comparison bl =
  let open Addition in
  match bl.bl_term_desc with
  | BvComp(op, bv1, bv2) when (match op with BvEqual | BvDistinct -> true | _ -> false) ->
    let bv = mk_bv_sub bv1 bv2 in
    let env = env_of_bv_term bv in
    let env, env2 = split env in
    mk_bv_comp op (bv_term_of_env env) (bv_term_of_env env2)
  | _ -> bl

