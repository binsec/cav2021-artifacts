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

module Stats = struct
  type t = {
    time : float;
    calls: int; (** number of times a formula was simplified *)
    max_universally_quantified_bv : int;
    max_universally_quantified_bool : int;
    max_universally_quantified_array : int;
    universal_calls: int; (** number of times a formula with a universal quantifier was simplified *)
    universal_quantifier_remains: int; (** number of times a formula was simplified enough to remove all universal quantifiers *)
  }

  let empty = {
    time = 0.;
    calls = 0;
    universal_calls = 0;
    max_universally_quantified_bv = 0;
    max_universally_quantified_bool = 0;
    max_universally_quantified_array = 0;
    universal_quantifier_remains = 0;
  }

  let add_call s time ~universal ~quantified_bool ~quantified_bv ~quantified_array =
    { calls = s.calls + 1;
      universal_calls = s.universal_calls + (if universal then 1 else 0);
      time = s.time +. time;
      max_universally_quantified_bool =
        max s.max_universally_quantified_bool quantified_bool;
      max_universally_quantified_bv =
        max s.max_universally_quantified_bv quantified_bv;
      max_universally_quantified_array =
        max s.max_universally_quantified_array quantified_array;
      universal_quantifier_remains = s.universal_quantifier_remains + (if (quantified_bv = 0) && (quantified_bool = 0) && (quantified_array = 0) then 0 else 1);
    }

  let pp ppf s =
    Format.fprintf ppf
      "@[<v 0>\
        @[<h>total simplification time %f@]@,\
        @[<h>avg simplification time %f@]@,\
        @[<h>simplification count %d@]@,\
        @[<h>universal quantifier simplification count %d@]@,\
        @[<h>times universal quantifier_remains after simplification %d@]@,\
        @[<h>max universally quantified bool variable %d@]@,\
        @[<h>max universally quantified bv variable %d@]@,\
        @[<h>max universally quantified array variable %d@]@,\
        @]"
      s.time
      (s.time /. (float_of_int s.calls))
      s.calls
      s.universal_calls
      s.universal_quantifier_remains
      s.max_universally_quantified_bool
      s.max_universally_quantified_bv
      s.max_universally_quantified_array
  ;;

  let pp_json ppf s =
    Format.fprintf ppf
      {|@[<v 0>{
        @[<h>"time": %f@],@,
        @[<h>"calls": %d@],@,
        @[<h>"universal_calls": %d@],@,
        @[<h>"universal_quantifier_remains": %d@],@,
        @[<h>"max_universally_quantifier_bool": %d@],@,
        @[<h>"max_universally_quantified_bv": %d@],@,
        @[<h>"max_universally_quantified_array": %d@]
        }@] |}
      s.time
      s.calls
      s.universal_calls
      s.universal_quantifier_remains
      s.max_universally_quantified_bool
      s.max_universally_quantified_bv
      s.max_universally_quantified_array
  ;;


  module R = struct
    let value = ref empty
    let add_call time ~universal ~quantified_bool ~quantified_bv ~quantified_array =
      value := add_call !value time ~universal ~quantified_bool ~quantified_bv ~quantified_array
    let pp ppf () = pp ppf !value
    let pp_json ppf () = pp_json ppf !value
  end
  include R
  let add_universal_call = add_call ~universal:true
  let add_normal_call = add_call ~universal:false ~quantified_bool:0 ~quantified_bv:0 ~quantified_array:0
end

module CheckBindings = struct
  type env = (unit, unit, unit) BindMap.t
  exception UnboundVariable of var

  let empty = BindMap.empty

  let rec add_decl (env:env) decl: env = match decl.decl_desc with
        | Formula.BlDecl (v, _) -> BindMap.bl_store env v () 
        | Formula.BvDecl (v, _) -> BindMap.bv_store env v ()
        | Formula.AxDecl (v, _) -> BindMap.ax_store env v ()

  and add_decls (env:env) (decls: decl list) = List.fold_left add_decl env decls

  and add_def (env:env) def: env = match def.def_desc with
        | BlDef(v, d, i) ->
          (visit_bl_term (add_decls env d) i;
          BindMap.bl_store env v ())
        | BvDef(v, d, i) ->
          (visit_bv_term (add_decls env d) i;
          BindMap.bv_store env v ())
        | AxDef(v, d, i) ->
          (visit_ax_term (add_decls env d) i;
          BindMap.ax_store env v ())

and add_defs env defs = List.fold_left add_def env defs

and visit_term (env:env) t = match t.term_desc with
  | BlTerm t -> visit_bl_term env t
  | BvTerm t -> visit_bv_term env t
  | AxTerm t -> visit_ax_term env t

and  visit_bl_term (env:env) bl = match bl.bl_term_desc with
    | Formula.BlTrue -> ()
    | Formula.BlFalse -> ()
    | Formula.BlFun (v, args) -> begin
      List.iter (visit_term env) args;
      if BindMap.bl_lookup env v = None
      then raise (UnboundVariable(BlVar v))
    end
    | Formula.BlLet (defs, inner) ->
      let env = add_defs env defs in
      visit_bl_term env inner
    | Formula.BlUnop (_, t) -> visit_bl_term env t
    | Formula.BlComp (_, t1, t2)
    | Formula.BlBnop (_, t1, t2) -> (visit_bl_term env t1; visit_bl_term env t2)
    | Formula.BvComp (_, t1, t2) -> (visit_bv_term env t1; visit_bv_term env t2)
    | Formula.AxComp (_, t1, t2) -> (visit_ax_term env t1; visit_ax_term env t2)
    | Formula.BlIte (x, y, z) ->
      (visit_bl_term env x;
       visit_bl_term env y;
       visit_bl_term env z)

  and visit_bv_term (env:env) bv: unit = match bv.bv_term_desc with
    | Formula.BvCst _ -> ()
    | Formula.BvFun (v, args) ->
      List.iter (visit_term env) args;
      if BindMap.bv_lookup env v = None
      then raise (UnboundVariable(BvVar v))
    | Formula.BvLet (defs, bv) ->
      let env = add_defs env defs in
      visit_bv_term env bv
    | Formula.BvUnop (_, t) -> visit_bv_term env t
    | Formula.BvBnop (_, t1, t2) -> (visit_bv_term env t1; visit_bv_term env t2)
    | Formula.BvIte (x, y, z) -> 
      (visit_bl_term env x;
       visit_bv_term env y;
       visit_bv_term env z)
    | Formula.Select (_, ax, idx) ->
      (visit_bv_term env idx ;visit_ax_term env ax)

  and visit_ax_term env a = match a.ax_term_desc with
    | Formula.AxFun (v, args) ->
      List.iter (visit_term env) args;
      if BindMap.ax_lookup env v = None
      then raise (UnboundVariable(AxVar v))
    | Formula.AxLet (defs, inner) -> 
      let env = add_defs env defs in
      visit_ax_term env inner
    | Formula.AxIte (x, y, z) -> 
      visit_bl_term env x;
      visit_ax_term env y;
      visit_ax_term env z
    | Formula.Store (_, ax, bv, bv') ->
      visit_ax_term env ax;
      visit_bv_term env bv;
      visit_bv_term env bv'

  and  process_entry_inner entry env = match entry.entry_desc with
    | Formula.Declare d -> add_decl env d
      | Formula.Define d -> add_def env d
      | Formula.Assume bl
      | Formula.Assert bl -> (visit_bl_term env bl; env)
      | Formula.Comment _ -> env

  and process_entry entry env =
    try process_entry_inner entry env
    with UnboundVariable v -> 
     Formula_options.Logger.fatal ~e:(UnboundVariable v) "Variable %a is not defined in %a"
                  Formula_pp.pp_var v Formula_pp.pp_entry entry

end

let check_bindings fm =
  begin
    try
      fold_forward CheckBindings.process_entry fm CheckBindings.empty |> ignore
    with CheckBindings.UnboundVariable _ as e ->  begin
        Formula_options.Logger.error "The formula with inconsistent bindings was:@ %a" Formula_pp.pp_formula fm;
        raise e
      end
  end;
  fm

module type INCREMENTAL_SIMPLIFICATION_INNER = sig
  type env
  type env_params

  val create: env_params -> env

  (** processes one formula entry and returns either None if the formula
   * needs not be appended, or Some(x) when x needs to be appended (push_front)
   * to the formula *)
  val process_entry: env -> entry -> env * entry option
end

module type INCREMENTAL_SIMPLIFICATION = sig
  type env
  type env_params

  val create: env_params -> env

  (** processes one formula entry and returns either None if the formula
   * needs not be appended, or Some(x) when x needs to be appended (push_front)
   * to the formula *)
  val process_entry: env -> entry -> env * entry option

  (* simplify a full formula, takes the same params as create *)
  val simplify_formula: env_params -> formula -> formula
end

module Make(M: INCREMENTAL_SIMPLIFICATION_INNER): INCREMENTAL_SIMPLIFICATION with 
  type env_params = M.env_params = struct

  type env_params = M.env_params
  type env = M.env * CheckBindings.env

  let create p = (M.create p, CheckBindings.empty)

  let process_entry (env, check) entry =
    let env, entry = M.process_entry env entry in
    let check = match entry with
        None -> check
      | Some e -> CheckBindings.process_entry e check
    in
    (env, check), entry

  let simplify_formula params fm =
    let env = create params in
    fold_forward (fun entry (env, fm) -> match process_entry env entry with
        | env, None -> env, fm
        | env, Some(x) -> env, push_front x fm
      ) fm (env, empty)
    |> snd
end
    

let rename_bl_var f bl =
  bl_var (f bl.bl_name)

let rename_bv_var f bv =
  bv_var (f bv.bv_name) bv.bv_size

let rename_ax_var f ax =
  ax_var (f ax.ax_name) ax.idx_size ax.elt_size

let rename_list : 'env 'a 'b.
  ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
  fun f env ls -> List.map (f env) ls

let rec rename_term f tm =
  rename_term_desc f tm.term_desc

and rename_term_desc f = function
  | BlTerm bl -> mk_bl_term (rename_bl_term f bl)
  | BvTerm bv -> mk_bv_term (rename_bv_term f bv)
  | AxTerm ax -> mk_ax_term (rename_ax_term f ax)

and rename_bl_term f bl =
  rename_bl_term_desc f bl.bl_term_desc

and rename_bl_term_desc f = function
  | BlTrue -> mk_bl_true
  | BlFalse -> mk_bl_false
  | BlFun (v,ls) ->
    mk_bl_fun (rename_bl_var f v) (rename_list rename_term f ls)
  | BlLet (bn,bl) ->
    mk_bl_let (rename_list rename_def f bn) (rename_bl_term f bl)
  | BlUnop (u,bl) ->
    mk_bl_unop u (rename_bl_term f bl)
  | BlBnop (b,bl1,bl2) ->
    mk_bl_bnop b (rename_bl_term f bl1) (rename_bl_term f bl2)
  | BlComp (c,bl1,bl2) ->
    mk_bl_comp c (rename_bl_term f bl1) (rename_bl_term f bl2)
  | BvComp (c,bv1,bv2) ->
    mk_bv_comp c (rename_bv_term f bv1) (rename_bv_term f bv2)
  | AxComp (c,ax1,ax2) ->
    mk_ax_comp c (rename_ax_term f ax1) (rename_ax_term f ax2)
  | BlIte (bl,bl1,bl2) ->
    mk_bl_ite (rename_bl_term f bl) (rename_bl_term f bl1) (rename_bl_term f bl2)

and rename_bv_term f bv =
  rename_bv_term_desc f bv.bv_term_desc

and rename_bv_term_desc f = function
  | BvCst bv -> mk_bv_cst bv
  | BvFun (v,ls) ->
    mk_bv_fun (rename_bv_var f v) (rename_list rename_term f ls)
  | BvLet (bn,bv) ->
    mk_bv_let (rename_list rename_def f bn) (rename_bv_term f bv)
  | BvUnop (u,bv) ->
    mk_bv_unop u (rename_bv_term f bv)
  | BvBnop (b,bv1,bv2) ->
    mk_bv_bnop b (rename_bv_term f bv1) (rename_bv_term f bv2)
  | BvIte (bl,bv1,bv2) ->
    mk_bv_ite (rename_bl_term f bl) (rename_bv_term f bv1) (rename_bv_term f bv2)
  | Select (n,ax,bv) -> mk_select n (rename_ax_term f ax) (rename_bv_term f bv)

and rename_ax_term f ax =
  rename_ax_term_desc f ax.ax_term_desc

and rename_ax_term_desc f = function
  | AxFun (v,ls) ->
    mk_ax_fun (rename_ax_var f v) (rename_list rename_term f ls)
  | AxLet (bn,ax) ->
    mk_ax_let (rename_list rename_def f bn) (rename_ax_term f ax)
  | AxIte (bl,ax1,ax2) ->
    mk_ax_ite (rename_bl_term f bl) (rename_ax_term f ax1) (rename_ax_term f ax2)
  | Store (n,ax,bv1,bv2) ->
    mk_store n (rename_ax_term f ax) (rename_bv_term f bv1) (rename_bv_term f bv2)

and rename_def f df =
  rename_def_desc f df.def_desc

and rename_def_desc f = function
  | BlDef (v,ls,bl) ->
    mk_bl_def (rename_bl_var f v) (rename_list rename_decl f ls) (rename_bl_term f bl)
  | BvDef (v,ls,bv) ->
    mk_bv_def (rename_bv_var f v) (rename_list rename_decl f ls) (rename_bv_term f bv)
  | AxDef (v,ls,ax) ->
    mk_ax_def (rename_ax_var f v) (rename_list rename_decl f ls) (rename_ax_term f ax)

and rename_decl f dc =
  rename_decl_desc f dc.decl_desc

and rename_decl_desc f = function
  | BlDecl (v,ls) -> mk_bl_decl (rename_bl_var f v) ls
  | BvDecl (v,ls) -> mk_bv_decl (rename_bv_var f v) ls
  | AxDecl (v,ls) -> mk_ax_decl (rename_ax_var f v) ls


let defs_shadow s dfs =
  match s.def_desc with
  | BlDef (v,_,_) ->
    List.fold_left (fun bool df ->
        bool || match df.def_desc with
        | BvDef _ | AxDef _ -> false
        | BlDef (v',_,_) -> v = v')
      false dfs
  | BvDef (v,_,_) ->
    List.fold_left (fun bool df ->
        bool || match df.def_desc with
        | BlDef _ | AxDef _ -> false
        | BvDef (v',_,_) -> v = v')
      false dfs
  | AxDef (v,_,_) ->
    List.fold_left (fun bool df ->
        bool || match df.def_desc with
        | BlDef _ | BvDef _ -> false
        | AxDef (v',_,_) -> v = v')
      false dfs

let decls_shadow s dcs =
  match s.def_desc with
  | BlDef (v,_,_) ->
    List.fold_left (fun bool dc ->
        bool || match dc.decl_desc with
        | BvDecl _ | AxDecl _ -> false
        | BlDecl (v',_) -> v = v')
      false dcs
  | BvDef (v,_,_) ->
    List.fold_left (fun bool dc ->
        bool || match dc.decl_desc with
        | BlDecl _ | AxDecl _ -> false
        | BvDecl (v',_) -> v = v')
      false dcs
  | AxDef (v,_,_) ->
    List.fold_left (fun bool dc ->
        bool || match dc.decl_desc with
        | BlDecl _ | BvDecl _ -> false
        | AxDecl (v',_) -> v = v')
      false dcs

let replace_list : 'env 'a 'b.
  ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
  fun f env ls -> List.map (f env) ls

let rec replace_term s tm =
  replace_term_desc s tm.term_desc

and replace_term_desc s = function
  | BlTerm bl -> mk_bl_term (replace_bl_term s bl)
  | BvTerm bv -> mk_bv_term (replace_bv_term s bv)
  | AxTerm ax -> mk_ax_term (replace_ax_term s ax)

and replace_bl_term s bl =
  replace_bl_term_desc s bl.bl_term_desc

and replace_bl_term_desc s = function
  | BlTrue -> mk_bl_true
  | BlFalse -> mk_bl_false
  | BlFun (v,ls) ->
    (match s.def_desc with
     | BvDef (_,_,_) | AxDef (_,_,_) ->
       mk_bl_fun v (replace_list replace_term s ls)
     | BlDef (v',[],bl') ->
       if v = v' then
         if ls = [] then bl'
         else invalid_arg "Ill-typed replacement"
       else mk_bl_fun v (replace_list replace_term s ls)
     | BlDef _ -> invalid_arg "Ill-typed replacement")
  | BlLet (bn,bl) ->
    if defs_shadow s bn then mk_bl_let (replace_list replace_def s bn) bl
    else mk_bl_let (replace_list replace_def s bn) (replace_bl_term s bl)
  | BlUnop (u,bl) ->
    mk_bl_unop u (replace_bl_term s bl)
  | BlBnop (b,bl1,bl2) ->
    mk_bl_bnop b (replace_bl_term s bl1) (replace_bl_term s bl2)
  | BlComp (c,bl1,bl2) ->
    mk_bl_comp c (replace_bl_term s bl1) (replace_bl_term s bl2)
  | BvComp (c,bv1,bv2) ->
    mk_bv_comp c (replace_bv_term s bv1) (replace_bv_term s bv2)
  | AxComp (c,ax1,ax2) ->
    mk_ax_comp c (replace_ax_term s ax1) (replace_ax_term s ax2)
  | BlIte (bl,bl1,bl2) ->
    mk_bl_ite (replace_bl_term s bl) (replace_bl_term s bl1) (replace_bl_term s bl2)

and replace_bv_term s bv =
  replace_bv_term_desc s bv.bv_term_desc

and replace_bv_term_desc s = function
  | BvCst bv -> mk_bv_cst bv
  | BvFun (v,ls) ->
    (match s.def_desc with
     | BlDef (_,_,_) | AxDef (_,_,_) ->
       mk_bv_fun v (replace_list replace_term s ls)
     | BvDef (v',[],bv') ->
       if v = v' then
         if ls = [] then bv'
         else invalid_arg "Ill-typed replacement"
       else mk_bv_fun v (replace_list replace_term s ls)
     | BvDef _ -> invalid_arg "Ill-typed replacement")
  | BvLet (bn,bv) ->
    if defs_shadow s bn then mk_bv_let (replace_list replace_def s bn) bv
    else mk_bv_let (replace_list replace_def s bn) (replace_bv_term s bv)
  | BvUnop (u,bv) ->
    mk_bv_unop u (replace_bv_term s bv)
  | BvBnop (b,bv1,bv2) ->
    mk_bv_bnop b (replace_bv_term s bv1) (replace_bv_term s bv2)
  | BvIte (bl,bv1,bv2) ->
    mk_bv_ite (replace_bl_term s bl) (replace_bv_term s bv1) (replace_bv_term s bv2)
  | Select (n,ax,bv) ->
    mk_select n (replace_ax_term s ax) (replace_bv_term s bv)

and replace_ax_term s ax =
  replace_ax_term_desc s ax.ax_term_desc

and replace_ax_term_desc s = function
  | AxFun (v,ls) ->
    (match s.def_desc with
     | BlDef (_,_,_) | BvDef (_,_,_) ->
       mk_ax_fun v (replace_list replace_term s ls)
     | AxDef (v',[],ax') ->
       if v = v' then
         if ls = [] then ax'
         else invalid_arg "Ill-typed replacement"
       else mk_ax_fun v (replace_list replace_term s ls)
     | AxDef _ -> invalid_arg "Ill-typed replacement")
  | AxLet (bn,ax) ->
    if defs_shadow s bn then mk_ax_let (replace_list replace_def s bn) ax
    else mk_ax_let (replace_list replace_def s bn) (replace_ax_term s ax)
  | AxIte (bl,ax1,ax2) ->
    mk_ax_ite (replace_bl_term s bl) (replace_ax_term s ax1) (replace_ax_term s ax2)
  | Store (n,ax,bv1,bv2) ->
    mk_store n (replace_ax_term s ax) (replace_bv_term s bv1) (replace_bv_term s bv2)

and replace_def s df =
  replace_def_desc s df.def_desc

and replace_def_desc s = function
  | BlDef (v,ls,bl) ->
    if decls_shadow s ls then mk_bl_def v ls bl
    else mk_bl_def v ls (replace_bl_term s bl)
  | BvDef (v,ls,bv) ->
    if decls_shadow s ls then mk_bv_def v ls bv
    else mk_bv_def v ls (replace_bv_term s bv)
  | AxDef (v,ls,ax) ->
    if decls_shadow s ls then mk_ax_def v ls ax
    else mk_ax_def v ls (replace_ax_term s ax)

(* this variable should be inlined for row to work *)
(* needed to rebase esp2 + 4 to esp + 6 when esp2 is defined as esp + 2 *)
let should_inline bv = match bv.bv_term_desc with
  | BvCst _ -> true
  | BvFun (_,[]) -> true
  | BvBnop (BvAdd, _, _) | BvBnop (BvSub, _, _) -> true
  | BvUnop (BvNeg, _) -> true
  | _ -> false

(** returns the congruence information for a Codex binary *)
let congruence_for ~size binary =
  let ival = Codex.Ival_basis.binary_to_ival ~signed:false ~size binary in
  if Codex.Framac_ival.Ival.(is_bottom ival || is_singleton_int ival) then Z.(zero, one) else
    let _, _, rem, modulo = Codex.Framac_ival.Ival.min_max_r_mod ival in 
    rem, modulo

(** returns the interval information for a Codex binary *)
let min_max_for ~signed ~size binary =
  let integer_to_bitvector x =
    let bigint = Bigint.of_zarith x in
    Bitvector.create bigint size
  in
  let integer_to_bitvector x = match x with None -> None | Some x -> Some(integer_to_bitvector x) in
  let ival = Codex.Ival_basis.binary_to_ival ~signed ~size binary in
  if Codex.Framac_ival.Ival.is_bottom ival then None, None else
    let min, max = Codex.Framac_ival.Ival.min_and_max ival in
    integer_to_bitvector min, integer_to_bitvector max

(* Assert table  *)

module Assertbl :
sig
  type t
  val empty: t
  val get : t -> bl_term -> bool
  val set : t -> bl_term -> t
end = struct
  type t = bool BlTermHashamt.t

  let empty = BlTermHashamt.singleton mk_bl_true true

  let get t bl =
    try BlTermHashamt.find bl t
    with Not_found -> false

  let set t bl =
    BlTermHashamt.add bl true t
end


(* Moving asserts early
 *
 * Read over write uses asserts to get information about intervals to abstract domains
 * but SSE puts asserts completely at the end of the formula, which prevents ROW
 * from taking advantage of it. *)

module EarlyAsserts =
struct

  type unique_entry = {
    (* the entry has already been added back to the formula *)
    mutable dead: bool;
    entry: entry;
  }

  let mk_unique_entry entry = { dead = false; entry }

  module EntrySet = Set.Make
      (struct
        type t = unique_entry
        (* don't compare the [dead] field *)
        let compare a b = compare a.entry b.entry
      end)

  type env = {
    (* var -> foo::_ means that foo depends on var being defined *)
    revdeps : EntrySet.t VarHashtbl.t;
    (* constant assertions *)
    mutable constant : EntrySet.t;
  }


  let create n = {
    revdeps = VarHashtbl.create n;
    constant = EntrySet.empty;
  }

  (** get the list of asserts/assumes mentioning this variable *)
  let get_dependents env var =
    match VarHashtbl.find_opt env.revdeps var with
    | None -> EntrySet.empty
    | Some(x) -> x

  (** declare that the entry depends on the variable being defined *)
  let add_dependency env entry var =
    let existing = get_dependents env var in
    VarHashtbl.replace env.revdeps var (EntrySet.add entry existing)

  type visitor = BindEnv.t

  (* visit functions return the free variables in their argument *)
  let visit_list : 'env 'a 'b.
    ('env -> 'a -> VarSet.t) -> 'env -> 'a list -> VarSet.t =
    fun f env ls -> List.fold_left (fun set x ->
        let y = f env x in
        VarSet.union set y
      ) VarSet.empty ls

  let rec visit_term visitor tm: VarSet.t =
    visit_term_desc visitor tm.term_desc

  and visit_term_desc visitor = function
    | BlTerm bl -> visit_bl_term visitor bl
    | BvTerm bv -> visit_bv_term visitor bv
    | AxTerm ax -> visit_ax_term visitor ax

  and visit_bl_term visitor bl: VarSet.t =
    visit_bl_term_desc visitor bl.bl_term_desc

  and visit_bl_term_desc visitor = function
    | BlTrue | BlFalse -> VarSet.empty
    | BlFun (v,ls) ->
      let this = 
        if BindEnv.bl_lookup visitor v = BindEnv.Free then
          VarSet.singleton (BlVar v)
        else
          VarSet.empty
      in
      let x = visit_list visit_term visitor ls in
      VarSet.union x this
    | BlLet (bn,bl) ->
        let part1 = visit_list visit_def visitor bn in
        let visitor_with_let = List.fold_left BindEnv.def visitor bn in
        let part2 = visit_bl_term visitor_with_let bl in
        VarSet.union part1 part2
    | BlUnop (_,bl) -> visit_bl_term visitor bl
    | BlBnop (_,bl1,bl2) | BlComp(_, bl1, bl2) ->
      VarSet.union (visit_bl_term visitor bl1) (visit_bl_term visitor bl2)
    | BvComp (_,bv1,bv2) ->
      VarSet.union (visit_bv_term visitor bv1) (visit_bv_term visitor bv2)
    | AxComp (_,ax1,ax2) ->
      VarSet.union (visit_ax_term visitor ax1) (visit_ax_term visitor ax2)
    | BlIte (bl,bl1,bl2) ->
      (visit_bl_term visitor bl
       |> VarSet.union (visit_bl_term visitor bl1)
       |> VarSet.union (visit_bl_term visitor bl2))

  and visit_bv_term visitor bv =
    visit_bv_term_desc visitor bv.bv_term_desc

  and visit_bv_term_desc visitor = function
    | BvCst _ -> VarSet.empty
    | BvFun (v,ls) ->
      let this = 
        if BindEnv.bv_lookup visitor v = BindEnv.Free then
          VarSet.singleton (BvVar v)
        else
          VarSet.empty
      in
      VarSet.union (visit_list visit_term visitor ls) this
    | BvLet (bn,bv) ->
        let part1 = visit_list visit_def visitor bn in
        let visitor = List.fold_left BindEnv.def visitor bn in
        let part2 = visit_bv_term visitor bv in
        VarSet.union part1 part2
    | BvUnop (_,bv) -> visit_bv_term visitor bv
    | BvBnop (_,bv1,bv2) ->
      VarSet.union (visit_bv_term visitor bv1) (visit_bv_term visitor bv2)
    | BvIte (bl,bv1,bv2) ->
      (visit_bl_term visitor bl
       |> VarSet.union (visit_bv_term visitor bv1)
       |> VarSet.union (visit_bv_term visitor bv2))
    | Select (_,ax,bv) ->
      VarSet.union (visit_bv_term visitor bv) (visit_ax_term visitor ax)

  and visit_ax_term visitor ax =
    visit_ax_term_desc visitor ax.ax_term_desc

  and visit_ax_term_desc visitor = function
    | AxFun (v,ls) ->
      let this =
        if BindEnv.ax_lookup visitor v = BindEnv.Free then
          VarSet.singleton (AxVar v)
        else
          VarSet.empty
      in
      VarSet.union (visit_list visit_term visitor ls) this
    | AxLet (bn,ax) ->
        let part1 = visit_list visit_def visitor bn in
        let visitor = List.fold_left BindEnv.def visitor bn in
        let part2 = visit_ax_term visitor ax in
        VarSet.union part1 part2
    | AxIte (bl,ax1,ax2) ->
      (visit_bl_term visitor bl
       |> VarSet.union (visit_ax_term visitor ax1)
       |> VarSet.union (visit_ax_term visitor ax2))
    | Store (_,ax,bv1,bv2) ->
      (visit_ax_term visitor ax
       |> VarSet.union (visit_bv_term visitor bv1)
       |> VarSet.union (visit_bv_term visitor bv2))

  and visit_def visitor df =
    visit_def_desc visitor df.def_desc

  and with_bindings : 'a 'b. visitor -> decl list -> (visitor -> 'a -> 'b) -> 'a -> 'b =
    fun visitor ls f x ->
      let visitor = List.fold_left BindEnv.decl visitor ls in
      f visitor x

  and visit_def_desc visitor = function
    | BlDef (_,ls,bl) -> with_bindings visitor ls visit_bl_term bl
    | BvDef (_,ls,bv) -> with_bindings visitor ls visit_bv_term bv
    | AxDef (_,ls,ax) -> with_bindings visitor ls visit_ax_term ax

  (* add entries to fm and mark them dead ("to never add again") *)
  let add_entries entries filter fm =
    EntrySet.fold (fun entry fm ->
        if not entry.dead && filter entry.entry then begin
          entry.dead <- true;
          push_back entry.entry fm
        end else fm) entries fm

  (* put assume before assert so AI can learn !*)
  (* note: because we process formulas reverse, we push assert first *)
  let add_entries entries fm =
    add_entries entries (
      fun entry -> match entry.entry_desc with | Assert _ -> true | _ -> false
    ) fm
    |> add_entries entries (fun _ -> true)

  (** like get variants, but remove the binding *)
  let consume_dependents env v = 
    let res = get_dependents env v in
    VarHashtbl.remove env.revdeps v;
    res
end

(** Moves asserts and assumes as early as possible *)
let move_asserts_early fm =
  let open EarlyAsserts in
  let env = create (length fm / 4) in
  let res = fold_backward
    (fun entry fm ->
       match entry.entry_desc with
         (* push all the asserts and assume which depend on a variable
          * just after the declaration of the variable *)
         | Declare dc -> begin
             let var = match dc.decl_desc with
               | BlDecl(v, _) -> BlVar(v)
               | BvDecl(v, _) -> BvVar(v)
               | AxDecl(v, _) -> AxVar(v)
             in
             add_entries (consume_dependents env var) fm |>
             push_back entry
           end
         | Define df -> begin
             let var = match df.def_desc with
               | BlDef(v, _, _) -> BlVar(v)
               | BvDef(v, _, _) -> BvVar(v)
               | AxDef(v, _, _) -> AxVar(v)
             in
             add_entries (consume_dependents env var) fm |>
             push_back entry
           end
         | Comment _ -> push_back entry fm
         (* remove the assertion and store it in the env along with its dependencies *)
         | Assert bl | Assume bl -> 
           let process_bl_aux subterm =
             let visitor = BindEnv.empty in
             let free_variables = visit_bl_term visitor subterm in
             let subentry = match entry.entry_desc with
               | Assert _ -> mk_assert subterm
               | Assume _ -> mk_assume subterm
               | _ -> assert false
             in
             let pointer = mk_unique_entry subentry in
             Formula_options.Logger.debug "moving %a early" Formula_pp.pp_entry subentry;
             if VarSet.is_empty free_variables then
               env.constant <- EntrySet.add pointer env.constant
              else
               VarSet.iter (add_dependency env pointer) free_variables
           in
           (* split conjunctions *)
           let rec process_bl bl = match bl.bl_term_desc with
             | BlBnop(BlAnd, a, b) -> process_bl a; process_bl b
             | _ -> process_bl_aux bl
           in
           process_bl bl; fm)
             
    fm empty
  in
  (* add assertions without free variables *)
  add_entries env.constant res
  |> check_bindings

(* Constant propagation *)

module ConstantPropagation = Make(struct

  type env_params = VarSet.t option

  type env = {
    keep: env_params;
    assertbl: Assertbl.t;
    bindenv : BindEnv.t;
  }

  let create keep = {
    keep = keep;
    assertbl= Assertbl.empty;
    bindenv = BindEnv.empty;
  }

  let get_assertbl env bl = Assertbl.get env.assertbl bl
  let set_assertbl env bl = {env with assertbl = Assertbl.set env.assertbl bl; }

  let keep_def env df =
    match df.def_desc with
    | BlDef (_,_,bl) ->
      BindEnv.is_bl_cst env.bindenv bl = None &&
      BindEnv.is_bl_var env.bindenv bl = None
    | BvDef (_,_,bv) ->
      BindEnv.is_bv_cst env.bindenv bv = None &&
      BindEnv.is_bv_var env.bindenv bv = None
    | AxDef (_,_,_) -> true

  let filter_defs env dfs = List.filter (keep_def env) dfs

  let do_bindenv f env x = { env with bindenv = f env.bindenv x; }
  let fold_bindenv f env ls = List.fold_left (fun env x -> 
      { env with bindenv = f env.bindenv x; }) env ls

  let bind_assert env bl =
    match bl.bl_term_desc with
    | BlComp (BlEqual,bl1,bl2) ->
      (match BindEnv.is_bl_var env.bindenv bl1, BindEnv.is_bl_var env.bindenv bl2 with
       | None, None | Some _, Some _ -> env
       | Some v, None -> do_bindenv BindEnv.def env (mk_bl_def v [] bl2)
       | None, Some v -> do_bindenv BindEnv.def env (mk_bl_def v [] bl1))
    | BvComp (BvEqual,bv1,bv2) ->
      (match BindEnv.is_bv_var env.bindenv bv1, BindEnv.is_bv_var env.bindenv bv2 with
       | None, None | Some _, Some _ -> env
       | Some v, None -> do_bindenv BindEnv.def env (mk_bv_def v [] bv2)
       | None, Some v -> do_bindenv BindEnv.def env (mk_bv_def v [] bv1))
    | _ -> env

  let visit_list : 'env 'a 'b.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> mk_bl_term (visit_bl_term env bl)
    | BvTerm bv -> mk_bv_term (visit_bv_term env bv)
    | AxTerm ax -> mk_ax_term (visit_ax_term env ax)

  and visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse -> mk_bl_false

    | BlFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let bl = mk_bl_fun v ls in
      (match BindEnv.is_bl_var env.bindenv bl with
       | Some v -> mk_bl_fun v ls
       | None ->
         match BindEnv.is_bl_cst env.bindenv bl with
         | Some bool -> if bool then mk_bl_true else mk_bl_false
         | None -> bl)

    | BlLet (bn,bl) ->
      let bn = visit_list visit_def env bn in
      let env = fold_bindenv BindEnv.def env bn in
      let bl = visit_bl_term env bl in
      let bn' = filter_defs env bn in
      mk_bl_let bn' bl

    | BlUnop (u,bl) ->
      let bl = visit_bl_term env bl in
      mk_bl_unop u bl

    | BlBnop (b,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2

    | BlComp (c,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2

    | BvComp (c,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2

    | AxComp (c,ax1,ax2) ->
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2

    | BlIte (bl,bl1,bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2

  and visit_bv_term env bv =
    visit_bv_term_desc env bv.bv_term_desc

  and visit_bv_term_desc env = function
    | BvCst bv -> mk_bv_cst bv

    | BvFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let bv = mk_bv_fun v ls in
      (match BindEnv.is_bv_var env.bindenv bv with
       | Some v -> mk_bv_fun v ls
       | None ->
         match BindEnv.is_bv_cst env.bindenv bv with
         | Some bv -> mk_bv_cst bv
         | None -> bv)

    | BvLet (bn,bv) ->
      let bn = visit_list visit_def env bn in
      let env = fold_bindenv BindEnv.def env bn in
      let bv = visit_bv_term env bv in
      let bn' = filter_defs env bn in
      mk_bv_let bn' bv

    | BvUnop (u,bv) ->
      let bv = visit_bv_term env bv in
      mk_bv_unop u bv

    | BvBnop (b,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2

    | BvIte (bl,bv1,bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2

    | Select (n,ax,bv) ->
      let ax = visit_ax_term env ax in
      let bv = visit_bv_term env bv in
      mk_select n ax bv

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      let ax = mk_ax_fun v ls in
      (match BindEnv.is_ax_var env.bindenv ax with
       | Some v -> mk_ax_fun v ls
       | None -> ax)

    | AxLet (bn,ax) ->
      let bn = visit_list visit_def env bn in
      let env = fold_bindenv BindEnv.def env bn in
      let ax = visit_ax_term env ax in
      let bn' = filter_defs env bn in
      mk_ax_let bn' ax

    | AxIte (bl,ax1,ax2) ->
      let bl = visit_bl_term env bl in
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_ite bl ax1 ax2

    | Store (n,ax,bv1,bv2) ->
      let ax = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_store n ax bv1 bv2

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      let env = fold_bindenv BindEnv.decl env ls in
      let bl = visit_bl_term env bl in
      mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      let env = fold_bindenv BindEnv.decl env ls in
      let bv = visit_bv_term env bv in
      mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      let env = fold_bindenv BindEnv.decl env ls in
      let ax = visit_ax_term env ax in
      mk_ax_def v ls ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      let env = do_bindenv BindEnv.decl env dc in
      env, mk_declare dc
    | Define df ->
      let df = visit_def env df in
      let env = do_bindenv BindEnv.def env df in
      env, mk_define df
    | Assert bl ->
      let bl = visit_bl_term env bl in
      let env = bind_assert env bl in
      env, mk_assert bl
    | Assume bl ->
      let bl = visit_bl_term env bl in
      let env = bind_assert env bl in
      env, mk_assume bl
    | Comment s -> env, mk_comment s

  let process_entry env entry =
    let env, entry = visit_entry env entry in
    match entry.entry_desc with
    | Declare _ | Comment _ -> env, Some entry
    | Define df ->
      begin
        match env.keep with
        | Some(set) when (keep_def env df ||
                          match df.def_desc with
                          | BlDef (v,_,_) -> VarSet.mem (BlVar v) set
                          | BvDef (v,_,_) -> VarSet.mem (BvVar v) set
                          | AxDef (_,_,_) -> true) -> env, Some(entry)
        | None -> env, Some(entry)
        | _ -> env, None (* FIXME: probably should propagate an env without the def *)
      end
    | Assert bl ->
      if get_assertbl env bl then env, None
      else set_assertbl env bl, Some entry
    | Assume bl ->
      set_assertbl env bl, Some(entry)
end)

module FunctionInversion =
struct
  (** result for rewriting f(var)=other_hand_side:
   * (declare-fun old_var)
   * (define-fun var synthetized_term)
   * remplacer l'égalité par condition
   * *)
  type rewrite_result = {
    orig_value: bv_term; (** le terme f(old_var), avec toutes les variables inlinées *)
    condition: bl_term;
    synthetised_term: bv_term;
  }
  type term_env = 
    Unsolvable | Universal of bv_term | Solvable of {
      unknown: bv_var;
      (** only controlled var of this term *)

      rewrite : decl:bv_term -> other_hand_side:bv_term -> rewrite_result;
      (** rewrite f(unknown) = other_hand_size, where decl is a new declaration
       * of a same sorted variable as unknown
       * *)
  }
  let base_for_var var =
    let name = "_" ^ (bv_var_name var) ^ "_synthesis_base" in
    bv_var name (var.bv_size)

  [@@@warning "-32"]
  let pp ~bvsize ppf = function
    | Unsolvable -> Format.fprintf ppf "unsolvable"
    | Universal t -> Format.fprintf ppf "universal(%a)" Formula_pp.pp_bv_term t
    | Solvable { unknown; rewrite } ->
      let other_hand_side = mk_bv_var (bv_var "other_hand_side" bvsize) in
      let decl = mk_bv_var (base_for_var unknown) in
      let { orig_value; synthetised_term; condition } = rewrite ~decl ~other_hand_side in
      Format.fprintf ppf "solvable(unknown=%a, solve %a = %a to %a if %a)"
        Formula_pp.pp_bv_var unknown
        Formula_pp.pp_bv_term orig_value
        Formula_pp.pp_bv_term other_hand_side
        Formula_pp.pp_bv_term synthetised_term
        Formula_pp.pp_bl_term condition

  type env = {
    is_controlled: var -> bool;
    is_param: var -> bool;
    rewrite : (bl_var * bv_term) list BvVarMap.t;
    (** all synthesized terms for these decls along with the variable which is true when we select this one *)
    
    bindings: term_env BvVarMap.t;
    (** bv var bindings *)
  }

  let selector = ref 1

  let rec cut_at pred = function
    | hd::tl when pred hd -> Some(hd, tl)
    | _::tl -> cut_at pred tl
    | [] -> None


  (** returns an env where this possible value for unknow is registered and
   * guaranteed to be used when the returned boolean is true *)
  let add_synthethised_term unknown term env =
    let old = match BvVarMap.find unknown env.rewrite with
      | x -> x
      | exception Not_found -> []
    in
    let env, svar, old = match cut_at (fun (_s, t) -> t = term) old with
      | Some((s, _), tl) -> env, s, tl
      | None -> 
        let i = !selector in
        selector := i + 1;
        let svar = bl_var ("_selector_" ^ (string_of_int i)) in
        let rewrite = BvVarMap.add unknown ((svar, term)::old) env.rewrite in
        { env with rewrite }, svar, old
    in
    let cond = List.fold_left (fun acc (v, _) -> mk_bl_and (mk_bl_not (mk_bl_var v)) acc) (mk_bl_var svar) old in
    env, cond

  let unimplemented_map = Basic_types.String.Htbl.create 7
  let unimplemented what =
    if not (Basic_types.String.Htbl.mem unimplemented_map what) then
      begin
        Formula_options.Logger.warning "unimplemented solving for %s" what;
        Basic_types.String.Htbl.add unimplemented_map what ()
      end
    ;
    Unsolvable

  let do_orig (f: bv_term -> bv_term) rewrite = fun ~decl ~other_hand_side ->
    let {orig_value; condition; synthetised_term} = rewrite ~decl ~other_hand_side in
    { orig_value = f orig_value; condition; synthetised_term }

  let do_ohs (f: bv_term -> bv_term) rewrite = fun ~decl ~other_hand_side ->
    rewrite ~decl ~other_hand_side:(f other_hand_side)

  let rec visit_bv_term env bvterm = match bvterm.bv_term_desc with
    | BvCst _ -> Universal bvterm
    | BvFun (var, []) -> begin
      match BvVarMap.find_opt var env.bindings with
      | Some(term) -> term
      | None -> (* declared variable *)
      if env.is_controlled (BvVar var) then
        let rewrite ~decl ~other_hand_side =
          let condition = mk_bl_true in
          let synthetised_term = other_hand_side in
          let orig_value = decl in
          { condition; synthetised_term; orig_value }
        in
        Solvable { unknown = var; rewrite; }
      else if env.is_param (BvVar var) then
        Universal bvterm
      else
      Unsolvable
    end
    | BvFun (_, _::_) -> Unsolvable
    | BvLet _ -> unimplemented "bvlet"
    | BvUnop (op,bv) -> 
      let res = visit_bv_term env bv in begin
      match res with
      | Unsolvable -> Unsolvable
      | Universal x -> Universal (mk_bv_unop op x)
      | Solvable { unknown; rewrite } -> begin
        match op with
          | Formula.BvNeg
          | Formula.BvNot -> Solvable {
              unknown;
              rewrite = do_orig (mk_bv_unop op) (do_ohs (mk_bv_unop op) rewrite)
            }
        | Formula.BvRepeat _ -> Unsolvable
        | Formula.BvZeroExtend n -> 
          let size = bv_size bv in
          let rewrite ~decl ~other_hand_side =
            let higher = mk_bv_extract Interval.{hi = size + n - 1; lo=size; } other_hand_side in
            let lower = mk_bv_extract Interval.{hi = size - 1; lo=0; } other_hand_side in
            let condition = mk_bv_equal higher (mk_bv_zeros n) in
            let res = rewrite ~decl ~other_hand_side:lower in
            let condition = mk_bl_and res.condition condition in
            let synthetised_term = res.synthetised_term in
            let orig_value = mk_bv_unop op bv in
            { orig_value; synthetised_term; condition }
          in
          Solvable { unknown; rewrite }
        | Formula.BvSignExtend _ -> unimplemented "bvsignextend"
        | Formula.BvRotateLeft _ -> unimplemented "bvrotateleft"
        | Formula.BvRotateRight _ -> unimplemented "bvrotateright"
        | Formula.BvExtract Interval.{hi; lo;} -> 
          let size = bv_size bv in
          let maybe_concat a b = match a, b with
            | Some x, Some y -> Some (mk_bv_concat x y)
            | Some x, None | None, Some x -> Some x
            | None, None -> None
          in
          let rewrite ~decl ~other_hand_side =
            let res = rewrite ~decl ~other_hand_side in
            let higher = if hi = size - 1
              then None
              else Some(mk_bv_extract Interval.{hi = size - 1; lo=hi+1; } res.orig_value)
            in
            let lower = if lo = 0
              then None
              else Some(mk_bv_extract Interval.{hi = lo- 1; lo=0; } res.orig_value)
            in
            let synthetised_term = Utils.unsafe_get_opt (maybe_concat higher (maybe_concat (Some res.synthetised_term) lower)) in
            let orig_value = mk_bv_extract Interval.{hi; lo} res.orig_value in
            { condition = res.condition; synthetised_term; orig_value }
          in
          Solvable { unknown; rewrite }
      end
    end
    | BvBnop (op,bv1,bv2) ->
      let lhs = visit_bv_term env bv1 in
      let rhs = visit_bv_term env bv2 in
      begin match lhs, rhs with
        | Unsolvable, _ | _, Unsolvable | Solvable _, Solvable _ -> Unsolvable
        | Universal t1, Universal t2 -> Universal (mk_bv_bnop op t1 t2)
        | Solvable {unknown; rewrite}, Universal t ->
          (match op with
           | Formula.BvConcat -> unimplemented "bvconcat"
           | Formula.BvAnd
           | Formula.BvNand
           | Formula.BvOr
           | Formula.BvNor -> Unsolvable
           | Formula.BvXor -> Solvable { unknown; rewrite = do_ohs (mk_bv_xor t) (do_orig (mk_bv_xor t) rewrite) }
           | Formula.BvXnor -> Solvable { unknown; rewrite = do_ohs (fun ohs -> mk_bv_xor t (mk_bv_not ohs)) (do_orig (mk_bv_xnor t) rewrite) }
           | Formula.BvCmp -> unimplemented "bvcmp"
           | Formula.BvAdd -> Solvable { unknown; rewrite = do_ohs (fun ohs -> mk_bv_sub ohs t) (do_orig (mk_bv_add t) rewrite) }
           | Formula.BvSub -> Solvable { unknown; rewrite = do_ohs (mk_bv_add t) (do_orig (fun v -> mk_bv_sub v t) rewrite) }
           | Formula.BvMul -> Unsolvable
           | Formula.BvUdiv -> Unsolvable
           | Formula.BvSdiv -> Unsolvable
           | Formula.BvUrem -> Unsolvable
           | Formula.BvSrem -> Unsolvable
           | Formula.BvSmod -> Unsolvable
           | Formula.BvShl -> Unsolvable
           | Formula.BvAshr -> Unsolvable
           | Formula.BvLshr -> Unsolvable
          )
        | Universal t, Solvable {unknown; rewrite} ->
          (match op with
           | Formula.BvConcat -> unimplemented "bvconcat"
           | Formula.BvAnd
           | Formula.BvNand
           | Formula.BvOr
           | Formula.BvNor -> Unsolvable
           | Formula.BvXor -> Solvable { unknown; rewrite = do_ohs (mk_bv_xor t) (do_orig (mk_bv_xor t) rewrite) }
           | Formula.BvXnor -> Solvable { unknown; rewrite = do_ohs (fun ohs -> mk_bv_xor t (mk_bv_not ohs)) (do_orig (mk_bv_xnor t) rewrite) }
           | Formula.BvCmp -> unimplemented "bvcmp"
           | Formula.BvAdd -> Solvable { unknown; rewrite = do_ohs (fun ohs -> mk_bv_sub ohs t) (do_orig (mk_bv_add t) rewrite) }
           | Formula.BvSub -> Solvable { unknown; rewrite = do_ohs (mk_bv_sub t) (do_orig (mk_bv_sub t) rewrite) }
           | Formula.BvMul -> Unsolvable
           | Formula.BvUdiv -> Unsolvable
           | Formula.BvSdiv -> Unsolvable
           | Formula.BvUrem -> Unsolvable
           | Formula.BvSrem -> Unsolvable
           | Formula.BvSmod -> Unsolvable
           | Formula.BvShl -> Unsolvable
           | Formula.BvAshr -> Unsolvable
           | Formula.BvLshr -> Unsolvable
          )
      end
    | BvIte (_bl,_bv1,_bv2) -> unimplemented "bvite"
    | Select _ -> Unsolvable


  and visit_def env def = match def.def_desc with
    | BlDef (v,ls,bl) ->
      let env, bl = visit_bl_term env bl in
      env, mk_bl_def v ls bl
    | BvDef (v,_ls,bv) ->
      let t = visit_bv_term env bv in
      { env with bindings = BvVarMap.add v t env.bindings }, def
    | AxDef _ -> env, def


  and visit_entry env en = match en.entry_desc with
    | Declare _ -> env, en
    | Define df ->
      let env, def = visit_def env df in
      env, mk_define def
    | Assume _ -> env, en
    | Assert bl ->
      let env, bl = visit_bl_term env bl in
      env, mk_assert bl
    | Comment _ -> env, en
  
  and visit_bl_term env bl = match bl.bl_term_desc with
    | BlTrue
    | BlFalse
    | BlFun _ -> env, bl

    | BlLet (_bn,_bl) -> env, bl

    | BlUnop (op,bl) ->
      let env, bl = visit_bl_term env bl in
      env, mk_bl_unop op bl
    | BlBnop (op,bl1,bl2) ->
      let env, bl1 = visit_bl_term env bl1 in
      let env, bl2 = visit_bl_term env bl2 in
      env, mk_bl_bnop op bl1 bl2
    | BlComp (op,bl1,bl2) ->
      let env, bl1 = visit_bl_term env bl1 in
      let env, bl2 = visit_bl_term env bl2 in
      env, mk_bl_comp op bl1 bl2
    | BvComp (op,bv1,bv2) ->
      (match op with
       | Formula.BvEqual ->
         let t1 = visit_bv_term env bv1 in
         let t2 = visit_bv_term env bv2 in
         begin match t1, t2 with
           | Unsolvable, _ | _, Unsolvable | Universal _, Universal _| Solvable _ , Solvable _ -> env, bl
           | Solvable { unknown; rewrite }, Universal other_hand_side | Universal other_hand_side, Solvable { unknown; rewrite } ->
             let new_var = base_for_var unknown in
             let decl = mk_bv_var new_var in
             let res = rewrite ~decl ~other_hand_side in
             let env, condition = add_synthethised_term unknown res.synthetised_term env in
             env, mk_bl_ite condition res.condition bl
         end
       | Formula.BvDistinct -> (* FIXME *) env, bl
       | Formula.BvUlt
       | Formula.BvUle
       | Formula.BvUgt
       | Formula.BvUge
       | Formula.BvSlt
       | Formula.BvSle
       | Formula.BvSgt
       | Formula.BvSge -> env, bl)
    | AxComp _ -> env, bl
    | BlIte (bl,bl1,bl2) ->
      let env, bl = visit_bl_term env bl in
      let env, bl1 = visit_bl_term env bl1 in
      let env, bl2 = visit_bl_term env bl2 in
      env, mk_bl_ite bl bl1 bl2

end


let synthetise_solutions_with_function_inversion ~complete is_param is_controlled fm =
  let env = FunctionInversion.{
    bindings = BvVarMap.empty;
    rewrite = BvVarMap.empty;
    is_controlled;
    is_param;
  } in
  let env, fm = fold_forward (fun entry (env, fm) ->
      let env, entry = FunctionInversion.visit_entry env entry in
      env, push_back entry fm)
      fm (env, empty)
  in
  let fm = fold_forward (fun entry fm -> match entry.entry_desc with
      | Declare d -> begin match d.decl_desc with
          | BvDecl (v, []) ->
            begin match BvVarMap.find v env.FunctionInversion.rewrite with
              | terms ->
                let new_var = FunctionInversion.base_for_var v in
                let terms, base_body_def, fm = if complete then
                    terms, mk_bv_var new_var, fm
                  else
                    let s, t = List.hd terms in
                    let fm = push_back_define (mk_bl_def s [] mk_bl_true) fm in
                    List.tl terms, t, fm
                in
                let new_decl = mk_declare (mk_bv_decl new_var []) in
                let body = List.fold_left (fun t (svar, t') ->
                    mk_bv_ite (mk_bl_var svar) t' t) base_body_def terms
                in
                let fm = push_back_define (mk_bv_def v [] body) fm in
                (* push the declaration for all selectors *)
                let fm = List.fold_left (fun fm (svar, _) ->
                    push_back_declare (mk_bl_decl svar []) fm) fm terms
                in
                let fm = push_back new_decl fm in
                fm
              | exception Not_found -> push_back entry fm
            end
          | _ -> push_back entry fm
        end
      | _ -> push_back entry fm) fm empty
  in
  (* move declarations up, because our new definition might depend on the
   * declaration of the unknown *)
  let declarations = fold_forward (fun entry fm -> match entry.entry_desc with
      | Declare _ -> push_front entry fm
      | _  -> fm
    ) fm empty
  in
  let fm = fold_forward (fun entry fm -> match entry.entry_desc with
      | Declare _ -> fm
      | _  -> push_front entry fm
    ) fm declarations
  in
  let selectors = BvVarMap.fold (fun _k v acc ->
      List.fold_left (fun acc (s, _) -> Formula.VarSet.add (BlVar s) acc) acc v)
      env.FunctionInversion.rewrite Formula.VarSet.empty
  in
  let new_vars = BvVarMap.fold (fun k _v acc ->
      Formula.VarSet.add (BvVar (FunctionInversion.base_for_var k)) acc)
      env.FunctionInversion.rewrite selectors
  in
  check_bindings fm, selectors, fun v -> is_controlled v || Formula.VarSet.mem v new_vars



(* Memory control approximation *)

module SelectCollector =
struct
  type env = bv_term list


  let rec visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and env_let (env:env) (bn: def list) = List.fold_left visit_def env bn

  and visit_bl_term_desc env = function
    | BlTrue
    | BlFalse
    | BlFun _ -> env

    | BlLet (bn,bl) ->
      let res = env_let env bn in
      let res = visit_bl_term res bl in
      if res = res then
        res
      else
        Formula_options.Logger.fatal "select under a bl_let %a" Formula_pp.pp_bl_term bl

    | BlUnop (_,bl) ->
      visit_bl_term env bl
    | BlBnop (_,bl1,bl2) ->
      let env = visit_bl_term env bl1 in
      visit_bl_term env bl2
    | BlComp (_,bl1,bl2) ->
      let env = visit_bl_term env bl1 in
      visit_bl_term env bl2
    | BvComp (_,bv1,bv2) ->
      let env = visit_bv_term env bv1 in
      visit_bv_term env bv2
    | AxComp (_,ax1,ax2) ->
      let env = visit_ax_term env ax1 in
      visit_ax_term env ax2
    | BlIte (bl,bl1,bl2) ->
      let env = visit_bl_term env bl in
      let env = visit_bl_term env bl1 in
      visit_bl_term env bl2

  and visit_bv_term env bvterm = match bvterm.bv_term_desc with
    | BvCst _ -> env
    | BvFun _ -> env
    | BvLet (bn,bv) ->
      let res = env_let env bn in
      let res = visit_bv_term res bv in
      if res = res then res else
      Formula_options.Logger.fatal "select under a bv_let %a" Formula_pp.pp_bv_term bvterm

    | BvUnop (_,bv) ->
      visit_bv_term env bv

    | BvBnop (_,bv1,bv2) ->
      let env = visit_bv_term env bv1 in
      visit_bv_term env bv2
    | BvIte (bl,bv1,bv2) ->
      let env = visit_bl_term env bl in
      let env = visit_bv_term env bv1 in
      visit_bv_term env bv2
    | Select (_,ax,bv) ->
      let env = visit_ax_term env ax in
      let env = visit_bv_term env bv in
      bvterm::env

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun _ -> env

    | AxLet (bn,ax) ->
      let res = env_let env bn in
      let res = visit_ax_term res ax in
      if res = res then res else
      Formula_options.Logger.fatal "select under a ax_let %a" Formula_pp.pp_ax_term ax

    | AxIte (bl,ax1,ax2) ->
      let env = visit_ax_term env ax1 in
      let env = visit_ax_term env ax2 in
      visit_bl_term env bl
    | Store (_,ax,bv1,bv2) ->
      let env = visit_ax_term env ax in
      let env = visit_bv_term env bv1 in
      visit_bv_term env bv2

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (_v,_ls,bl) ->
      visit_bl_term env bl
    | BvDef (_v,_ls,bv) ->
      visit_bv_term env bv
    | AxDef (_v,_ls,ax) ->
      visit_ax_term env ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare _ -> env
    | Define df -> visit_def env df
    | Assert bl -> visit_bl_term env bl
    | Assume bl -> visit_bl_term env bl
    | Comment _ -> env
end

(** returns the list of all select terms in the formula *)
let collect_selects fm =
  fold_forward
      (fun entry env -> SelectCollector.visit_entry env entry)
      fm []

(** returns the list of declared arrays *)
let get_memory fm =
  fold_forward
    (fun entry acc -> match entry.entry_desc with
       | Declare d -> (match d.decl_desc with
           | AxDecl(v, []) -> v::acc
           | _ -> acc
         )
       | _ -> acc
    )
    fm []

(** returns a list of assertions A to add to a formula fm where memory is not controlled
 * such that
 * ∃a, memory: ∀x fm ʌ A
 * implies
 * ∃a: ∀x, memory, fm
 *)
let memory_control_approx memory fm =
  let rec add_for_select ax idx n acc = match n with
    | 0 -> acc
    | n ->
      let index = mk_bv_add_int idx (n-1) in
      let orig = mk_select 1 ax index in
      let initial = mk_select 1 memory index in
      let assertion = mk_bv_distinct orig initial in
      let acc = (mk_assert assertion) :: acc in
      add_for_select ax idx (n-1) acc
  in
  let selects = collect_selects fm in
  List.fold_left (fun acc select -> match select.bv_term_desc with
      | Select(n, ax, idx) -> add_for_select ax idx n acc
      | _ -> failwith "collect_selects returned non select"
    ) [] selects

module MemControl2 = struct
  type env = {
    new_mem: ax_var;
    init_mem: ax_term;
    prefix: formula;
    fm: formula;
  }

  let idx = ref 0
  let fresh_bv radix size =
    let i = !idx in
    idx := i+1;
    bv_var ("__control_" ^ radix ^ (string_of_int i)) size

  let fresh_index = fresh_bv "index"
  let fresh_memcell = fresh_bv "memcell"

  let visit_select env x = match x.bv_term_desc with
    | Select(n, _ax, idx) ->
      let index = fresh_index (bv_size idx) in
      let memcell = fresh_memcell (bv_size x) in
      let cond = mk_bv_equal (mk_bv_var index) idx in
      let fm = push_front_assume cond env.fm in
      let init_mem = mk_store n env.init_mem (mk_bv_var index) (mk_bv_var memcell) in
      let prefix = env.prefix 
                   |> push_front_declare (mk_bv_decl index [])
                   |> push_front_declare (mk_bv_decl memcell [])
      in
      { env with prefix; init_mem; fm; }
    | _ -> failwith "visit_select called on non select"

  let fresh_env memory fm =
    let new_mem = ax_var ("__controlled_memory"^(string_of_int !idx)) memory.idx_size memory.elt_size in
    let init_mem = mk_ax_var new_mem in
    let prefix = push_front_declare (mk_ax_decl new_mem []) empty in
    { new_mem; init_mem; prefix; fm }
end

(** returns formula fm' equivalent to fm but where memory is controlled, and the new name of memory
 *)
let memory_control2 memory fm =
  let open MemControl2 in
  let env = fresh_env memory fm in
  let selects = collect_selects fm in
  let env = List.fold_left visit_select env selects in
  let fm = fold_forward (fun entry fm ->
      let replacement = match entry.entry_desc with
      | Declare d -> (match d.decl_desc with
        | AxDecl(mem,[]) when mem = memory ->
          mk_define (mk_ax_def memory [] env.init_mem)
        | _ -> entry)
      | _ -> entry
      in
      push_front replacement fm) env.fm env.prefix
  in
  check_bindings fm, env.new_mem

(* Remove ax_ite usage *)

(* the core idea is
 *
 * (ite c (store a i e) b) = (store (ite c a b) i (ite c e (select b i)))
 *
 * but we must know when to stop the recursion.
 * For this we use the following invaraint:
 * no ax ite in output
 * no ax binding in let in output
 *
 * then we observe that an ax_term is either declared, or defined as (store (store (store some_variable)))
 * In the latter case, we say the base of the term is some_variable.
 *
 * Then a term has at most one base, so bases form a tree. We stop recursing when we
 * arrive on the latest common ancestor of the bases of a and b in this tree.
 * *)
module AxIte =
struct
  type infos = ax_term * ax_var (** definition, base *)

  type env = {
    bindmap  : (unit, unit, infos option) BindMap.t; (** for arrays: None if declared, or its infos if defined *)
  }

  let empty = {
    bindmap  = BindMap.empty;
  }

  (* check the invariants: only global bindings for arrays *)
  let no_ax_def bn = 
      List.for_all (fun def -> match def.def_desc with AxDef _ -> false | _ -> true) bn
  let no_ax_decl ls = 
      List.for_all (fun decl -> match decl.decl_desc with AxDecl _ -> false | _ -> true) ls

  (** the list of ancestors of a base in the base tree *)
  let ancestors env ax_var =
    let rec ancestors v acc =
      match BindMap.ax_lookup env.bindmap v with
      | None -> Formula_options.Logger.fatal "could not compute ancestors for %a: %a is not defined"
                  Formula_pp.pp_ax_var ax_var
                  Formula_pp.pp_ax_var v
      | Some(None) -> acc
      | Some(Some(_, base)) -> ancestors base (base::acc)
    in
    ancestors ax_var [ax_var]

  (** the latest common ancestor for two bases *)
  let latest_common_ancestor env ax_var1 ax_var2 =
    let a1 = ancestors env ax_var1 in
    let a2 = ancestors env ax_var2 in
    let rec luca = function
      | h::t, h'::t', acc ->
        if h <> h' then acc else luca (t, t', Some(h))
      | _, _, acc -> acc
    in
    let res = luca (a1, a2, None) in
    match res with
    | None -> Formula_options.Logger.fatal "%a and %a have no common ancestors: %a %a"
                  Formula_pp.pp_ax_var ax_var1
                  Formula_pp.pp_ax_var ax_var2
                  (Format.pp_print_list Formula_pp.pp_ax_var) a1
                  (Format.pp_print_list Formula_pp.pp_ax_var) a2
    | Some(res) -> res

  (* visit functions return the modified term, and for array ones, the base *)

  let rec visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse -> mk_bl_false
    | BlFun (v,ls) -> mk_bl_fun v ls

    | BlLet (bn,bl) ->
      assert (no_ax_def bn);
      let bl = visit_bl_term env bl in
      mk_bl_let bn bl

    | BlUnop (u,bl) ->
      let bl = visit_bl_term env bl in
      mk_bl_unop u bl
    | BlBnop (b,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2
    | BlComp (c,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2
    | BvComp (c,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2
    | AxComp (c,ax1,ax2) ->
      let ax1, _ = visit_ax_term env ax1 in
      let ax2, _ = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2
    | BlIte (bl,bl1,bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2

  and visit_bv_term env bvterm = match bvterm.bv_term_desc with
    | BvCst bv -> mk_bv_cst bv
    | BvFun (v,ls) -> mk_bv_fun v ls
    | BvLet (bn,bv) ->
      assert (no_ax_def bn);
      let bv = visit_bv_term env bv in
      mk_bv_let bn bv

    | BvUnop (u,bv) ->
      let bv = visit_bv_term env bv in
      mk_bv_unop u bv

    | BvBnop (b,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2
    | BvIte (bl,bv1,bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2
    | Select (n,ax,bv) ->
      let bv = visit_bv_term env bv in
      match ax.ax_term_desc with
      | AxIte(c, a, b) ->
        (* rewrite select (ite c a b) i -> ite c (select a i) (select b i) *)
        let ra = mk_select n a bv in
        let rb = mk_select n b bv in
        let r = mk_bv_ite c ra rb in
        visit_bv_term env r
      | _ ->
        let ax, _base = visit_ax_term env ax in
        mk_select n ax bv

  and visit_ax_term env ax: ax_term*ax_var =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) -> mk_ax_fun v ls, v

    | AxLet (bn,ax) ->
      assert (no_ax_def bn);
      let ax, base = visit_ax_term env ax in
      mk_ax_let bn ax, base

    | AxIte (bl,ax1,ax2) ->
      let ax1, _ = visit_ax_term env ax1 in
      let ax2, _ = visit_ax_term env ax2 in
      let bl = visit_bl_term env bl in
      begin
      match ax1.ax_term_desc, ax2.ax_term_desc with
      | Store(n1, axb1, idx1, value1), _ ->
 (* (ite c (store a i e) b) = (store (ite c a b) i (ite c e (select b i))) *)
        let ax, base = mk_ax_ite bl axb1 ax2 |> visit_ax_term env in
        let alt2 = mk_select n1 ax2 idx1 in
        let value = mk_bv_ite bl value1 alt2 in
        mk_store n1 ax idx1 value, base
      | _, Store(n2, axb2, idx2, value2) ->
 (* (ite c b (store a i e)) = (store (ite c b a) i (ite c (select b i) e)) *)
        let ax, base = mk_ax_ite bl ax1 axb2 |> visit_ax_term env in
        let alt1 = mk_select n2 ax1 idx2 in
        let value = mk_bv_ite bl alt1 value2 in
        mk_store n2 ax idx2 value, base
      | AxFun(v1, []), AxFun(v2, []) ->
        (* on variables, inline them and recurse if we haven't reached the common ancestor *)
        let luca = latest_common_ancestor env v1 v2 in
        (* this function inlines *)
        let get v = match BindMap.ax_lookup env.bindmap v with
          | None -> Formula_options.Logger.fatal "cannot resolve %a" Formula_pp.pp_ax_var v
          | Some(None) -> Formula_options.Logger.fatal "should not try to resolve declared %a" Formula_pp.pp_ax_var v
          | Some(Some(content, _base)) -> content
        in
        let ax1 = if luca = v1 then ax1 else get v1 in
        let ax2 = if luca = v2 then ax2 else get v2 in
        if ax1 = ax2
        then visit_ax_term env ax1
        else mk_ax_ite bl ax1 ax2 |> visit_ax_term env
      | AxLet _, _ | _, AxLet _ -> Formula_options.Logger.fatal "axlet in axite %a" Formula_pp.pp_ax_term (mk_ax_ite bl ax1 ax2)
      | AxFun(_, _::_), _ | _, AxFun(_, _::_) -> Formula_options.Logger.fatal "ax function in axite %a" Formula_pp.pp_ax_term (mk_ax_ite bl ax1 ax2)
      | AxIte _, _ | _, AxIte _ -> Formula_options.Logger.fatal "axite in axite %a" Formula_pp.pp_ax_term (mk_ax_ite bl ax1 ax2)
    end
    | Store (n,ax,bv1,bv2) ->
      let ax, base = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_store n ax bv1 bv2, base

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      assert (no_ax_decl ls);
      let bl = visit_bl_term env bl in
      mk_bl_def v ls bl, env
    | BvDef (v,ls,bv) ->
      assert (no_ax_decl ls);
      let bv = visit_bv_term env bv in
      mk_bv_def v ls bv, env
    | AxDef (v,ls,ax) ->
      assert (no_ax_decl ls);
      let ax, base = visit_ax_term env ax in
      let env = { bindmap = BindMap.ax_store env.bindmap v (Some((ax, base))); } in
      mk_ax_def v ls ax, env

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      (match dc.decl_desc with
       | AxDecl(v, []) ->
         let env = { bindmap = BindMap.ax_store env.bindmap v None; } in
         mk_declare dc, env
       | _ -> mk_declare dc, env)
    | Define df -> 
      let def, env = visit_def env df in
      mk_define def, env
    | Assert bl -> mk_assert (visit_bl_term env bl), env
    | Assume bl -> mk_assume (visit_bl_term env bl), env
    | Comment s -> mk_comment s, env
end

(** removes all array ite *)
let remove_ax_ite fm =
  let fm, _ = fold_forward
      (fun entry (fm, env) ->
         let entry, env = AxIte.visit_entry env entry in
         let fm = push_front entry fm in
         (fm, env))
      fm (empty, AxIte.empty)
  in
  check_bindings fm

(* Prune and inline *)

module PruneAndInline =
struct
  type env = {
    keep : VarSet.t;
    bl_count : int BlVarMap.t;
    bv_count : int BvVarMap.t;
    ax_count : int AxVarMap.t;
    bindenv  : BindEnv.t;
  }

  let create keep = {
    keep;
    bl_count = BlVarMap.empty;
    bv_count = BvVarMap.empty;
    ax_count = AxVarMap.empty;
    bindenv  = BindEnv.empty;
  }

  let count_bl_var env v =
    try BlVarMap.find v env.bl_count
    with Not_found -> 0    
                           
  let count_bv_var env v   =
    try BvVarMap.find v env.bv_count
    with Not_found -> 0    
                           
  let count_ax_var env v   =
    try AxVarMap.find v env.ax_count
    with Not_found -> 0

  let incr_bl_var env v =
    {env with bl_count = BlVarMap.add v (count_bl_var env v + 1) env.bl_count;}

  let incr_bv_var env v =
    {env with bv_count = BvVarMap.add v (count_bv_var env v + 1) env.bv_count;}

  let incr_ax_var env v =
    {env with ax_count = AxVarMap.add v (count_ax_var env v + 1) env.ax_count;}

  let do_bindenv f env x = { env with bindenv = f env.bindenv x; }

  let bind_assert env bl =
    match bl.bl_term_desc with
    | BlComp (BlEqual,bl1,bl2) ->
      (match BindEnv.is_bl_var env.bindenv bl1, BindEnv.is_bl_var env.bindenv bl2 with
       | None, None | Some _, Some _ -> env
       | Some v, None -> do_bindenv BindEnv.def env (mk_bl_def v [] bl2)
       | None, Some v -> do_bindenv BindEnv.def env (mk_bl_def v [] bl1))
    | BvComp (BvEqual,bv1,bv2) ->
      (match BindEnv.is_bv_var env.bindenv bv1, BindEnv.is_bv_var env.bindenv bv2 with
       | None, None | Some _, Some _ -> env
       | Some v, None -> do_bindenv BindEnv.def env (mk_bv_def v [] bv2)
       | None, Some v -> do_bindenv BindEnv.def env (mk_bv_def v [] bv1))
    | _ -> env

  (* Deciding which definitions to keep: all used declarations, and all
   * definitions of variables used at least twice (a definition used once is inlined)
   *)

  let keep_bl_decl env v =
    count_bl_var env v > 0 || VarSet.mem (BlVar v) env.keep

  let keep_bv_decl env v =
    count_bv_var env v > 0 || VarSet.mem (BvVar v) env.keep

  let keep_ax_decl env v =
    count_ax_var env v > 0 || VarSet.mem (AxVar v) env.keep

  let keep_bl_def env v =
    count_bl_var env v > 1 || VarSet.mem (BlVar v) env.keep

  let keep_bv_def env v =
    count_bv_var env v > 1 || VarSet.mem (BvVar v) env.keep

  let keep_ax_def env v =
    (* arbitrary choice: no array inlining *)
    count_ax_var env v > 0 || VarSet.mem (AxVar v) env.keep

  let filter_defs env dfs =
    List.filter
      (fun df ->
         match df.def_desc with
         | BlDef (v,_,_) -> keep_bl_def env v
         | BvDef (v,_,_) -> keep_bv_def env v
         | AxDef (v,_,_) -> keep_ax_def env v)
      dfs

  (* Visit the terms to count occurences *)

  let count_list : 'env 'a.
    ('env -> 'a -> 'env) -> 'env -> 'a list -> 'env =
    fun f env ls -> List.fold_left f env ls


  let rec count_term env tm: env =
    count_term_desc env tm.term_desc

  and count_term_desc env = function
    | BlTerm bl -> count_bl_term env bl
    | BvTerm bv -> count_bv_term env bv
    | AxTerm ax -> count_ax_term env ax

  and count_bl_term env bl =
    count_bl_term_desc env bl.bl_term_desc

  and count_bl_term_desc env = function
    | BlTrue | BlFalse -> env
    | BlFun (v,ls) ->
      let env = incr_bl_var env v in
      count_list count_term env ls
    | BlLet (bn,bl) ->
      let env = count_bl_term env bl in
      count_list count_def env bn
    | BlUnop (_,bl) ->
      count_bl_term env bl
    | BlBnop (_,bl1,bl2) ->
      let env = count_bl_term env bl1 in
      count_bl_term env bl2
    | BlComp (_,bl1,bl2) ->
      let env = count_bl_term env bl1 in
      count_bl_term env bl2
    | BvComp (_,bv1,bv2) ->
      let env = count_bv_term env bv1 in
      count_bv_term env bv2
    | AxComp (_,ax1,ax2) ->
      let env = count_ax_term env ax1 in
      count_ax_term env ax2
    | BlIte (bl,bl1,bl2) ->
      let env = count_bl_term env bl in
      let env = count_bl_term env bl1 in
      count_bl_term env bl2

  and count_bv_term env bv =
    count_bv_term_desc env bv.bv_term_desc

  and count_bv_term_desc env = function
    | BvCst _ -> env
    | BvFun (v,ls) ->
      let env = incr_bv_var env v in
      count_list count_term env ls
    | BvLet (bn,bv) ->
      let env = count_bv_term env bv in
      count_list count_def env bn
    | BvUnop (_,bv) ->
      count_bv_term env bv
    | BvBnop (_,bv1,bv2) ->
      let env = count_bv_term env bv1 in
      count_bv_term env bv2
    | BvIte (bl,bv1,bv2) ->
      let env = count_bl_term env bl in
      let env = count_bv_term env bv1 in
      count_bv_term env bv2
    | Select (_,ax,bv) ->
      let env = count_ax_term env ax in
      count_bv_term env bv

  and count_ax_term env ax =
    count_ax_term_desc env ax.ax_term_desc

  and count_ax_term_desc env = function
    | AxFun (v,ls) ->
      let env = incr_ax_var env v in
      count_list count_term env ls
    | AxLet (bn,ax) ->
      let env = count_ax_term env ax in
      count_list count_def env bn
    | AxIte (bl,ax1,ax2) ->
      let env = count_bl_term env bl in
      let env = count_ax_term env ax1 in
      count_ax_term env ax2
    | Store (_,ax,bv1,bv2) ->
      let env = count_ax_term env ax in
      let env = count_bv_term env bv1 in
      count_bv_term env bv2

  and count_def env df =
    count_def_desc env df.def_desc

  and count_def_desc env = function
    | BlDef (v,ls,bl) ->
      let env = List.fold_left (do_bindenv BindEnv.decl) env ls in
      let env = if keep_bl_decl env v then count_bl_term env bl else env in
      List.fold_left (do_bindenv BindEnv.undecl) env ls
    | BvDef (v,ls,bv) ->
      let env = List.fold_left (do_bindenv BindEnv.decl) env ls in
      let env = if keep_bv_decl env v then count_bv_term env bv else env in
      List.fold_left (do_bindenv BindEnv.undecl) env ls
    | AxDef (v,ls,ax) ->
      let env = List.fold_left (do_bindenv BindEnv.decl) env ls in
      let env = if keep_ax_decl env v then count_ax_term env ax else env in
      List.fold_left (do_bindenv BindEnv.undecl) env ls

  and count_entry env en =
    count_entry_desc env en.entry_desc

  and count_entry_desc env = function
    | Declare _ -> env
    | Define df ->
      let env = do_bindenv BindEnv.def env df in
      count_def env df
    | Assert bl | Assume bl ->
      let env= bind_assert env bl in
      count_bl_term env bl
    | Comment _ -> env


  (* Rewrite the terms after counting *)

  let visit_list : 'env 'a.
    ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list =
    fun f env ls -> List.map (f env) ls

  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> mk_bl_term (visit_bl_term env bl)
    | BvTerm bv -> mk_bv_term (visit_bv_term env bv)
    | AxTerm ax -> mk_ax_term (visit_ax_term env ax)

  and visit_bl_term env bl =
    visit_bl_term_desc env bl.bl_term_desc

  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse -> mk_bl_false

    | BlFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if VarSet.mem (BlVar v) env.keep then mk_bl_fun v ls
      else
      (* always inline variables starting with "assert".
       * This is a hack so that AI can learn from asserts after
       * ssa and prune_and_inline (because AI does not resolve bindings,
       * so (assert var) is just useless if var is never used later.)
       *)
      if count_bl_var env v = 1 || String_utils.starts_with ~substring:"assert" v.bl_name then
        match BindEnv.bl_lookup env.bindenv v with
        | BindEnv.Defined (_,[],bl) ->
          (match is_bl_var bl with
           | None -> visit_bl_term env bl
           | Some v' ->
             if v = v' then bl
             else visit_bl_term env bl)
        |_ -> mk_bl_fun v ls
      else mk_bl_fun v ls

    | BlLet (bn,bl) ->
      let bn = visit_list visit_def env bn in
      let env = List.fold_left (do_bindenv BindEnv.def) env bn in
      let bl = visit_bl_term env bl in
      let bn' = filter_defs env bn in
      mk_bl_let bn' bl

    | BlUnop (u,bl) ->
      let bl = visit_bl_term env bl in
      mk_bl_unop u bl
    | BlBnop (b,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_bnop b bl1 bl2
    | BlComp (c,bl1,bl2) ->
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_comp c bl1 bl2
    | BvComp (c,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_comp c bv1 bv2
    | AxComp (c,ax1,ax2) ->
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_comp c ax1 ax2
    | BlIte (bl,bl1,bl2) ->
      let bl = visit_bl_term env bl in
      let bl1 = visit_bl_term env bl1 in
      let bl2 = visit_bl_term env bl2 in
      mk_bl_ite bl bl1 bl2

(** returns [Some(a, b)] when the argument is [concat a b] or a variable resolving to this *)
  and bv_as_concat env bv = match bv.bv_term_desc with
    | BvFun (v, []) -> begin
        match BindEnv.bv_lookup env.bindenv v with
        | BindEnv.Defined (_,[],inner) ->
          (match is_bv_var inner with
           | None -> bv_as_concat env inner
           | Some v' ->
             if v = v' then None
             else bv_as_concat env inner)
        |_ -> None
        end
    | BvBnop(BvConcat, a, b) -> Some(a, b)
    | _ -> None

  (** tries to simplify extract of concat. If the argument can be simplified as
   * [x], returns [Some(x)], otherwise None *)
  and simplify_extract_of_concat env bv = match bv.bv_term_desc with
    | BvUnop (BvExtract itv,inner) ->
      begin match bv_as_concat env inner with
        | None -> None
        | Some(a, b) -> 
          let open Basic_types in
          match itv.lo, itv.hi with
          (* extract(concat(a, b)) = b *)
          | (0, j) when j + 1 = b.bv_term_size -> Some b
          (* extract(concat(a, b)) = extract(b) *)
          | (_, j) when j < b.bv_term_size -> Some(mk_bv_extract itv b)
          (* extract(concat(a, b)) = a *)
          | (i, j) when i = b.bv_term_size && j + 1 = a.bv_term_size + b.bv_term_size -> Some(a)
          (* extract(concat(a, b)) = extract(a) *)
          | (i, j) when i >= b.bv_term_size && j < a.bv_term_size + b.bv_term_size ->
            Some(mk_bv_extract { lo = (i - b.bv_term_size); hi = (j - b.bv_term_size) } a)
          | _ -> None
      end
    | _ -> None

  and visit_bv_term env bvterm = match bvterm.bv_term_desc with
    | BvCst bv -> mk_bv_cst bv

    | BvFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if VarSet.mem (BvVar v) env.keep then mk_bv_fun v ls
      else
        begin
          match BindEnv.bv_lookup env.bindenv v with
          | BindEnv.Defined (_,[],bv) when count_bv_var env v = 1 || should_inline bv ->
            (match is_bv_var bv with
             | None -> visit_bv_term env bv
             | Some v' ->
               if v = v' then bv
               else visit_bv_term env bv)
          |_ -> mk_bv_fun v ls
        end
    | BvLet (bn,bv) ->
      let bn = visit_list visit_def env bn in
      let env = List.fold_left (do_bindenv BindEnv.def) env bn in
      let bv = visit_bv_term env bv in
      let bn' = filter_defs env bn in
      mk_bv_let bn' bv

    | BvUnop (u,bv) ->
      (* if [u] is extract, try to simplify a potential extract of concat construct *)
      begin match simplify_extract_of_concat env bvterm with
        | None ->
          (* normal course of things *)
          let bv = visit_bv_term env bv in
          mk_bv_unop u bv
        | Some x -> (
            Formula_options.Logger.debug "Simplify extract(concat()): %a -> %a" Formula_pp.pp_bv_term (mk_bv_unop u bv) Formula_pp.pp_bv_term x;
            (* something was simplified, we can safely recurse for simplification *)
            visit_bv_term env x
          )
      end

    | BvBnop (b,bv1,bv2) ->
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_bnop b bv1 bv2
    | BvIte (bl,bv1,bv2) ->
      let bl = visit_bl_term env bl in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_bv_ite bl bv1 bv2
    | Select (n,ax,bv) ->
      let ax = visit_ax_term env ax in
      let bv = visit_bv_term env bv in
      mk_select n ax bv

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) ->
      let ls = visit_list visit_term env ls in
      if VarSet.mem (AxVar v) env.keep then mk_ax_fun v ls
      else (* Arbitrary choice: no array inlining *)
        (match BindEnv.ax_lookup env.bindenv v with
         | BindEnv.Defined (_,[],ax) ->
           (match is_ax_var ax with
            | None -> mk_ax_fun v ls
            | Some _ -> ax)
         |_ -> mk_ax_fun v ls)

    | AxLet (bn,ax) ->
      let bn = visit_list visit_def env bn in
      let env = List.fold_left (do_bindenv BindEnv.def) env bn in
      let ax = visit_ax_term env ax in
      let bn' = filter_defs env bn in
      mk_ax_let bn' ax

    | AxIte (bl,ax1,ax2) ->
      let bl = visit_bl_term env bl in
      let ax1 = visit_ax_term env ax1 in
      let ax2 = visit_ax_term env ax2 in
      mk_ax_ite bl ax1 ax2
    | Store (n,ax,bv1,bv2) ->
      let ax = visit_ax_term env ax in
      let bv1 = visit_bv_term env bv1 in
      let bv2 = visit_bv_term env bv2 in
      mk_store n ax bv1 bv2

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      let env = List.fold_left (do_bindenv BindEnv.decl) env ls in
      let bl = if keep_bl_def env v then visit_bl_term env bl else bl in
      mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      let env = List.fold_left (do_bindenv BindEnv.decl) env ls in
      let bv = if keep_bv_def env v then visit_bv_term env bv else bv in
      mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      let env = List.fold_left (do_bindenv BindEnv.decl) env ls in
      let ax = if keep_ax_def env v then visit_ax_term env ax else ax in
      mk_ax_def v ls ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      (match dc.decl_desc with
       | BlDecl (v,[]) ->
         (match BindEnv.is_bl_cst env.bindenv (mk_bl_fun v []) with
          | None -> mk_declare dc
          | Some bl ->
            let df = mk_bl_def v [] (if bl then mk_bl_true else mk_bl_false) in
            mk_define (visit_def env df))
       | BvDecl (v,[]) ->
         (match BindEnv.is_bv_cst env.bindenv (mk_bv_fun v []) with
          | None -> mk_declare dc
          | Some bv ->
            let df = mk_bv_def v [] (mk_bv_cst bv) in
            mk_define (visit_def env df))
       | _ -> mk_declare dc)

    | Define df -> mk_define (visit_def env df)
    | Assert bl -> mk_assert (visit_bl_term env bl)
    | Assume bl -> mk_assume (visit_bl_term env bl)
    | Comment s -> mk_comment s
end

(** Counts the occurences of each variables in the formula and
 * - if a variable is unused, remove its definition or declaration
 * - if a variable is defined and used once, inline it
 * - else, keep it as is.
 *)
let prune_and_inline ?(keep=VarSet.empty) fm =
  let env = PruneAndInline.create keep
      |> fold_backward (fun entry env -> PruneAndInline.count_entry env entry) fm in
  fold_backward
    (fun entry fm ->
       let entry = PruneAndInline.visit_entry env entry in
       match entry.entry_desc with
       | Declare dc ->
         if
           (match dc.decl_desc with
            | BlDecl (v,_) -> PruneAndInline.keep_bl_decl env v
            | BvDecl (v,_) -> PruneAndInline.keep_bv_decl env v
            | AxDecl (v,_) -> PruneAndInline.keep_ax_decl env v)
         then push_back entry fm
         else fm
       | Define df ->
         if
           (match df.def_desc with
            | BlDef (v,_,_) -> PruneAndInline.keep_bl_def env v
            | BvDef (v,_,_) -> PruneAndInline.keep_bv_def env v
            | AxDef (v,_,_) -> PruneAndInline.keep_ax_def env v)
         then push_back entry fm
         else fm
       | Assert _ | Assume _ | Comment _ -> push_back entry fm)
    fm empty
  |> check_bindings


(* Read over write *)
type row_params = {
  lst: int option; (** If [Some i] Use [ListEnv] with max depth [i] *)
  itv: bool; (** use interval domain, ignored if [lst < None] *)
  rbs: bool; (** rebase indices *)
  unfold: bool; (** unfold irrelevant prefix of arrays in read over write *)
}

module ReadOverWriteInner = struct

  type env_params = row_params

  module PH (K: Map.OrderedType) = struct

    module H = Map.Make(K)

    type 'a t = 'a option H.t
    let empty = H.empty

    let safe_find a x =
      try H.find x a
      with Not_found -> None

    (* let safe_find_or_set a x = *)
    (*   match H.find x a with *)
    (*   | y -> a, y *)
    (*   | exception Not_found -> (H.add x None a), None *)

    let set t idx elt = H.add idx (Some(elt)) t

    let get = safe_find
    (* module H = Hashtbl.Make(K) *)

    (* type 'a t = { mutable data : 'a data } *)
    (* and 'a data = *)
    (*   | Array of 'a option H.t *)
    (*   | Diff of K.t * 'a option * 'a t *)

    (* let empty = Array (H.empty) *)

    (* let safe_find a x = *)
    (*   try H.find a x *)
    (*   with Not_found -> H.add a x None; None *)

    (* let rec rebase t k = *)
    (*   match t.data with *)
    (*   | Array _ -> k () *)
    (*   | Diff (idx, elt, t') -> *)
    (*     rebase t' *)
    (*       (fun () -> *)
    (*          (match t'.data with *)
    (*           | Array a as p -> *)
    (*             let elt' = safe_find a idx in *)
    (*             H.replace a idx elt; *)
    (*             t.data <- p; *)
    (*             t'.data <- Diff (idx, elt', t) *)
    (*           | Diff _ -> assert false); *)
    (*          k()) *)

    (* let rebase t = rebase t (fun () -> ()) *)

    (* let set t idx elt = *)
    (*   rebase t; *)
    (*   match t.data with *)
    (*   | Array a as p -> *)
    (*     let old = safe_find a idx in *)
    (*     if old == elt then t *)
    (*     else ( *)
    (*       H.replace a idx elt; *)
    (*       let res = { data = p } in *)
    (*       t.data <- Diff (idx, old, res); *)
    (*       res) *)
    (*   | Diff _ -> assert false *)

    (* let set t idx elt = set t idx (Some elt) *)

    (* let get t = *)
    (*   match t.data with *)
    (*   | Array a -> a *)
    (*   | Diff (_,_,_) -> *)
    (*     rebase t; *)
    (*     match t.data with *)
    (*     | Array a -> a *)
    (*     | Diff (_,_,_) -> assert false *)

    (* let get t idx = safe_find (get t) idx *)

  end

  type address = {
    base : bv_term;
    delta : Bigint.t;
  }

  (** [default base] is the [address] representing [base] *)
  let default base = { base; delta = Bigint.zero_big_int }

  (** the symbolic [address] (symbol + integer offset) of the argument *)
  let get_address bv =
    match bv.bv_term_desc with
    | BvCst bv ->
      let base = mk_bv_zeros (Bitvector.size_of bv) in
      let delta = Bitvector.value_of bv in
      { base; delta }
    | BvBnop (b,bv1,bv2) ->
      (match b with
       | BvAdd ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | Some bv1, Some bv2 ->
            default (mk_bv_cst (Bitvector.add bv1 bv2))
          | Some bv1, None ->
            { base = bv2; delta = Bitvector.value_of bv1 }
          | None, Some bv2 ->
            { base = bv1; delta = Bitvector.value_of bv2 }
          | None, None -> default bv)
       | BvSub ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | Some bv1, Some bv2 ->
            default (mk_bv_cst (Bitvector.sub bv1 bv2))
          | Some bv1, None ->
            { base = mk_bv_neg bv2; delta = Bitvector.value_of bv1 }
          | None, Some bv2 ->
            { base = bv1; delta = Bigint.minus_big_int (Bitvector.value_of bv2)}
          | None, None -> default bv)
       | _ -> default bv)
    | BvFun (_,_)
    | BvLet (_,_)
    | BvUnop (_,_)
    | BvIte (_,_,_)
    | Select (_,_,_) -> default bv

  (* About "results" things: a BvSelect can have a size of several bytes,
   * so may need several smt2 selects. For the lookup to succeed, all of them
   * must be simplified. The datastrucuture chosen to model it is
   * [bv_term option array] where each cell represents one smt2 select. *)

  (** tests whether no select was resolved for this multi-byte select *)
  let no_result results = Array.for_all ((=) None) results

  (** returns the bvterm representing the multibyte select, or None
   * if one of the underlying single-byte select failed. *)
  let get_result results =
    let bv = ref (results.(0)) in
    Array.iteri
      (fun i opt -> if i > 0 then
          match !bv with
          | None -> ()
          | Some bv1 ->
            match opt with
            | None -> bv := None
            | Some bv2 -> bv := Some (mk_bv_concat bv2 bv1))
      results; !bv

(** [update_result results address m addr n bv]:
 * assuming that [results] is the [m]-byte value at [address]
 * and that [bv] is a [n]-byte bitvector written [addr],
 * update [results] and return the corresponding bitvector (see [get_results])
 *)
  let update_result results address m addr n bv =
    let size = bv.bv_term_size / n in
    let open Bigint in
    if eq_big_int address.delta addr.delta && m = n && no_result results
    then Some bv (* perfect match *)
    else begin
      let delta_m = pred_big_int (add_int_big_int m address.delta) in
      let delta_n = pred_big_int (add_int_big_int n addr.delta) in
      if lt_big_int delta_m addr.delta ||
         lt_big_int delta_n address.delta
      then None (* no interval intersection *)
      else
        let max_big = min_big_int delta_m delta_n in
        let min_big = max_big_int address.delta addr.delta in
        let base_m  = sub_big_int min_big address.delta |> int_of_big_int in
        let base_n  = sub_big_int min_big addr.delta |> int_of_big_int in
        let loop = (sub_big_int max_big min_big |> int_of_big_int) in
        for i = 0 to loop do
          if results.(i + base_m) = None then
            let lo = (base_n + i) * size in
            let hi = (base_n + i + 1) * size - 1 in
            let bv = mk_bv_extract Interval.{lo; hi} bv in
            results.(i + base_m) <- Some bv
        done;
        get_result results
    end

  let get_interval t bv =
    match is_bv_cst bv with
    | Some bv -> Interval.BitVecFlat.equal bv
    | None ->
      try BvTermHashamt.find bv t
      with Not_found -> Interval.BitVecFlat.top (bv_size bv)

  let set_interval t bv itv =
    let next = get_interval t bv
    |> Interval.BitVecFlat.inter itv in
    BvTermHashamt.add bv next t

  (** Signature for a Read over Write simplifier *)
  module type S =
  sig
    type t
    type aux
    type params
    val empty: params -> t

    (** [lookup x itv size array base] evaluates to a [bv_term] which represents [size] bytes read from [array] at index [base]. *)
    val lookup : t -> aux -> int -> ax_term -> bv_term -> bv_term

    (** [update x itv array]: if [array] is a store, record it inside [t]. *)
    val update : t -> aux -> ax_term -> t

    (** records a new binding for the specified array *)
    val alias  : t -> ax_var -> ax_term -> t

    (** undo a binding recorded with [alias] *)
    val unalias: t -> ax_var -> t
  end

  (** never simplifies any read over write *)
  module DummyEnv : S with type aux = unit and type params = unit =
  struct
    type t = unit
    type aux = unit
    type params = unit

    let empty () = ()
    let lookup () _ n ax bv = mk_select n ax bv
    let update () _ _ = ()
    let alias () _ _ = ()
    let unalias () _ = ()
  end

  let depth = ref max_int

  (** naive read over write simplification: for each select go through the entire list of stores
   * until indices are incomparable *)
  module ListEnv : S with type aux = unit and type params = unit =
  struct
    (* Each new store has its own binding, so instead of a stack of store, we
     * store a map (variable name -> the store expression) *)
    type t = ax_term AxVarMap.t
    type aux = unit
    type params = unit
    let empty () = AxVarMap.empty

(** [lookup t address m last ax bv results fuel]:
 * [ax]: indexed array
 * [m]: number of bytes to read
 * [address], [bv]: index of the select
 * [last]: a "last resort" expression for the original indexed array
 * [results]: the current result of the lookup
 * [fuel]: maximum recursion depth
 *)
    let rec lookup t address m last ax bv results fuel =
      if fuel <= 0 then
        if no_result results && is_ax_var ax <> None
        then mk_select m ax bv
        else mk_select m last bv
      else
        match ax.ax_term_desc with
        | AxFun (v,[]) ->
          let last = if no_result results then ax else last in
          let ax' = AxVarMap.find v t in
          if equal_ax_term ax ax'
          then mk_select m last bv
          else lookup t address m last ax' bv results (fuel-1)
        | AxFun (_,_)
        | AxLet (_,_)
        | AxIte (_,_,_) -> mk_select m last bv
        | Store (n,ax',bv1,bv2) ->
          let addr = get_address bv1 in
          if equal_bv_term address.base addr.base then
            match update_result results address m addr n bv2 with
            | Some bv -> bv
            | None -> lookup t address m last ax' bv results (fuel-1)
          else mk_select m last bv

    let lookup t _ n ax bv =
      let address = get_address bv in
      let results = Array.init n (fun  _ -> None) in
      lookup t address n ax ax bv results !depth

    let update t _ _ = t

    let alias t v ax =
      let ax =
        match is_ax_var ax with
        | None -> ax
        | Some v ->
          try AxVarMap.find v t
          with Not_found -> ax
      in
      AxVarMap.add v ax t

    let unalias t v = AxVarMap.remove v t
  end

  (** List of maps  (base * (offset -> value)) *)
  module MapEnv : S with type aux = unit and type params = unit =
  struct

    module PH = PH
        (struct
          type t = Bigint.t
          let compare = Bigint.compare_big_int
        end)

    type t = (ax_term * bv_term * bv_term PH.t) option AxTermHashamt.t
    type aux = unit
    type params = unit
    let empty () = AxTermHashamt.empty

    let lookup t _ n ax bv =
      match AxTermHashamt.find ax t with
      | None -> mk_select n ax bv
      | Some (array,base,map) ->
        let address = get_address bv in
        if equal_bv_term address.base base
        then
          let results =
            Array.init n
              (fun i -> PH.get map (Bigint.add_int_big_int i address.delta))
          in
          match get_result results with
          | Some bv -> bv
          | None ->
            match is_ax_var array with
            | Some _ -> mk_select n (if no_result results then array else ax) bv
            | None -> mk_select n ax bv
        else mk_select n ax bv

    let rec update map delta n bv size =
      if n < 0 then map
      else
        let lo = n * size in
        let hi = (n + 1) * size - 1 in
        let bv' = mk_bv_extract Interval.{lo; hi} bv in
        update
          (PH.set map (Bigint.add_int_big_int n delta) bv')
          delta (n-1) bv size

    let update t _ ax =
      match ax.ax_term_desc with
      | AxFun (_,[]) -> t
      | AxFun (_,_)
      | AxLet (_,_)
      | AxIte (_,_,_) -> AxTermHashamt.add ax None t
      | Store (n,array,bv1,bv2) ->
        let address = get_address bv1 in
        let array,bv,map =
          match AxTermHashamt.find array t with
          | None -> array, address.base, PH.empty
          | Some (_,bv,_ as tp) ->
            if equal_bv_term address.base bv then tp
            else array, address.base, PH.empty
        in
        AxTermHashamt.add ax
          (Some (array,bv,update map address.delta (n-1) bv2 ax.elt_term_size))
          t

    let alias t v ax =
      AxTermHashamt.add (mk_ax_var v)
        (try AxTermHashamt.find ax t
         with Not_found -> None) t

    let unalias t v = AxTermHashamt.remove (mk_ax_var v) t
  end

  module type RowDomain = sig
    (** abstract value for a bitvector *)
    type binary

    (** storage for abstract values *)
    type t

    (** returns the abstract value of a term, as stored in t *)
    val abstract : t -> bv_term -> binary

    (** if this abstract value is a singleton, return it *)
    val is_point : binary -> Bitvector.t option

    val is_empty : binary -> bool

    val inter : binary -> binary -> binary
    val union : binary -> binary -> binary

    (** [shift n a bv] an abstract value for [{ x + bv + k | 0 ≤ k < n }] where [x \in a] *)
    val shift: int -> binary -> Bigint.t -> binary

    val pp_binary: Format.formatter -> binary -> unit
  end

  module AIEnv(Domain: RowDomain) =
  struct
    module PH = PH
        (struct
          type t = Bigint.t
          let compare = Bigint.compare_big_int
        end)


    (** useful to consider an array as a set of stores *)
    type ordered_store = {
      ax: ax_term;
      index: int; (** Invariant: in a non_overlapping_stores, ax_terms with
                      greater indices contain all the stores of lower index. *)
    }

    (** the set of stores on a single symbolic basis *)
    type store_layer = {
      term: ordered_store; (* an array containing all the stores for this symbolic base *)
      abstract: Domain.binary; (** the abstract value of the set of written indices *)
      values: bv_term PH.t; (** the values associated to each index *)
    }

    (** a set of non overlapping stores *)
    type non_overlapping_stores = {
      basis: ax_term; (** The array on which everything is stored *)
      stores: store_layer BvTermHashamt.t; (* what is stored on basis *)
    }

    type params = bool
    type t = {
      row: non_overlapping_stores AxTermHashamt.t; (** maps an array to its stores *)
      aliases: ax_term AxTermHashamt.t; (** maps an array to its smallest alias *)
      unfold : bool; (** enable unfolding of irrelevant prefix of arrays *)
    }

    type aux = Domain.t

    let empty unfold = {
      row = AxTermHashamt.empty;
      aliases = AxTermHashamt.empty;
      unfold;
    }

    type result =
      | Layer of bv_term (** store/select inside one single layer *)
      | Conflict of bv_term * bv_term list (** store/select where the base index conflicts with a list of layers *)
      | Below (** reading uninitialized memory. Does not happen with stores. *)


    let pp_result fmt = function
      | Layer(base) -> Format.fprintf fmt "Layer(base=%a)" Formula_pp.pp_bv_term base
      | Conflict(base, conflicts) ->
        Format.fprintf fmt "Conflict(base %a conflicts with %a)"
          Formula_pp.pp_bv_term base
          (Format.pp_print_list ~pp_sep:Format.pp_print_space Formula_pp.pp_bv_term) conflicts
      | Below -> Format.fprintf fmt "Below"

    let pp_ordered_store ppf {ax; index} = Format.fprintf ppf "%a [%i]"
        Formula_pp.pp_ax_term ax
        index

    let pp_store_layer ppf {term; abstract; values=_} = Format.fprintf ppf "%a in %a"
        Domain.pp_binary abstract
        pp_ordered_store term

    let pp_non_overlapping_stores ppf {basis; stores} =
      Format.fprintf ppf "Store table on top of %a:@.@[%a@]"
        Formula_pp.pp_ax_term basis
        (fun ppf-> BvTermHashamt.iter (fun key layer -> Format.fprintf ppf "** %a %a"
                                           Formula_pp.pp_bv_term key pp_store_layer layer) ) stores

    (* let pp ppf t = *)
    (*   AxTermHashamt.iter (fun key elt -> Format.fprintf ppf "* %a => @[%a@]@." Formula_pp.pp_ax_term key pp_non_overlapping_stores elt) t.row *)

    (** returns the pair of stores which do overlap with the specified interval, 
     * and those which do not. *)
    let split sft stores =
      let is_disjoin layer = Domain.(is_empty (inter sft layer.abstract)) in
      BvTermHashamt.fold (fun key value (overlapping, disjoin) ->
          let add = BvTermHashamt.add key value in
          if is_disjoin value then
            (overlapping, add disjoin)
          else
            (add overlapping, disjoin))
        stores
        (BvTermHashamt.empty, BvTermHashamt.empty)

    let get_address abstract_values bv =
      let address = get_address bv in
      match Domain.is_point (Domain.abstract abstract_values address.base) with
      | None -> address
      | Some bv ->
        let open! Bitvector in
        get_address (mk_bv_cst (add bv (create address.delta (size_of bv))))

    (** returns an array containing all the stores in the argument *)
    let minimum_containing_array stores =
      BvTermHashamt.fold (fun _key value min ->
          match min with
          | Some(x) when x.index > value.term.index -> min
          | _ -> Some(value.term)
        ) stores None
    
    (** returns an expression of this array which is a variable *)
    let to_variable t ax =
      match is_ax_var ax with
      | Some(_) -> Some(ax)
      | None -> try
          let alias = AxTermHashamt.find ax t.aliases in
          match is_ax_var alias with
          | Some(_) -> (Formula_options.Logger.debug ~level:6 "found alias for %a %a" Formula_pp.pp_ax_term ax Formula_pp.pp_ax_term alias; Some(alias))
          | None -> None
        with Not_found -> None

    (** returns an array variable with index greater or equal to min_index *)
    let minimum_named_array t stores min_index = 
      if (t:t).unfold then
      BvTermHashamt.fold (fun _key value candidate ->
          Formula_options.Logger.debug ~level:10 "considering %a for %d" pp_store_layer value min_index;
          if value.term.index < min_index then candidate else
            match candidate, to_variable t value.term.ax with 
            | Some(x), Some(ax) when x.index > value.term.index ->
              Some({ value.term with ax;})
            | None, Some(ax) -> Some({ value.term with ax;})
            | _ -> candidate
        ) stores None
      else None

    let lookup_debug t abstract_values n ax bv =
      let address = get_address abstract_values bv in
      let itv = Domain.abstract abstract_values address.base in
      let sft = Domain.shift n itv address.delta in
      Formula_options.Logger.debug "LOOKUP select of %a at %a %a"
        Formula_pp.pp_ax_term ax
        Formula_pp.pp_bv_term bv
        Domain.pp_binary sft;
      let {basis;stores} = AxTermHashamt.find ax t.row in
      Formula_options.Logger.debug "%a" pp_non_overlapping_stores {basis;stores};
      let overlapping,_disjoin = split sft stores in
      let other_overlapping = BvTermHashamt.remove address.base overlapping in
      let fallback = mk_select n ax bv in
      if not (BvTermHashamt.is_empty other_overlapping) then
        begin
          (* at least one other store could correspond to this select, so we don't know
           * which one to use. However, we know that non overlapping stores are irrelevant. *)
          let result = if BvTermHashamt.cardinal overlapping = 1 then
              (* we read a new base in only one other layer.
               * Not a real conflict *)
              let base, _ = BvTermHashamt.bindings overlapping |> List.hd in
              Layer(base)
            else
              Conflict(address.base, BvTermHashamt.fold (fun key _value acc -> key::acc) other_overlapping [])
          in
          let min_containing_array = minimum_containing_array overlapping in
          Formula_options.Logger.debug ~level:5
            "Lookup fallback because of overlap; can unfold up to %a"
            (Print_utils.pp_opt pp_ordered_store) min_containing_array;
          let best = match min_containing_array with
            | None -> None
            | Some(x) -> minimum_named_array t stores x.index
          in
          match best with
          | None -> begin
              Formula_options.Logger.debug ~level:5 "no name for fallback ?";
              fallback, result
            end
          | Some(var) -> mk_select n var.ax bv, result
        end
      else
        (* no store can explain this write except maybe a store with the same symbolic base *)
        try
          (* this is the layer we must read from *)
          let layer = BvTermHashamt.find address.base stores in
          let result = Layer address.base in
          let results =
            Array.init n
              (fun i -> PH.get layer.values (Bigint.add_int_big_int i address.delta))
          in
          (* did this store cover all the bytes we want to read ? *)
          match get_result results with
          (* yes ! *)
          | Some bv -> bv, result
          (* no ! *)
          | None ->
            if Array.for_all ((=) None) results then
              begin
                (* This layer has no corresponding store, and no other layer
                 * can explain this write at all, so read the base (symbolic)
                 * array *)
                Formula_options.Logger.debug ~level:5 "Lookup down to base array because no bytes were found in the only layer";
                mk_select n basis bv, Below
              end else begin
                (* this layer contains relevant bytes, but not all of them *)
                Formula_options.Logger.debug ~level:5 "Lookup found only some of the bytes";
                match minimum_named_array t stores layer.term.index with
                | None -> fallback, result
                | Some(var) -> mk_select n var.ax bv, result
            end
        with Not_found ->
          begin
            (* no store can explain this write at all, read the base (symbolic) array *)
            Formula_options.Logger.debug ~level:5 "Lookup down to base array because no corresponding store";
            mk_select n basis bv, Below
          end

    let lookup t abstract_values n ax bv =
      let simplified, _ = lookup_debug t abstract_values n ax bv in
      simplified

    let rec update map delta n bv size =
      if n < 0 then map
      else
        let lo = n * size in
        let hi = (n + 1) * size - 1 in
        let bv' = mk_bv_extract Interval.{lo; hi} bv in
        update
          (PH.set map (Bigint.add_int_big_int n delta) bv')
          delta (n-1) bv size

    let update_debug t abstract_values whole =
      match whole.ax_term_desc with
      | AxFun (_,_)
      | AxLet (_,_)
      | AxIte (_,_,_) -> assert false
      | Store (n,stored_on_array,index,value) ->
        let address = get_address abstract_values index in
        let itv = Domain.abstract abstract_values address.base in
        let sft = Domain.shift n itv address.delta in
        Formula_options.Logger.debug "UPDATE %a where %a in %a"
          Formula_pp.pp_ax_term whole
          Formula_pp.pp_bv_term index
          Domain.pp_binary sft;
        let {basis;stores} = AxTermHashamt.find stored_on_array t.row in
        Formula_options.Logger.debug "Old: %a" pp_non_overlapping_stores {basis;stores};
        let overlapping,disjoin = split sft stores in
        let other_overlapping = BvTermHashamt.remove address.base overlapping in
        let old_abstract, old_values =
          try
            let old = BvTermHashamt.find address.base stores in
            old.abstract, old.values
          with Not_found -> sft, PH.empty
        in
        let index = match minimum_containing_array stores with
          | Some(x) -> x.index + 1
          | None -> 0
        in
        let new_layer = { 
          term = { ax = whole; index; };
          abstract = Domain.union old_abstract sft;
          values = update old_values address.delta (n-1) value whole.elt_term_size;
        }
        in
        let new_stores = BvTermHashamt.add address.base new_layer disjoin in
        let new_basis, result = if BvTermHashamt.is_empty other_overlapping
          then basis, Layer address.base
          else
            begin
              Formula_options.Logger.debug "UPDATE CONFLICT with %a"
                pp_non_overlapping_stores  {basis; stores = other_overlapping};
              let new_basis = (Utils.unsafe_get_opt (minimum_containing_array stores)).ax in
              let result = Conflict(address.base, BvTermHashamt.fold (fun key _value acc -> key::acc) other_overlapping []) in
              new_basis, result
            end
        in
        let res = { basis = new_basis; stores = new_stores; } in
        Formula_options.Logger.debug "New: %a" pp_non_overlapping_stores res;
        { t with row = AxTermHashamt.add whole res t.row }, result

    let update t abstract_values whole =
      match whole.ax_term_desc with
      | AxFun (_,[]) -> t 
      | AxFun (_,_)
      | AxLet (_,_)
      | AxIte (_,_,_) ->
        { t with row = AxTermHashamt.add whole {basis=whole;stores=BvTermHashamt.empty} t.row }
      | Store _ ->
        let t, _ = update_debug t abstract_values whole in
        t

    let alias t v ax =
      {
        aliases = AxTermHashamt.add ax (mk_ax_var v) t.aliases;
        row = AxTermHashamt.add (mk_ax_var v)
        (try AxTermHashamt.find ax t.row
         with Not_found -> { basis = mk_ax_var v; stores = BvTermHashamt.empty })
        t.row;
        unfold = t.unfold;
      }

    let unalias _t _v = assert false
  end

  module ItvEnv : S with type aux = Interval.BitVecFlat.t BvTermHashamt.t and type params = bool = AIEnv(struct
    type binary = Interval.BitVecFlat.t
    type t = binary Formula.BvTermHashamt.t

    let abstract t bv = get_interval t bv

    let shift n itv bv =
      let rec shift_aux n itv bv acc =
        if n = 0 then acc
        else
          let lo = Bitvector.create bv (Bitvector.size_of itv.Interval.lo) in
          let lo = Bitvector.add itv.Interval.lo lo in
          let hi = Bitvector.create bv (Bitvector.size_of itv.Interval.hi) in
          let hi = Bitvector.add itv.Interval.hi hi in
          shift_aux (n-1) itv (Bigint.succ_big_int bv)
            (Interval.BitVecFlat.union acc
               (if Bitvector.ule lo hi
                then Interval.BitVecFlat.(inter (uge lo) (ule hi))
                else Interval.BitVecFlat.(union (uge lo) (ule hi))))
      in
      shift_aux n itv bv Interval.BitVecFlat.empty

    (** returns an interval containing { x + bv + k | 0 ≤ k < n } where x \in itv *)
    let shift n itv bv =
      Interval.BitVecFlat.fold
        (fun itv acc -> Interval.BitVecFlat.union acc (shift n itv bv))
        itv Interval.BitVecFlat.empty

    let pp_binary ppf a =
      Format.fprintf ppf "%s" (Interval.BitVecFlat.print Bitvector.print a)

    let is_point = Interval.BitVecFlat.is_point
    let inter = Interval.BitVecFlat.inter
    let union = Interval.BitVecFlat.union
    let is_empty = Interval.BitVecFlat.is_empty
  end)

  (** The datastructure to simplify read over write *)
  type pack =
    | Dummy of DummyEnv.t
    | List  of ListEnv.t
    | Map   of MapEnv.t
    | Itv   of ItvEnv.t

  type env = {
    pack     : pack;
    rebase   : bool;
    assertbl : Assertbl.t;
    interval : Interval.BitVecFlat.t BvTermHashamt.t;
    bindenv  : BindEnv.t;
  }

  let create params = {
    pack =
      (match params.lst with
       | None ->
         if params.itv then Itv (ItvEnv.empty params.unfold)
         else Map (MapEnv.empty ());
       | Some i ->
         if i > 0 then List (depth := i; ListEnv.empty ())
         else Dummy (DummyEnv.empty ()));
    rebase   = params.rbs;
    assertbl = Assertbl.empty;
    interval = BvTermHashamt.empty;
    bindenv  = BindEnv.empty;
  }

  let get_assertbl env bl = Assertbl.get env.assertbl bl
  let set_assertbl env bl = {env with assertbl = Assertbl.set env.assertbl bl; }

  let do_pack t f = { t with pack = f t.pack; }

  let lookup t n ax bv =
    match t.pack with
    | Dummy env -> DummyEnv.lookup env () n ax bv
    | List env -> ListEnv.lookup env () n ax bv
    | Map env -> MapEnv.lookup env () n ax bv
    | Itv env -> ItvEnv.lookup env t.interval n ax bv

  let update t ax =
    do_pack t (function
    | Dummy env -> Dummy (DummyEnv.update env () ax)
    | List env -> List (ListEnv.update env () ax)
    | Map env -> Map (MapEnv.update env () ax)
    | Itv env -> Itv (ItvEnv.update env t.interval ax)
      )

  let alias t v ax =
    do_pack t (function
    | Dummy env -> Dummy (DummyEnv.alias env v ax)
    | List env -> List (ListEnv.alias env v ax)
    | Map env -> Map (MapEnv.alias env v ax)
    | Itv env -> Itv(ItvEnv.alias env v ax)
      )

  let unalias t v =
    do_pack t (function
    | Dummy env -> Dummy (DummyEnv.unalias env v)
    | List env -> List (ListEnv.unalias env v)
    | Map env -> Map (MapEnv.unalias env v)
    | Itv env -> Itv(ItvEnv.unalias env v)
      )

  (* we need to inline linear bvs for the row not to loose track that
   * esp2+4 is the same as esp+6 when esp2 is defined as esp+2 *)
  let should_inline env bv = 
    env.rebase &&
    should_inline bv

  let do_bindenv f env x = { env with bindenv = f env.bindenv x; }

  let def env df =
    let env = do_bindenv BindEnv.def env df in
    match df.def_desc with
    | BlDef (_,_,_)  -> env
    | BvDef (v,_,bv) -> { env with interval = BvTermHashamt.add (mk_bv_var v) (get_interval env.interval bv) env.interval; }
    | AxDef (v,_,ax) -> alias env v ax

  let undef env df =
    let env = do_bindenv BindEnv.undef env df in
    match df.def_desc with
    | BlDef (_,_,_) -> env
    | BvDef (v,_,_) -> { env with interval = BvTermHashamt.remove (mk_bv_var v) env.interval; }
    | AxDef (v,_,_) -> unalias env v

  let decl env dc =
    let env = do_bindenv BindEnv.decl env dc in
    match dc.decl_desc with
    | BlDecl (_,_) -> env
    | BvDecl (_,_) -> env
    | AxDecl (v,_) -> alias env v (mk_ax_var v)

  let undecl env dc =
    let env = do_bindenv BindEnv.undecl env dc in
    match dc.decl_desc with
    | BlDecl (_,_) -> env
    | BvDecl (_,_) -> env
    | AxDecl (v,_) -> unalias env v


  let propagate_unop sz u itv =
    match u with
    | BvZeroExtend i -> Interval.BitVecFlat.zero_extend i itv
    | BvSignExtend i -> Interval.BitVecFlat.sign_extend i itv
    | BvExtract i -> Interval.BitVecFlat.extract i itv
    | _ -> Interval.BitVecFlat.top sz

  let propagate_unop u (env, bv,itv) =
    let tm = mk_bv_unop u bv in
    let itv = propagate_unop (bv_size tm) u itv in
    env, tm, itv

  let propagate_bnop sz b itv1 itv2 =
    match b with
    | BvConcat -> Interval.BitVecFlat.concat itv1 itv2
    | BvAnd -> Interval.BitVecFlat.bvand itv1 itv2
    | BvOr  -> Interval.BitVecFlat.bvor itv1 itv2
    | BvAdd -> Interval.BitVecFlat.bvadd itv1 itv2
    | BvSub -> Interval.BitVecFlat.bvsub itv1 itv2
    | _ -> Interval.BitVecFlat.top sz

  let propagate_bnop b (bv1,itv1) (bv2,itv2) =
    let tm = mk_bv_bnop b bv1 bv2 in
    let itv = propagate_bnop (bv_size tm) b itv1 itv2 in
    tm, itv


  let rec assert_interval t bl =
    match bl.bl_term_desc with
    | BlFun (v,[]) ->
      (match BindEnv.bl_lookup t.bindenv v with
       | BindEnv.Defined (_,[],bl') ->
         if not (equal_bl_term bl bl')
         then assert_interval t bl'
         else None
       | _ -> None)

    | BlBnop (BlAnd,bl1,bl2) ->
      (match assert_interval t bl1 with
       | None -> None
       | Some (bv1,t1) ->
         match assert_interval t bl2 with
         | None -> None
         | Some (bv2,t2) ->
           if equal_bv_term bv1 bv2
           then Some (bv1, Interval.BitVecFlat.inter t1 t2)
           else None)

    | BlBnop (BlOr,bl1,bl2) ->
      (match assert_interval t bl1 with
       | None -> None
       | Some (bv1,t1) ->
         match assert_interval t bl2 with
         | None -> None
         | Some (bv2,t2) ->
           if equal_bv_term bv1 bv2
           then Some (bv1, Interval.BitVecFlat.union t1 t2)
           else None)

    | BvComp (c,bv1,bv2) ->
      (match is_bv_cst bv2 with
       | None -> None
       | Some bv2 ->
         let open Interval.BitVecFlat in
         Some (bv1,
               match c with
               | BvEqual -> equal bv2
               | BvDistinct -> distinct bv2
               | BvUlt -> ult bv2
               | BvUle -> ule bv2
               | BvUgt -> ugt bv2
               | BvUge -> uge bv2
               | BvSlt -> slt bv2
               | BvSle -> sle bv2
               | BvSgt -> sgt bv2
               | BvSge -> sge bv2))
    | _ -> None

  let set_interval bv itv t = { t with interval = set_interval t.interval bv itv; }

  let assert_interval t bl =
    let open Interval.BitVecFlat in
    match assert_interval t bl with
    | None ->
      (match bl.bl_term_desc with
       | BvComp (BvEqual,bv1,bv2) ->
         let itv =
           inter
             (get_interval t.interval bv1)
             (get_interval t.interval bv2)
         in
         if is_empty itv then Formula_options.Logger.warning "Empty domain";
         set_interval bv1 itv t
         |> set_interval bv2 itv
       | _ -> t)

    | Some (bv,itv as tp) ->
      let t = set_interval bv itv t in
      (match bv.bv_term_desc with
       | BvBnop (BvAdd, bv1, bv2) ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | None, None | Some _, Some _ -> t
          | None, Some bv2 ->
            let bv,itv = propagate_bnop BvSub tp (mk_bv_cst bv2, equal bv2) in
            set_interval bv itv t
          | Some bv1, None ->
            let bv,itv = propagate_bnop BvSub tp (mk_bv_cst bv1, equal bv1) in
            set_interval bv itv t)
       | BvBnop (BvSub, bv1, bv2) ->
         (match is_bv_cst bv1, is_bv_cst bv2 with
          | None, None | Some _, Some _ -> t
          | None, Some bv2 ->
            let bv,itv = propagate_bnop BvAdd tp (mk_bv_cst bv2, equal bv2) in
            set_interval bv itv t
          | Some bv1, None ->
            let bv,itv = propagate_bnop BvSub (mk_bv_cst bv1, equal bv1) tp in
            set_interval bv itv t)
       | _ -> t)


  let rec bind_assert env bl =
    match bl.bl_term_desc with
    | BlFun (v,[]) ->
      (match BindEnv.bl_lookup env.bindenv v with
       | BindEnv.Defined (_,[],bl') ->
         if not (equal_bl_term bl bl')
         then bind_assert env bl'
         else env
       | _ -> env)
    | BlComp (BlEqual,bl1,bl2) ->
      (match BindEnv.is_bl_var env.bindenv bl1, BindEnv.is_bl_var env.bindenv bl2 with
       | None, None | Some _, Some _ -> env
       | Some v, None -> def env (mk_bl_def v [] bl2)
       | None, Some v -> def env (mk_bl_def v [] bl1))
    | BvComp (BvEqual,bv1,bv2) ->
      (match BindEnv.is_bv_var env.bindenv bv1, BindEnv.is_bv_var env.bindenv bv2 with
       | None, None | Some _, Some _ -> env
       | Some v, None -> def env (mk_bv_def v [] bv2)
       | None, Some v -> def env (mk_bv_def v [] bv1))
    | _ -> env


  let visit_list : 'env 'a 'b.
    ('env -> 'a -> 'env*'b) -> 'env -> 'a list -> 'env*('b list) =
    fun f env ls ->
    List.fold_left
      (fun (env, res) x -> let env, y = f env x in env, y::res)
      (env, []) ls

  let replace_interval env bv itv = { env with interval = BvTermHashamt.add bv itv env.interval; }
  let do_term f (a, b) = a, f b


  let rec visit_term env tm: env*term =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> do_term mk_bl_term (visit_bl_term env bl)
    | BvTerm bv ->
      let env,bv,itv = visit_bv_term env bv in
      replace_interval env bv itv, mk_bv_term bv
    | AxTerm ax -> do_term mk_ax_term (visit_ax_term env ax)

  and visit_bl_term env bl =
    let env, res = visit_bl_term_desc env bl.bl_term_desc in
    env, Term_transformation.simplify_comparison res

  and visit_bl_term_desc env: bl_term_desc -> env*bl_term = function
    | BlTrue -> env, mk_bl_true
    | BlFalse ->  env, mk_bl_false

    | BlFun (v,ls) ->
      let env, ls = visit_list visit_term env ls in
      let bl = mk_bl_fun v ls in
      env, (match BindEnv.is_bl_var env.bindenv bl with
       | Some v -> mk_bl_fun v ls
       | None ->
         match BindEnv.is_bl_cst env.bindenv bl with
         | Some bool -> if bool then mk_bl_true else mk_bl_false
         | None -> bl)

    | BlLet (bn,bl) ->
      let env, bn = visit_list visit_def env bn in
      let env = List.fold_left def env bn in
      let env, bl = visit_bl_term env bl in
      let env = List.fold_left undef env bn in
      env, mk_bl_let bn bl
    | BlUnop (u,bl) ->
      let env, bl = visit_bl_term env bl in
      env, mk_bl_unop u bl
    | BlBnop (b,bl1,bl2) ->
      let env, bl1 = visit_bl_term env bl1 in
      let env, bl2 = visit_bl_term env bl2 in
      env, mk_bl_bnop b bl1 bl2
    | BlComp (c,bl1,bl2) ->
      let env, bl1 = visit_bl_term env bl1 in
      let env, bl2 = visit_bl_term env bl2 in
      env, mk_bl_comp c bl1 bl2
    | BvComp (c,bv1,bv2) ->
      let env, bv1,itv1 = visit_bv_term env bv1 in
      let env = replace_interval env bv1 itv1 in
      let env, bv2,itv2 = visit_bv_term env bv2 in
      let env = replace_interval env bv2 itv2 in
      let res = mk_bv_comp c bv1 bv2 in
      env, (match c with
          | Formula.BvEqual when Interval.BitVecFlat.(is_empty (inter itv1 itv2)) -> mk_bl_false
          | Formula.BvDistinct when Interval.BitVecFlat.(is_empty (inter itv1 itv2)) -> mk_bl_true
          | _ -> res
        )
    | AxComp (c,ax1,ax2) ->
      let env, ax1 = visit_ax_term env ax1 in
      let env, ax2 = visit_ax_term env ax2 in
      env, mk_ax_comp c ax1 ax2
    | BlIte (bl,bl1,bl2) ->
      let env, bl = visit_bl_term env bl in
      let env, bl1 = visit_bl_term env bl1 in
      let env, bl2 = visit_bl_term env bl2 in
      env, mk_bl_ite bl bl1 bl2

  and visit_bv_term env bv: env*bv_term*'a =
    let env, bv,itv = visit_bv_term_desc env bv.bv_term_desc in
    let bv = Term_transformation.simplify_additions bv in
    match Interval.BitVecFlat.is_point itv with
    | None -> env, bv, itv
    | Some bv -> env, mk_bv_cst bv, itv

  and visit_bv_term_desc env = function
    | BvCst bv -> env, mk_bv_cst bv, Interval.BitVecFlat.equal bv
    | BvFun (v,ls) ->
      let env, ls = visit_list visit_term env ls in
      let bv =
        if ls = [] then
          match BindEnv.bv_lookup env.bindenv v with
          | BindEnv.Defined (_,[],bv) ->
            if should_inline env bv then bv
            else mk_bv_fun v ls
          | _ -> mk_bv_fun v ls
        else mk_bv_fun v ls
      in
      env, bv, get_interval env.interval bv

    | BvLet (bn,bv) ->
      let env, bn = visit_list visit_def env bn in
      let env = List.fold_left def env bn in
      let env, bv,itv = visit_bv_term env bv in
      let env = List.fold_left undef env bn in
      env, mk_bv_let bn bv, itv
    | BvUnop (u,bv) ->
      let tp = visit_bv_term env bv in
      propagate_unop u tp
    | BvBnop (b,bv1,bv2) ->
      let (env, bv, itv) = visit_bv_term env bv1 in
      let (env, bv2, itv2) = visit_bv_term env bv2 in
      let (bv3, itv3) = propagate_bnop b (bv, itv) (bv2, itv2) in
      env, bv3, itv3
    | BvIte (bl,bv1,bv2) ->
      let env, bl = visit_bl_term env bl in
      let env, bv1,itv1 = visit_bv_term env bv1 in
      let env, bv2,itv2 = visit_bv_term env bv2 in
      env, mk_bv_ite bl bv1 bv2, Interval.BitVecFlat.union itv1 itv2
    | Select (n,ax,bv) ->
      let env,ax = visit_ax_term env ax in
      let env,bv,itv = visit_bv_term env bv in
      let env = replace_interval env bv itv in
      Formula_options.Logger.debug "SELECT %i\n%s\n%s %s\n" n
        (Formula_pp.print_ax_term ax)
        (Formula_pp.print_bv_term bv)
        (Interval.BitVecFlat.print Bitvector.print itv);
      let bv = lookup env n ax bv in
      let itv = get_interval env.interval bv in
      Formula_options.Logger.debug "FOUND\n%s %s\n"
        (Formula_pp.print_bv_term bv)
        (Interval.BitVecFlat.print Bitvector.print itv);
      env, bv, itv

  and visit_ax_term env ax =
    visit_ax_term_desc env ax.ax_term_desc

  and visit_ax_term_desc env = function
    | AxFun (v,ls) ->
      let env, ls = visit_list visit_term env ls in
      let res = mk_ax_fun v ls in
      update env res, res
    | AxLet (bn,ax) ->
      let env, bn = visit_list visit_def env bn in
      let env = List.fold_left def env bn in
      let env, ax = visit_ax_term env ax in
      let env = List.fold_left undef env bn in
      let res = mk_ax_let bn ax in
      update env res, res
    | AxIte (bl,ax1,ax2) ->
      let env, bl = visit_bl_term env bl in
      let env, ax1 = visit_ax_term env ax1 in
      let env, ax2 = visit_ax_term env ax2 in
      let res = mk_ax_ite bl ax1 ax2 in
      update env res, res
    | Store (n,ax,bv1,bv2) ->
      let env, ax = visit_ax_term env ax in
      let env, bv1,itv1 = visit_bv_term env bv1 in
      let env = replace_interval env bv1 itv1 in
      let env, bv2,itv2 = visit_bv_term env bv2 in
      let env = replace_interval env bv2 itv2 in
      Formula_options.Logger.debug "STORE %i %a %a %s %a %s" n
        Formula_pp.pp_ax_term ax
        Formula_pp.pp_bv_term bv1
        (Interval.BitVecFlat.print Bitvector.print itv1)
        Formula_pp.pp_bv_term bv2
        (Interval.BitVecFlat.print Bitvector.print itv2);
      let res = mk_store n ax bv1 bv2 in
      update env res, res

  and visit_def env df =
    visit_def_desc env df.def_desc

  and visit_def_desc env = function
    | BlDef (v,ls,bl) ->
      let env = List.fold_left decl env ls in
      let env, bl = visit_bl_term env bl in
      let env = List.fold_left undecl env ls in
      env, mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      let env = List.fold_left decl env ls in
      let env,bv,itv = visit_bv_term env bv in
      let env = replace_interval env bv itv in
      let env = List.fold_left undecl env ls in
      env, mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      let env = List.fold_left decl env ls in
      let env, ax = visit_ax_term env ax in
      let env = List.fold_left undecl env ls in
      env, mk_ax_def v ls ax

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      decl env dc, mk_declare dc
    | Define df ->
      let env, df = visit_def env df in
      def env df, mk_define df
    | Assert bl ->
      let env, bl = visit_bl_term env bl in
      let env = bind_assert env bl in
      assert_interval env bl, mk_assert bl
    | Assume bl ->
      let env, bl = visit_bl_term env bl in
      let env = bind_assert env bl in
      assert_interval env bl, mk_assume bl
    | Comment c -> env, mk_comment c


  let process_entry env entry =
       let (env, entry) = visit_entry env entry in
       match entry.entry_desc with
       | Declare _ | Define _ | Comment _ -> env, Some entry
       | Assert bl ->
         if get_assertbl env bl then env, None
         else (set_assertbl env bl, Some entry)
       | Assume bl ->
         set_assertbl env bl, Some entry

end

(* Simplification by abstract interpretation, via Codex *)

module AIInner = struct
  open Codex.Ival_basis
  open Codex

  let _ = Codex_config.set_show_memory_on_exit false

  (* missing transfer functions *)
  let forward_xor a b =
    let open Quadrivalent_Lattice in
    match a, b with
    | Bottom, _ | _, Bottom -> Bottom
    | Top, _ | _, Top -> Top
    | True, True | False, False -> False
    | True, False | False, True -> True

  let forward_ite join bottom c a b = match c with
    | Quadrivalent_Lattice.True -> a
    | Quadrivalent_Lattice.False -> b
    | Quadrivalent_Lattice.Top -> join a b
    | Quadrivalent_Lattice.Bottom -> bottom
  let forward_bl_ite = Quadrivalent_Lattice.(forward_ite join bottom)
  let forward_bv_ite ~size = Binary_Lattice.(forward_ite (join ~size) bottom)

  let unimplemented_backward msg return =
    Formula_options.Logger.warning "backward %s is not implemented" msg;
    return

  (** top, but with a warning *)
  let loud_top msg size =
    Formula_options.Logger.warning "forward %s is not implemented" msg;
    Binary_Forward.bunknown ~size

  let backward_xor _a _b _c = unimplemented_backward "bl xor" (None,None)

  let backward_ite inter is_bottom c a b res =
    let a' = inter a res in
    let b' = inter b res in
    if is_bottom a' then
      Some(Quadrivalent_Lattice.False), None, Some(b')
    else if is_bottom b' then
      Some(Quadrivalent_Lattice.True), Some(a'), None
    else match c with
      | Quadrivalent_Lattice.True -> None, Some(a'), None
      | Quadrivalent_Lattice.False -> None, None, Some(b')
      | Quadrivalent_Lattice.Top | Quadrivalent_Lattice.Bottom -> None, None, None

  let backward_bl_ite =
    backward_ite Quadrivalent_Lattice.inter Quadrivalent_Lattice.is_bottom
  let backward_bv_ite ~size =
    backward_ite (Binary_Lattice.inter ~size) Binary_Lattice.is_bottom

  let forward_bneg ~size b =
    let zero = Binary_Forward.biconst ~size Z.zero in
    Binary_Forward.bisub ~size ~nsw:false ~nuw:false zero b

  let forward_bnot ~size b =
    let minus1 = Binary_Forward.biconst ~size (Z.neg Z.one) in
    (* for interval domain *)
    let res1 = Binary_Forward.bisub ~size ~nsw:false ~nuw:false minus1 b in
    (* for bitwise domain *)
    let res2 = Binary_Forward.bxor ~size minus1 b in
    Binary_Lattice.inter ~size res1 res2

  (** backward transfer function for involutive operator *)
  let backward_involutive ~size forward b res =
    let refined = forward ~size res in
    if Binary_Lattice.equal refined b
    then None
    else Some(Binary_Lattice.inter ~size refined b)
  let backward_bneg = backward_involutive forward_bneg
  let backward_bnot = backward_involutive forward_bnot

  (** backward transfer function for ~(op a b) *)
  let backward_bnot_op forward backward ~size a b res =
    let fa = forward ~size a b in
    let ra = backward_bnot ~size fa res in
    match ra with
    | None -> None, None
    | Some(res') -> backward ~size a b res'
  (* end missing transfer functions *)

  (* this state lives for the time of only one entry *)
  type visitor = {
    bl_mem: boolean BlTermHashamt.t; (** memoisation of forward abstract values for booleans in the current entry. *)

    bv_mem: binary BvTermHashamt.t; (** memoisation of forward abstract values for bitvectors in the current entry. *)
  }

  (*  bv_term option: a bv def is linear, we store it here to inline it later *)
  type bindmap = (boolean, bv_term option * Binary_Lattice.t, unit) BindMap.t

  (* for read over write with our abstract values *)
  module CodexRowDomain = struct
    type binary = { size: int; value: Binary_Lattice.t }
    type t = bindmap * visitor

    let pp_binary ppf {size; value} = Binary_Lattice.pretty ~size ppf value

    let inter {size; value} b = assert (b.size == size); {size; value = Binary_Lattice.inter ~size value b.value }
    let union {size; value} b = assert (b.size == size); {size; value = Binary_Lattice.join ~size value b.value }
    let is_point {size; value} = match binary_is_singleton ~size value with
      | None -> None
      | Some x -> Some(Bitvector.create (Bigint.of_zarith x) size)
    let is_empty x = Binary_Lattice.is_bottom x.value

    let abstract (bindmap, visitor) bv =
      let size = bv_size bv in
      let value = 
        match is_bv_cst bv with
        | Some cst -> Bitvector.value_of cst |> Bigint.to_zarith |> Binary_Lattice.singleton ~size
        | None -> begin try BvTermHashamt.find bv visitor.bv_mem with Not_found ->
          match is_bv_var bv with
          | Some var -> begin match BindMap.bv_lookup bindmap var with
              | Some (_, a) -> a
              | None -> failwith (Format.asprintf "no abstract value for %a" Formula_pp.pp_bv_term bv)
            end
          | None -> failwith (Format.asprintf "no abstract value for %a" Formula_pp.pp_bv_term bv)
          end
      in
      {size; value;}

    (** [shift n a bv] an abstract value for [{ x + bv + k | 0 ≤ k < n }] where [x \in a] *)
    let shift n {size; value} bv =
      let top = Binary_Forward.bunknown ~size in
      let n' = Binary_Lattice.singleton ~size (Z.of_int n) in
      let addend = match Binary_Backward.biule ~size n' top Quadrivalent_Lattice.False with
        | _, Some(x) -> x
        | _ -> failwith ("could not generated interval 0 to "^(string_of_int n))
      in
      let value = Binary_Forward.biadd ~size ~nsw:false ~nuw:false value addend in
      let value = Binary_Forward.biadd ~size ~nsw:false ~nuw:false value (Binary_Lattice.singleton ~size (Bigint.to_zarith bv)) in
      {size; value}
  end

  module CodexRow = ReadOverWriteInner.AIEnv(CodexRowDomain)

  (* to satisfy the signature of the functor *)
  type env_params = bool

  type env = {
    row : CodexRow.t;
    (** memoisation of forward abstract values for top-level bindings. Persistent across entries. *)
    bindmap : bindmap;
  }

  let empty_visitor = {
    bl_mem= BlTermHashamt.empty;
    bv_mem= BvTermHashamt.empty;
  }

  let create unfold = {
    row = CodexRow.empty unfold;
    bindmap = BindMap.empty;
  }

  let do_bindmap f env x = { env with bindmap = f env.bindmap x; }
  let fold_bindmap f env ls = List.fold_left (fun env x -> 
      { env with bindmap = f env.bindmap x; }) env ls

  let get_forward_bl visitor bl = try
      BlTermHashamt.find bl visitor.bl_mem
    with e -> Formula_options.Logger.fatal ~e "No forward abstract value of %a" Formula_pp.pp_bl_term bl
  let get_forward_bv visitor bv = try
      BvTermHashamt.find bv visitor.bv_mem
    with e -> Formula_options.Logger.fatal ~e "No forward abstract value of %a" Formula_pp.pp_bv_term bv

  (* learn_* functions are called when we encounter an assert,
   * to use the backward transfer functions to learn finer
   * abstract values for global variables. These are store in
   * env, which is returned by these functions.
  *)
  let rec learn_bl_term visitor env bl a: env =
    match a with
    | Some a -> 
      Formula_options.Logger.debug ~level:5 "Learn %a in %a"
        Formula_pp.pp_bl_term bl
        Quadrivalent_Lattice.pretty a;
      learn_bl_term_desc visitor env bl.bl_term_desc a
    | None -> env

  and learn_bl_term_desc visitor env bl_desc a = match bl_desc with
    | Formula.BlTrue -> env
    | Formula.BlFalse -> env
    | Formula.BlFun (v,[]) ->
      (* if it is a let binding, wrong. But let bindings are not implemented. *)
      let before = match BindMap.bl_lookup env.bindmap v with
        | Some x -> x
        | None -> Quadrivalent_Lattice.Top
      in
      let after = Quadrivalent_Lattice.inter a before in
      { env with bindmap = BindMap.bl_store env.bindmap v after; }
      (* FIXME maybe recurse ? *)
    | Formula.BlFun (_,_) -> env
    (* if you implement this, make sure let bindings which shadow global
     * variable don't mess everything up *)
    | Formula.BlLet (_,_) -> unimplemented_backward "bl let" env
    | Formula.BlUnop (u,x) ->
      let fa = get_forward_bl  visitor x in
      let ra = match u with
        | Formula.BlNot -> Boolean_Backward.not fa a
      in
      learn_bl_term visitor env x ra
    | Formula.BlBnop (b,x1,x2) ->
      let fa1 = get_forward_bl  visitor x1 in
      let fa2 = get_forward_bl  visitor x2 in
      let ra1, ra2 = match b with
        | Formula.BlImply -> unimplemented_backward "=>" (None, None)
        | Formula.BlAnd -> Boolean_Backward.(&&) fa1 fa2 a
        | Formula.BlOr -> Boolean_Backward.(||) fa1 fa2 a
        | Formula.BlXor -> backward_xor fa1 fa2 a
      in
      let env = learn_bl_term visitor env x1 ra1 in
      learn_bl_term visitor env x2 ra2
    | Formula.BlComp (b,x1,x2) ->
      let fa1 = get_forward_bl  visitor x1 in
      let fa2 = get_forward_bl  visitor x2 in
      let ra1, ra2 = match b with
        | Formula.BlEqual -> backward_xor fa1 fa2 (Boolean_Forward.not a)
        | Formula.BlDistinct -> backward_xor fa1 fa2 a
      in
      let env = learn_bl_term visitor env x1 ra1 in
      learn_bl_term visitor env x2 ra2
    | Formula.BvComp (b,x1,x2) ->
      let open Binary_Backward in
      let size = bv_size x1 in
      let fa1 = get_forward_bv visitor x1 in
      let fa2 = get_forward_bv visitor x2 in
      let flip op ~size fa1 fa2 res = let ra2, ra1 = op ~size fa2 fa1 res in ra1, ra2 in
      let ra1, ra2 = match b with
        | Formula.BvEqual -> beq ~size fa1 fa2 a
        | Formula.BvDistinct -> beq ~size fa1 fa2 (Boolean_Forward.not a)
        | Formula.BvUlt -> flip biule ~size fa1 fa2 (Boolean_Forward.not a)
        | Formula.BvUle -> biule ~size fa1 fa2 a
        | Formula.BvUgt -> biule ~size fa1 fa2 (Boolean_Forward.not a)
        | Formula.BvUge -> flip biule ~size fa1 fa2 a
        | Formula.BvSlt -> flip bisle ~size fa1 fa2 (Boolean_Forward.not a)
        | Formula.BvSle -> bisle ~size fa1 fa2 a
        | Formula.BvSgt -> bisle ~size fa1 fa2 (Boolean_Forward.not a)
        | Formula.BvSge -> flip bisle ~size fa1 fa2 a
      in
      let env = learn_bv_term visitor env x1 ra1 in
      learn_bv_term visitor env x2 ra2
    | Formula.AxComp (_,_,_) -> env
    | Formula.BlIte (c,x1,x2) ->
      let fa1 = get_forward_bl  visitor x1 in
      let fa2 = get_forward_bl  visitor x2 in
      let fa  = get_forward_bl  visitor c  in
      let ra, ra1, ra2 = backward_bl_ite fa fa1 fa2 a in
      let env = learn_bl_term visitor env x1 ra1 in
      let env = learn_bl_term visitor env x2 ra2 in
      learn_bl_term visitor env c ra

  and learn_bv_term visitor env bv a =
    let size = bv_size bv in
    match a with
    | Some a -> 
      Formula_options.Logger.debug ~level:5 "Learn %a in %a"
        Formula_pp.pp_bv_term bv
        (Binary_Lattice.pretty ~size) a;
      learn_bv_term_desc visitor env bv.bv_term_desc a
    | None -> env

  and learn_bv_term_desc visitor env bv a = match bv with
    | Formula.BvCst _ -> env
    | Formula.BvFun (v,[]) -> 
      (* if it is a let binding, this is wrong. But let bindings are not implemented *)
      let (old_inline, old_abstract) = match BindMap.bv_lookup env.bindmap v with
        | Some x -> x
        | None -> None, Binary_Forward.bunknown ~size:v.bv_size
      in
      let after = Binary_Lattice.inter ~size:v.bv_size a old_abstract in
      { env with bindmap = BindMap.bv_store env.bindmap v (old_inline, after); }
    (* FIXME maybe recurse ? *)
    | Formula.BvFun (_,_::_) -> env
    (* if you implement this, make sure let bindings which shadow global
     * variables don't mess everything up *)
    | Formula.BvLet (_,_) -> unimplemented_backward "bv let" env
    | Formula.BvUnop (u,x) ->
      let size = bv_size x in
      let open Binary_Backward in
      let fa = get_forward_bv visitor x in
      let ra = match u with
        | Formula.BvNot -> backward_bnot ~size fa a
        | Formula.BvNeg -> backward_bneg ~size fa a
        | Formula.BvRepeat _ -> None
        | Formula.BvZeroExtend n -> buext ~size:(size+n) ~oldsize:size fa a
        | Formula.BvSignExtend n -> bsext ~size:(size+n) ~oldsize:size fa a
        | Formula.BvRotateLeft _ -> None
        | Formula.BvRotateRight _ -> None
        | Formula.BvExtract Interval.{ lo; hi} -> bextract ~size:(hi-lo+1) ~oldsize:size ~index:lo fa a
      in
      learn_bv_term visitor env x ra
    | Formula.BvBnop (b,x1,x2) ->
      let size = bv_size x1 in
      let size2 = bv_size x2 in
      let open Binary_Backward in
      let fa1 = get_forward_bv visitor x1 in
      let fa2 = get_forward_bv visitor x2 in
      let ra1, ra2 = match b with
       | Formula.BvConcat -> bconcat ~size1:size ~size2 fa1 fa2 a
       | Formula.BvAnd -> band ~size fa1 fa2 a
       | Formula.BvNand -> backward_bnot_op Binary_Forward.band band ~size fa1 fa2 a
       | Formula.BvOr -> bor ~size fa1 fa2 a
       | Formula.BvNor -> backward_bnot_op Binary_Forward.bor bor ~size fa1 fa2 a
       | Formula.BvXor -> bxor ~size fa1 fa2 a
       | Formula.BvXnor -> backward_bnot_op Binary_Forward.bxor bxor ~size fa1 fa2 a
       | Formula.BvAdd -> biadd ~size ~nsw:false ~nuw:false fa1 fa2 a
       | Formula.BvSub -> bisub ~size ~nsw:false ~nuw:false fa1 fa2 a
       | Formula.BvMul -> bimul ~size ~nsw:false ~nuw:false fa1 fa2 a
       | Formula.BvUdiv -> biudiv ~size fa1 fa2 a
       | Formula.BvSdiv -> bisdiv ~size fa1 fa2 a
       | Formula.BvUrem -> unimplemented_backward "bvurem" (None, None)
       | Formula.BvSrem -> bismod ~size fa1 fa2 a
       | Formula.BvSmod -> unimplemented_backward "bvsmod" (None, None)
       | Formula.BvShl -> bshl ~size ~nsw:false ~nuw:false fa1 fa2 a
       | Formula.BvAshr -> bashr ~size fa1 fa2 a
       | Formula.BvLshr -> blshr ~size fa1 fa2 a
       | Formula.BvCmp -> 
         let forward = Binary_Forward.beq ~size fa1 fa2 in
         begin
           match bofbool ~size:1 forward a with
           | None -> None, None
           | Some(a') -> beq ~size fa1 fa2 a'
         end
      in
      let env = learn_bv_term visitor env x1 ra1 in
      learn_bv_term visitor env x2 ra2
    | Formula.BvIte (c,x1,x2) ->
      let size = bv_size x1 in
      let fa1 = get_forward_bv visitor x1 in
      let fa2 = get_forward_bv visitor x2 in
      let fc = get_forward_bl visitor c in
      let rc, ra1, ra2 = backward_bv_ite ~size fc fa1 fa2 a in
      let env = learn_bl_term visitor env c rc in
      let env = learn_bv_term visitor env x1 ra1 in
      learn_bv_term visitor env x2 ra2
    | Formula.Select _ -> env

  let bind_assert visitor env bl  =
    learn_bl_term visitor env bl (Some Quadrivalent_Lattice.True)

  (* visit_* functions are called when we encounter any term to compute its
   * abstract value. If the abstract value is a singleton, the term returned is
   * simplified accordingly.  The function uses env to know the abstract value
   * of global variables. It enriches env with that of local bindings (lets),
   * but this is not propagated to later entries.  visitor is not used
   * directly. We store the abstract value of all visited subterms there for
   * use in learn* functions.  This is wasted computation time in case we are
   * not visiting an assertion ~ this is a possible improvement if performance
   * becomes a concern.
   *)
  let rec visit_bl_term (visitor:visitor) (env:env) bl: bl_term*boolean*visitor =
    let res, a, visitor = visit_bl_term_desc visitor env bl.bl_term_desc in
    let res = match a with
    | Quadrivalent_Lattice.True -> mk_bl_true
    | Quadrivalent_Lattice.False -> mk_bl_false
    | _ -> res
    in
    let visitor = { visitor with bl_mem = BlTermHashamt.add bl a visitor.bl_mem } in
    res, a, visitor

  and visit_bl_term_desc visitor env desc: bl_term*boolean*visitor = match desc with
    | BlTrue -> mk_bl_true, Quadrivalent_Lattice.True, visitor
    | BlFalse -> mk_bl_false, Quadrivalent_Lattice.False, visitor

    | BlFun (v,[]) ->
      let bl = mk_bl_fun v [] in
      bl, (match BindMap.bl_lookup env.bindmap v with
       | Some abstract -> abstract
       | None -> Quadrivalent_Lattice.Top), visitor

    | BlFun (v,ls) ->
      let bl = mk_bl_fun v ls in
      bl, Quadrivalent_Lattice.Top, visitor

    | BlLet (bn,bl) ->
      let visitor, env = visit_defs visitor env bn in
      let bl, a, visitor = visit_bl_term visitor env bl in
      mk_bl_let bn bl, a, visitor

    | BlUnop (u,bl) ->
      let bl, a, visitor = visit_bl_term visitor env bl in
      let a = match u with
      | Formula.BlNot -> Boolean_Forward.not a
      in mk_bl_unop u bl, a, visitor

    | BlBnop (b,bl1,bl2) ->
      let bl1, a1, visitor = visit_bl_term visitor env bl1 in
      let bl2, a2, visitor = visit_bl_term visitor env bl2 in
      mk_bl_bnop b bl1 bl2, (match b with
          | Formula.BlImply -> Boolean_Forward.(||) (Boolean_Forward.not a1) a2
          | Formula.BlAnd -> Boolean_Forward.(&&) a1 a2
          | Formula.BlOr -> Boolean_Forward.(||) a1 a2
          | Formula.BlXor -> forward_xor a1 a2
        ), visitor

    | BlComp (c,bl1,bl2) ->
      let bl1, a1, visitor = visit_bl_term visitor env bl1 in
      let bl2, a2, visitor = visit_bl_term visitor env bl2 in
      mk_bl_comp c bl1 bl2,
      (match c with
       | Formula.BlEqual -> Boolean_Forward.not (forward_xor a1 a2)
       | Formula.BlDistinct -> forward_xor a1 a2
      ), visitor

    | BvComp (c,bv1,bv2) ->
      let bv1, a1, visitor = visit_bv_term visitor env bv1 in
      let bv2, a2, visitor = visit_bv_term visitor env bv2 in
      let size = bv_size bv1 in
      mk_bv_comp c bv1 bv2,
      (match c with
       | Formula.BvEqual -> Binary_Forward.beq ~size a1 a2
       | Formula.BvDistinct -> Boolean_Forward.not (Binary_Forward.beq ~size a1 a2)
       | Formula.BvUlt -> Boolean_Forward.not (Binary_Forward.biule ~size a2 a1)
       | Formula.BvUle -> Binary_Forward.biule ~size a1 a2
       | Formula.BvUgt -> Boolean_Forward.not (Binary_Forward.biule ~size a1 a2)
       | Formula.BvUge -> Binary_Forward.biule ~size a2 a1
       | Formula.BvSlt -> Boolean_Forward.not (Binary_Forward.bisle ~size a2 a1)
       | Formula.BvSle -> Binary_Forward.bisle ~size a1 a2
       | Formula.BvSgt -> Boolean_Forward.not (Binary_Forward.bisle ~size a1 a2)
       | Formula.BvSge -> Binary_Forward.bisle ~size a2 a1
      ), visitor

    | AxComp (c,ax1,ax2) ->
      (* here we drop knowledge about stores inside ax1 and ax2 by ignoring the returned env,
       * but we don't care as these stores are not accessible in this scope *)
      let ax1, visitor, _env = visit_ax_term visitor env ax1 in
      let ax2, visitor, _env = visit_ax_term visitor env ax2 in
      mk_ax_comp c ax1 ax2, Boolean_Forward.unknown ?level:None, visitor

    | BlIte (bl,bl1,bl2) ->
      let bl , a , visitor = visit_bl_term visitor env bl in
      let bl1, a1, visitor = visit_bl_term visitor env bl1 in
      let bl2, a2, visitor = visit_bl_term visitor env bl2 in
      mk_bl_ite bl bl1 bl2, forward_bl_ite a a1 a2, visitor

  and visit_bv_term visitor env bv: bv_term*binary*visitor =
    let res, a, visitor = visit_bv_term_desc visitor env bv.bv_term_desc in
    let res = Term_transformation.simplify_additions res in
    let size = bv_size bv in
    let res = match binary_is_singleton ~size a with
    | Some(z) ->
      let bigint = Bigint.of_zarith z in
      let bitvector = Bitvector.create bigint size in
      let const = mk_bv_cst bitvector in
      const
    | None -> res
    in
    Formula_options.Logger.debug ~level:4 "Evaluate %a to %a"
      Formula_pp.pp_bv_term bv
      (Binary_Lattice.pretty ~size) a;
    let visitor = { visitor with bv_mem = BvTermHashamt.add bv a visitor.bv_mem } in
    res, a, visitor

  and visit_bv_term_desc visitor env = function
    | BvCst bv ->
      let size = Bitvector.size_of bv in
      let z = Bitvector.value_of bv |> Bigint.to_zarith in
      mk_bv_cst bv, Binary_Forward.biconst ~size z, visitor

    | BvFun (v,[]) ->
      let bv = mk_bv_fun v [] in
      let size = v.bv_size in
      let res, a = match BindMap.bv_lookup env.bindmap v with
        | Some (Some inline, a) -> inline, a
        | Some (None, a) -> bv, a
        | None -> bv, Binary_Forward.bunknown ~size
      in
      res, a, visitor

    | BvFun (v,ls) ->
      let bv = mk_bv_fun v ls in
      let size = v.bv_size in
      bv, Binary_Forward.bunknown ~size, visitor

    | BvLet (bn,bv) ->
      let visitor, env = visit_defs visitor env bn in
      let bv, a, visitor = visit_bv_term visitor env bv in
      mk_bv_let bn bv, a, visitor

    | BvUnop (u,bv) ->
      let open Binary_Forward in
      let bv, a, visitor = visit_bv_term visitor env bv in
      let size = bv_size bv in
      mk_bv_unop u bv,
      (match u with
       | Formula.BvNot -> forward_bnot ~size a
       | Formula.BvNeg -> forward_bneg ~size a
       | Formula.BvRepeat n -> loud_top "bvrepeat" (size*n)
       | Formula.BvZeroExtend n -> buext ~size:(size+n) ~oldsize:size a
       | Formula.BvSignExtend n -> bsext ~size:(size+n) ~oldsize:size a
       | Formula.BvRotateLeft _ -> loud_top "rotate left" size
       | Formula.BvRotateRight _ -> loud_top "rotate right" size
       | Formula.BvExtract Interval.{ lo; hi } -> bextract ~size:(hi-lo+1) ~oldsize:size a ~index:lo
      ), visitor

    | BvBnop (b,bv1,bv2) ->
      let open Binary_Forward in
      let bv1, a1, visitor = visit_bv_term visitor env bv1 in
      let bv2, a2, visitor = visit_bv_term visitor env bv2 in
      let size = bv_size bv1 in
      let size2 = bv_size bv2 in
      let a = match b with
        | Formula.BvConcat -> bconcat ~size1:size ~size2 a1 a2
        | Formula.BvAnd -> band ~size a1 a2
        | Formula.BvNand -> forward_bnot ~size (band ~size a1 a2)
        | Formula.BvOr -> bor ~size a1 a2
        | Formula.BvNor -> forward_bnot ~size (bor ~size a1 a2)
        | Formula.BvXor -> bxor ~size a1 a2
        | Formula.BvXnor -> forward_bnot ~size (bxor ~size a1 a2)
        | Formula.BvCmp -> bofbool ~size:1 (beq ~size a1 a2)
        | Formula.BvAdd -> biadd ~size ~nsw:false ~nuw:false a1 a2
        | Formula.BvSub -> bisub ~size ~nsw:false ~nuw:false a1 a2
        | Formula.BvMul -> bimul ~size ~nsw:false ~nuw:false a1 a2
        | Formula.BvUdiv -> biudiv ~size a1 a2
        | Formula.BvSdiv -> bisdiv ~size a1 a2
        | Formula.BvUrem -> loud_top "bvurem" size
        | Formula.BvSrem -> bismod ~size a1 a2
        | Formula.BvSmod -> loud_top "bvsmod" size
        | Formula.BvShl -> bshl ~size ~nsw:false ~nuw:false a1 a2
        | Formula.BvAshr -> bashr ~size a1 a2
        | Formula.BvLshr -> blshr ~size a1 a2
      in
      let res = mk_bv_bnop b bv1 bv2 in
      (* simplify (bvand a 0xff00) where a has already the low byte unset. *)
      let res = match b, is_bv_cst bv2 with
        | Formula.BvAnd, Some(cst) | Formula.BvOr, Some(cst) ->
          let rem, modulo = congruence_for ~size a1 in
          let k = Z.trailing_zeros modulo in
          if k = 0 then res else (* nothing to gain in this case *)
          let power = Z.shift_left Z.one k in
          let rem = Z.rem rem power in
          let high_order = Bitvector.extract cst Interval.{hi = size - 1; lo = k} in
          let expected_rem, expected_high_order = match b with
            | Formula.BvAnd -> Z.zero, Bitvector.fill (size - k)
            | Formula.BvOr -> Z.(-) power Z.one, Bitvector.zeros (size - k)
            | _ -> assert false
          in
          if Z.equal expected_rem rem && Bitvector.equal high_order expected_high_order then
            bv1
          else
            res
        | _ -> res
      in
      res, a, visitor

    | BvIte (bl,bv1,bv2) ->
      let bl, a, visitor = visit_bl_term visitor env bl in
      let bv1, a1, visitor = visit_bv_term visitor env bv1 in
      let bv2, a2, visitor = visit_bv_term visitor env bv2 in
      let size = bv_size bv1 in
      mk_bv_ite bl bv1 bv2, forward_bv_ite ~size a a1 a2, visitor

    | Select (n,ax,idx) ->
      let ax, visitor, env = visit_ax_term visitor env ax in
      let simplified_idx, _, visitor = visit_bv_term visitor env idx in
      let _, _, visitor = visit_bv_term visitor env simplified_idx in
      let size = n*ax.elt_term_size in
      let simplified = CodexRow.lookup env.row (env.bindmap, visitor) n ax simplified_idx in
      Formula_options.Logger.debug "Lookup select %i %a %a => %a"
        n
        Formula_pp.pp_ax_term ax
        Formula_pp.pp_bv_term idx
        Formula_pp.pp_bv_term simplified;
      match simplified.bv_term_desc with
      | Select _ -> simplified, Binary_Forward.bunknown ~size, visitor
      | _ -> visit_bv_term visitor env simplified

  and visit_ax_term visitor env ax =
    let ax, visitor, env = visit_ax_term_desc visitor env ax.ax_term_desc in
    let env = { env with row = CodexRow.update env.row (env.bindmap, visitor) ax } in
    ax, visitor, env

  and visit_ax_term_desc visitor env = function
    | AxFun (v,ls) -> mk_ax_fun v ls, visitor, env

    | AxLet (bn,ax) ->
      let visitor, env = visit_defs visitor env bn in
      let ax, visitor, env = visit_ax_term visitor env ax in
      mk_ax_let bn ax, visitor, env

    | AxIte (bl,ax1,ax2) ->
      let bl, _, visitor = visit_bl_term visitor env bl in
      let ax1, visitor, env = visit_ax_term visitor env ax1 in
      let ax2, visitor, env = visit_ax_term visitor env ax2 in
      mk_ax_ite bl ax1 ax2, visitor, env

    | Store (n,ax,index,value) ->
      let ax, visitor, env = visit_ax_term visitor env ax in
      let index, _, visitor = visit_bv_term visitor env index in
      let _, _, visitor = visit_bv_term visitor env index in
      let value, _, visitor = visit_bv_term visitor env value in
      mk_store n ax index value, visitor, env

  and visit_def visitor env df =
    visit_def_desc visitor env df.def_desc

  and visit_def_desc (visitor:visitor) (env:env) = function
    | BlDef (v,ls,bl) ->
      let env_with_let = fold_bindmap BindMap.decl_del env ls in
      let bl, a, visitor = visit_bl_term visitor env_with_let bl in
      let env = do_bindmap (fun map -> BindMap.bl_store map v) env a in
      mk_bl_def v ls bl, env, visitor
    | BvDef (v,ls,bv) ->
      let env_with_let = fold_bindmap BindMap.decl_del env ls in
      let bv, a, visitor = visit_bv_term visitor env_with_let bv in
      let inline = if should_inline bv then Some bv else None in
      let env = do_bindmap (fun map -> BindMap.bv_store map v) env (inline, a) in
      let res = mk_bv_def v ls bv in
      Formula_options.Logger.debug ~level:4 "Evaluate (inline=%b) %a to %a"
        (inline <> None)
        Formula_pp.pp_entry (mk_define res)
        (Binary_Lattice.pretty ~size:v.bv_size) a;
      res, env, visitor
    | AxDef (v,ls,ax) ->
      let env_with_let = fold_bindmap BindMap.decl_del env ls in
      (* this environment is polluted by let bindings and the like, but the row environment has been updated *)
      let ax, visitor, env_with_row = visit_ax_term visitor env_with_let ax in
      let row = CodexRow.alias env_with_row.row v ax in
      let env = { env with row } in
      mk_ax_def v ls ax, env, visitor

  and visit_defs (visitor:visitor) (env: env) defs =
    let visit_def_env (visitor, env) def =
      let _def, env, visitor = visit_def visitor env def in
      (visitor, env)
    in
    List.fold_left visit_def_env (visitor, env) defs

  and visit_entry env en =
    visit_entry_desc env en.entry_desc

  and visit_entry_desc env = function
    | Declare dc ->
      let env = do_bindmap BindMap.decl_del env dc in
      let row = match dc.decl_desc with
        | AxDecl (v,_) -> CodexRow.alias env.row v (mk_ax_var v)
        | _ -> env.row
      in
      mk_declare dc, { env with row }, None
    | Define df ->
      let df, env, visitor = visit_def empty_visitor env df in
      mk_define df, env, Some(visitor)
    | Assert bl ->
      let simplified, a, visitor = visit_bl_term empty_visitor env bl in
      Formula_options.Logger.debug ~level:4 "Evaluate assertion %a to %a -> %a"
        Formula_pp.pp_entry (mk_assert bl)
        (Quadrivalent_Lattice.pretty) a
        Formula_pp.pp_entry (mk_assert simplified);
      let env = bind_assert visitor env bl in
      mk_assert simplified, env, Some visitor
    | Assume bl ->
      let simplified, _, visitor = visit_bl_term empty_visitor env bl in
      let env = bind_assert visitor env bl in
      mk_assume simplified, env, Some visitor
    | Comment s -> mk_comment s, env, None

  let process_entry env entry = 
    let entry, env, _visitor = visit_entry env entry in
    env, Some entry
end

module AI = Make(AIInner)

(** Ill named module. This does transformation to A normal form *)
module StaticSingleAssignment =
struct

  type env = {
    names : int Basic_types.String.Htbl.t;
    bl_htbl : bl_var BlTermHashtbl.t;
    bv_htbl : bv_var BvTermHashtbl.t;
    ax_htbl : ax_var AxTermHashtbl.t;
  }

  let create n = {
    names = Basic_types.String.Htbl.create n;
    bl_htbl = BlTermHashtbl.create n;
    bv_htbl = BvTermHashtbl.create n;
    ax_htbl = AxTermHashtbl.create n;
  }

  let do_nothing s x = s, x

  let k_identity f s x = f s x

  let rec fresh_name env name =
    let i =
      try Basic_types.String.Htbl.find env.names name
      with Not_found -> 0
    in
    Basic_types.String.Htbl.replace env.names name (i+1);
    let fresh = Printf.sprintf "%s_%i" name i in
    if Basic_types.String.Htbl.mem env.names fresh
    then fresh_name env name
    else (Basic_types.String.Htbl.add env.names fresh 0; fresh)

  let fresh_bl_var env name bl =
    let name =
      try (BlTermHashtbl.find env.bl_htbl bl).bl_name
      with Not_found -> fresh_name env name
    in
    bl_var name

  let add_bl_term env seq v bl =
    let seq =
      if BlTermHashtbl.mem env.bl_htbl bl then seq
      else
        (BlTermHashtbl.add env.bl_htbl bl v;
         push_front_define (mk_bl_def v [] bl) seq)
    in seq

  let push_front_bl_term bool (name,env) seq bl =
    if bool then seq, bl
    else
      match bl.bl_term_desc with
      | BlTrue | BlFalse | BlFun (_,_) -> seq, bl
      | BlLet (_,_) -> assert false
      | BlUnop (_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_unop") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BlBnop (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_bnop") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BlComp (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_comp") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BvComp (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bv_comp") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | AxComp (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_ax_comp") bl in
        add_bl_term env seq v bl, mk_bl_var v
      | BlIte (_,_,_) ->
        let v = fresh_bl_var env (name ^ "_bl_ite") bl in
        add_bl_term env seq v bl, mk_bl_var v

  let fresh_bv_var env name bv =
    let name =
      try (BvTermHashtbl.find env.bv_htbl bv).bv_name
      with Not_found -> fresh_name env name
    in
    bv_var name bv.bv_term_size

  let add_bv_term env seq v bv =
    let seq =
      if BvTermHashtbl.mem env.bv_htbl bv then seq
      else
        (BvTermHashtbl.add env.bv_htbl bv v;
         push_front_define (mk_bv_def v [] bv) seq)
    in seq

  let push_front_bv_term bool (name,env) seq bv =
    if bool then seq, bv
    else
      match bv.bv_term_desc with
      | BvCst _ | BvFun (_,_) -> seq, bv
      | BvLet (_,_) -> assert false
      | BvUnop (_,_) ->
        let v = fresh_bv_var env (name ^ "_bv_unop") bv in
        add_bv_term env seq v bv, mk_bv_var v
      | BvBnop (_,_,_) ->
        let v = fresh_bv_var env (name ^ "_bv_bnop") bv in
        add_bv_term env seq v bv, mk_bv_var v
      | BvIte (_,_,_) ->
        let v = fresh_bv_var env (name ^ "_bv_ite") bv in
        add_bv_term env seq v bv, mk_bv_var v
      | Select (_,_,_) ->
        let v = fresh_bv_var env (name ^ "_select") bv in
        add_bv_term env seq v bv, mk_bv_var v

  let fresh_ax_var env name ax =
    let name =
      try (AxTermHashtbl.find env.ax_htbl ax).ax_name
      with Not_found -> fresh_name env name
    in
    ax_var name ax.idx_term_size ax.elt_term_size

  let add_ax_term env seq v ax =
    let seq =
      if AxTermHashtbl.mem env.ax_htbl ax then seq
      else
        (AxTermHashtbl.add env.ax_htbl ax v;
         push_front_define (mk_ax_def v [] ax) seq)
    in seq

  let push_front_ax_term bool (name,env) seq ax =
    if bool then seq, ax
    else
      match ax.ax_term_desc with
      | AxFun (_,_) -> seq, ax
      | AxLet (_,_) -> assert false
      | AxIte (_,_,_) ->
        let v = fresh_ax_var env (name ^ "_ax_ite") ax in
        add_ax_term env seq v ax, mk_ax_var v
      | Store (_,_,_,_) ->
        let v = fresh_ax_var env (name ^ "_store") ax in
        add_ax_term env seq v ax, mk_ax_var v

  (* There is certainly a bug here, mk_.._fun should takes a term list we do not
   * have here... *)
  let push_front_define env df seq =
    match df.def_desc with
    | BlDef (v,ls,bl) ->
      let bl =
        try mk_bl_fun (BlTermHashtbl.find env.bl_htbl bl) []
        with Not_found -> bl
      in
      BlTermHashtbl.add env.bl_htbl bl v;
      push_front_define (mk_bl_def v ls bl) seq
    | BvDef (v,ls,bv) ->
      let bv =
        try mk_bv_fun (BvTermHashtbl.find env.bv_htbl bv) []
        with Not_found -> bv
      in
      BvTermHashtbl.add env.bv_htbl bv v;
      push_front_define (mk_bv_def v ls bv) seq
    | AxDef (v,ls,ax) ->
      let ax =
        try mk_ax_fun (AxTermHashtbl.find env.ax_htbl ax) []
        with Not_found -> ax
      in
      AxTermHashtbl.add env.ax_htbl ax v;
      push_front_define (mk_ax_def v ls ax) seq

  let reserve_entry env em =
    match em.entry_desc with
    | Assert _ | Assume _ | Comment _ -> ()
    | Declare dc ->
      Basic_types.String.Htbl.add env.names (decl_name dc) 0
    | Define df ->
      Basic_types.String.Htbl.add env.names (def_name df) 0

  let visit_list : 'env 'a 'b.
    ('env -> 'a -> ('env * 'b)) -> 'env -> 'a list -> 'env * 'b list =
    fun f env ls ->
      let env,acc =
        List.fold_left
          (fun (env,acc) x -> let env,x = f env x in env, x::acc)
          (env,[]) ls
      in
      env, List.rev acc

  let rec visit_term env seq tm =
    visit_term_desc env seq tm.term_desc

  and visit_term_desc env seq = function
    | BlTerm bl ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      seq, mk_bl_term bl
    | BvTerm bv ->
      let seq,bv = visit_bv_term k_identity false env seq bv in
      seq, mk_bv_term bv
    | AxTerm ax ->
      let seq,ax = visit_ax_term k_identity false env seq ax in
      seq, mk_ax_term ax

  and visit_bl_term k bool env seq bl =
    visit_bl_term_desc k bool env seq bl.bl_term_desc

  and visit_bl_term_desc k bool (_,e as env) seq = function
    | BlTrue -> k do_nothing seq mk_bl_true
    | BlFalse -> k do_nothing seq mk_bl_false

    | BlFun (v,ls) ->
      let seq,ls = visit_list (visit_term (v.bl_name,e)) seq ls in
      k do_nothing seq (mk_bl_fun v ls)

    | BlLet (bn,bl) ->
      let seq,bn = visit_list (visit_def e) seq bn in
      let seq = List.fold_left (fun seq df -> push_front_define e df seq) seq bn in
      visit_bl_term k false env seq bl

    | BlUnop (u,bl) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      k (push_front_bl_term bool env) seq (mk_bl_unop u bl)

    | BlBnop (b,bl1,bl2) ->
      let seq,bl1 = visit_bl_term k_identity false env seq bl1 in
      let seq,bl2 = visit_bl_term k_identity false env seq bl2 in
      k (push_front_bl_term bool env) seq (mk_bl_bnop b bl1 bl2)

    | BlComp (c,bl1,bl2) ->
      let seq,bl1 = visit_bl_term k_identity false env seq bl1 in
      let seq,bl2 = visit_bl_term k_identity false env seq bl2 in
      k (push_front_bl_term bool env) seq (mk_bl_comp c bl1 bl2)

    | BvComp (c,bv1,bv2) ->
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_bl_term bool env) seq (mk_bv_comp c bv1 bv2)

    | AxComp (c,ax1,ax2) ->
      let seq,ax1 = visit_ax_term k_identity false env seq ax1 in
      let seq,ax2 = visit_ax_term k_identity false env seq ax2 in
      k (push_front_bl_term bool env) seq (mk_ax_comp c ax1 ax2)

    | BlIte (bl,bl1,bl2) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      let seq,bl1 = visit_bl_term k_identity false env seq bl1 in
      let seq,bl2 = visit_bl_term k_identity false env seq bl2 in
      k (push_front_bl_term bool env) seq (mk_bl_ite bl bl1 bl2)

  and visit_bv_term k bool env seq bv =
    visit_bv_term_desc k bool env seq bv.bv_term_desc

  and visit_bv_term_desc k bool (_,e as env) seq = function
    | BvCst bv -> k do_nothing seq (mk_bv_cst bv)

    | BvFun (v,ls) ->
      let seq,ls = visit_list (visit_term (v.bv_name,e)) seq ls in
      k do_nothing seq (mk_bv_fun v ls)

    | BvLet (bn,bv) ->
      let seq,bn = visit_list (visit_def e) seq bn in
      let seq = List.fold_left (fun seq df -> push_front_define e df seq) seq bn in
      visit_bv_term k false env seq bv

    | BvUnop (u,bv) ->
      let seq,bv = visit_bv_term k_identity false env seq bv in
      k (push_front_bv_term bool env) seq (mk_bv_unop u bv)

    | BvBnop (b,bv1,bv2) ->
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_bv_term bool env) seq (mk_bv_bnop b bv1 bv2)

    | BvIte (bl,bv1,bv2) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_bv_term bool env) seq (mk_bv_ite bl bv1 bv2)

    | Select (n,ax,bv) ->
      let seq,ax = visit_ax_term k_identity false env seq ax in
      let seq,bv = visit_bv_term k_identity false env seq bv in
      k (push_front_bv_term bool env) seq (mk_select n ax bv)

  and visit_ax_term k bool env seq ax =
    visit_ax_term_desc k bool env seq ax.ax_term_desc

  and visit_ax_term_desc k bool (_,e as env) seq = function
    | AxFun (v,ls) ->
      let seq,ls = visit_list (visit_term (v.ax_name,e)) seq ls in
      k do_nothing seq (mk_ax_fun v ls)

    | AxLet (bn,ax) ->
      let seq,bn = visit_list (visit_def e) seq bn in
      let seq = List.fold_left (fun seq df -> push_front_define e df seq) seq bn in
      visit_ax_term k false env seq ax

    | AxIte (bl,ax1,ax2) ->
      let seq,bl = visit_bl_term k_identity false env seq bl in
      let seq,ax1 = visit_ax_term k_identity false env seq ax1 in
      let seq,ax2 = visit_ax_term k_identity false env seq ax2 in
      k (push_front_ax_term bool env) seq (mk_ax_ite bl ax1 ax2)

    | Store (n,ax,bv1,bv2) ->
      let seq,ax = visit_ax_term k_identity false env seq ax in
      let seq,bv1 = visit_bv_term k_identity false env seq bv1 in
      let seq,bv2 = visit_bv_term k_identity false env seq bv2 in
      k (push_front_ax_term bool env) seq (mk_store n ax bv1 bv2)

  and visit_def env seq df =
    visit_def_desc env seq df.def_desc

  and visit_def_desc env seq = function
    | BlDef (v,ls,bl) ->
      if ls = [] then
        let seq,bl = visit_bl_term k_identity true (v.bl_name,env) seq bl in
        seq, mk_bl_def v ls bl
      else seq, mk_bl_def v ls bl
    | BvDef (v,ls,bv) ->
      if ls = [] then
        let seq,bv = visit_bv_term k_identity true (v.bv_name,env) seq bv in
        seq, mk_bv_def v ls bv
      else seq, mk_bv_def v ls bv
    | AxDef (v,ls,ax) ->
      if ls = [] then
        let seq,ax = visit_ax_term k_identity true (v.ax_name,env) seq ax in
        seq, mk_ax_def v ls ax
      else seq, mk_ax_def v ls ax

  and visit_entry env seq en =
    visit_entry_desc env seq en.entry_desc

  and visit_entry_desc env seq = function
    | Declare dc -> push_front_declare dc seq
    | Define df ->
      let seq,df = visit_def env seq df in
      push_front_define env df seq
    | Assert bl ->
      let seq,bl = visit_bl_term k_identity false ("assert",env) seq bl in
      push_front_assert bl seq
    | Assume bl ->
      let seq,bl = visit_bl_term k_identity false ("assume",env) seq bl in
      push_front_assume bl seq
    | Comment c -> push_front_comment c seq
end

(** Ill named function. This does transformation to A normal form *)
let static_single_assignment fm =
  let env = StaticSingleAssignment.create (length fm / 4) in
  iter_forward (StaticSingleAssignment.reserve_entry env) fm;
  fold_forward
    (fun en seq -> StaticSingleAssignment.visit_entry env seq en)
    fm empty


(* Taint *)

module Taint =
struct

  (**
   * The taint of arrays deserves some explanations.
   * if a in an array in the original
   * formula, then:
   *
   * a<taint> is an array of 1-bit bitvectors such that taint(j) & (select a<taint> j) is
   * the taint value of the value written to a at index j.
   * The corresponding variables are usually called ax_a.
   *
   * Two arrays "_false_array" and "_true__arrays" are introduced. They are meant
   * to be constant and equals to false and true respectively. As constant
   * arrays are non standard, they are just symbolic and each time they are read,
   * we assert we read the right value.
   *
   * *)
  type env = {
    tainted : var -> bool;
    bindenv : BindEnv.t;
    (** size => set of sizes for which we have true/false arrays *)
    const_arrays : Basic_types.Int.Set.t;
  }

  let create tainted = {
    tainted;
    bindenv = BindEnv.empty;
    const_arrays = Basic_types.Int.Set.empty;
  }

  (** converts a bv to a bl by testing equality to 1111... *)
  let bl_of_bv bv = mk_bv_equal bv (mk_bv_fill (bv_size bv))
  let bv_of_bl n bl = mk_bv_ite bl (mk_bv_fill n) (mk_bv_zeros n)

  (** converts the boolean variable [foo] to [foo<taint>] *)
  let taintify_bl v = bl_var (v.bl_name ^ "<taint>")

  (** converts the bitvector variable [foo] to [foo<taint>] *)
  let taintify_bv v = bl_var (v.bv_name ^ "<taint>")

  (** converts the array variable [foo] to [foo<taint>] *)
  let taintify_ax v = ax_var (v.ax_name ^ "<taint>") v.idx_size 1

(** the variable name denoting the constant array of value [bl] and of index size [size] *)
  let const_bl_array_var bl size = 
    ax_var (Printf.sprintf "_%b_array_%d" bl size) size 1
  
(** [mk_select len a index] where [a] is the constant array of value [bl] *)
  let const_bl_array_select bl len (index: bv_term) =
    let var = const_bl_array_var bl index.bv_term_size in
    mk_select len (mk_ax_var var) index

(** the condition to assert when using [const_bl_array_at bl len index] *)
  let const_bl_array_cond bl len index =
    let select = const_bl_array_select bl len index in
    mk_bv_equal (bv_of_bl len (mk_bl_const bl)) select

  (** return the taint value of the argument after applying the specified unary operator *)
  let taint_bl_unop _ (_,bl_t) = bl_t

  (** return the taint value of the argument after applying the specified unary operator *)
  let taint_bv_unop _ (_,bv_t) = bv_t

  (** return the taint value of the arguments after applying the specified binary operator.
   * An argument is specified as a pair (term, associated taint boolean term) *)
  let taint_bl_bnop b (bl1,bl1_t) (bl2,bl2_t) =
    match b with
    | BlImply ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bl1_t (mk_bl_equal bl1 mk_bl_false))
           (mk_bl_and bl2_t (mk_bl_equal bl2 mk_bl_true)))
        (mk_bl_and bl1_t bl2_t)
    | BlAnd ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bl1_t (mk_bl_equal bl1 mk_bl_false))
           (mk_bl_and bl2_t (mk_bl_equal bl2 mk_bl_false)))
        (mk_bl_and bl1_t bl2_t)
    | BlOr ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bl1_t (mk_bl_equal bl1 mk_bl_true))
           (mk_bl_and bl2_t (mk_bl_equal bl2 mk_bl_true)))
        (mk_bl_and bl1_t bl2_t)
    | _ -> mk_bl_and bl1_t bl2_t

  (** return the taint value of the arguments after applying the specified binary operator.
   * An argument is specified as a pair (term, associated taint boolean term) *)
  let taint_bv_bnop b (bv1,bv1_t) (bv2,bv2_t) =
    let sz1 = bv_size bv1 in
    let sz2 = bv_size bv2 in
    match b with
    | BvAnd | BvNand | BvMul ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bv1_t (mk_bv_equal bv1 (mk_bv_zeros sz1)))
           (mk_bl_and bv2_t (mk_bv_equal bv2 (mk_bv_zeros sz2))))
        (mk_bl_and bv1_t bv2_t)
    | BvOr | BvNor ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bv1_t (mk_bv_equal bv1 (mk_bv_fill sz1)))
           (mk_bl_and bv2_t (mk_bv_equal bv2 (mk_bv_fill sz2))))
        (mk_bl_and bv1_t bv2_t)
    | _ -> mk_bl_and bv1_t bv2_t

  (** env -> bl_term or bv_term -> taint value of the term as a bl_term *)
  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> visit_bl_term env bl
    | BvTerm bv -> visit_bv_term env bv
    | AxTerm _ -> assert false

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bl_term env bl: bl_term =
    visit_bl_term_desc env bl.bl_term_desc

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true
    | BlFalse -> mk_bl_true

    | BlFun (v,ls) ->
      (match BindEnv.bl_lookup env.bindenv v with
       | BindEnv.Free -> mk_bl_false
       | BindEnv.Declared _ -> mk_bl_fun (taintify_bl v) []
       | BindEnv.Defined _ ->
         let ls_t = List.map (visit_term env) ls in
         let v_t = mk_bl_fun (taintify_bl v) (ls @ (List.map mk_bl_term ls_t)) in
         mk_bl_and v_t (List.fold_right mk_bl_and ls_t mk_bl_true))

    | BlLet (_,_) -> mk_bl_false
    | BlUnop (u,bl) ->
      let bl_t = visit_bl_term env bl in
      taint_bl_unop u (bl,bl_t)
    | BlBnop (b,bl1,bl2) ->
      let bl1_t = visit_bl_term env bl1 in
      let bl2_t = visit_bl_term env bl2 in
      taint_bl_bnop b (bl1,bl1_t) (bl2,bl2_t)
    | BlComp (_,bl1,bl2) ->
      let bl1_t = visit_bl_term env bl1 in
      let bl2_t = visit_bl_term env bl2 in
      mk_bl_and bl1_t bl2_t
    | BvComp (_,bv1,bv2) ->
      let bv1_t = visit_bv_term env bv1 in
      let bv2_t = visit_bv_term env bv2 in
      mk_bl_and bv1_t bv2_t
    | AxComp _ -> mk_bl_false (* sorry *)
    | BlIte (bl,bl1,bl2) ->
      let bl_t = visit_bl_term env bl in
      let bl1_t = visit_bl_term env bl1 in
      let bl2_t = visit_bl_term env bl2 in
      mk_bl_or
        (mk_bl_and bl_t (mk_bl_ite bl bl1_t bl2_t))
        (mk_bl_and (mk_bl_and bl1_t bl2_t) (mk_bl_equal bl1 bl2))

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bv_term env bv =
    visit_bv_term_desc env bv.bv_term_desc

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bv_term_desc env = function
    | BvCst _ -> mk_bl_true

    | BvFun (v,ls) ->
      (match BindEnv.bv_lookup env.bindenv v with
       | BindEnv.Free -> mk_bl_false
       | BindEnv.Declared _ -> mk_bl_fun (taintify_bv v) []
       | BindEnv.Defined _ ->
         let ls_t = List.map (visit_term env) ls in
         let v_t = mk_bl_fun (taintify_bv v) (ls @ (List.map mk_bl_term ls_t)) in
         mk_bl_and v_t (List.fold_right mk_bl_and ls_t mk_bl_true))

    | BvLet (_,_) -> mk_bl_false
    | BvUnop (u,bv) ->
      let bv_t = visit_bv_term env bv in
      taint_bv_unop u (bv,bv_t)
    | BvBnop (b,bv1,bv2) ->
      let bv1_t = visit_bv_term env bv1 in
      let bv2_t = visit_bv_term env bv2 in
      taint_bv_bnop b (bv1,bv1_t) (bv2,bv2_t)
    | BvIte (bl,bv1,bv2) ->
      let bl_t = visit_bl_term env bl in
      let bv1_t = visit_bv_term env bv1 in
      let bv2_t = visit_bv_term env bv2 in
      mk_bl_or
        (mk_bl_and bl_t (mk_bl_ite bl bv1_t bv2_t))
        (mk_bl_and (mk_bl_and bv1_t bv2_t) (mk_bv_equal bv1 bv2))

    | Select (n,ax,bv) ->
      let ax_a = visit_ax_term env ax in
      let bv_t = visit_bv_term env bv in
           mk_bl_and
             (mk_bl_and bv_t (bl_of_bv (mk_select n ax_a bv)))
        (* assert that constant arrays have the value we want *)
             (mk_bl_and
                (const_bl_array_cond true n bv)
                (const_bl_array_cond false n bv)
             )

  (** env -> term -> taint value of the array as an ax_term
   * As such, arrays don't have a taint value, but if the
   * return value of this function is [ax_a], then
   * [taint(select array index) = taint(index) & select ax_a index] *)
  and visit_ax_term env array : ax_term =
    let ax_false = mk_ax_var (const_bl_array_var false array.idx_term_size) in
    match array.ax_term_desc with
    | AxFun (v,ls) ->
      let v_a = taintify_ax v in
      let ax_a = mk_ax_fun v_a ls in
      (
      match BindEnv.ax_lookup env.bindenv v with
       | BindEnv.Free -> ax_false
       | BindEnv.Declared _ -> ax_a
       | BindEnv.Defined _ -> (assert (ls = []); ax_a)
    )
    | AxLet (_,_) -> ax_false
    | AxIte (bl,ax1,ax2) ->
      let bl_t = visit_bl_term env bl in
      let ax1_t = visit_ax_term env ax1 in
      let ax2_t = visit_ax_term env ax2 in
      (* loss of precision, we'd better push select inside ite *)
      mk_ax_ite bl_t ax1_t ax2_t
    | Store (len,ax,index,value) ->
      let ax_a = visit_ax_term env ax in
      let index_t = visit_bv_term env index in
      let value_t = visit_bv_term env value in
      let ax_a' = mk_store len ax_a index (bv_of_bl len value_t) in
      mk_ax_ite index_t ax_a' ax_false

  let do_bindenv f env x = { env with bindenv = f env.bindenv x; }

  (** returns the same formula with additional definitions corresponding the the 
   * declaration passed as argument. *)
  let visit_declare env fm dc: env*formula =
    let env = do_bindenv BindEnv.decl env dc in
    match dc.decl_desc with
    | BlDecl (v,_) ->
      env, push_front_define
        (mk_bl_def (taintify_bl v) []
           (if env.tainted (BlVar v)
            then mk_bl_true else mk_bl_false))
        fm
    | BvDecl (v,_) ->
      env, push_front_define
        (mk_bl_def (taintify_bv v) []
           (if env.tainted (BvVar v)
            then mk_bl_true else mk_bl_false))
        fm
    | AxDecl (v,ls) ->
      assert (ls=[]);
      let const bl = const_bl_array_var bl v.idx_size in
      (* declare the const arrays of this size if not already done *)
      let env, to_declare = 
        if Basic_types.Int.Set.mem v.idx_size env.const_arrays then env, [] else
          begin 
            { env with const_arrays = Basic_types.Int.Set.add v.idx_size env.const_arrays; }, [true; false]
          end
      in
      let fm' =
        List.fold_left
          (fun fm -> fun bl -> push_front_declare (mk_ax_decl (const bl) [] ) fm)
          fm
          to_declare
      in
      (* declare the taint array *)
      let value: ax_term = mk_ax_var (const (env.tainted (AxVar v))) in
      env, push_front_define (mk_ax_def (taintify_ax v) [] value) fm'

  (* (declare-fun foo) -> (declare-fun foo<taint>) *)
  let taintify_decl dc =
    match dc.decl_desc with
    | BlDecl (v,_) -> mk_bl_decl (taintify_bl v) []
    | BvDecl (v,_) -> mk_bl_decl (taintify_bv v) []
    | AxDecl _ -> assert false

  (** returns the same formula with additional definitions corresponding the the 
   * definition passed as argument. *)
  let visit_define env fm df: env* formula =
    let env = do_bindenv BindEnv.def env df in
    match df.def_desc with
    | BlDef (v,ls,bl) ->
      let ls_t = List.map taintify_decl ls in
      let bl_t = visit_bl_term env bl in
      env, push_front_define (mk_bl_def (taintify_bl v) (ls @ ls_t) bl_t) fm
    | BvDef (v,ls,bv) ->
      let ls_t = List.map taintify_decl ls in
      let bv_t = visit_bv_term env bv in
      env, push_front_define (mk_bl_def (taintify_bv v) (ls @ ls_t) bv_t) fm
    | AxDef (v,ls,ax) ->
      let ax_a = visit_ax_term env ax in
      let v_a = taintify_ax v in
      env, push_front_define (mk_ax_def v_a ls ax_a) fm

  (** returns the formula with taint things appended *)
  let visit_entry env fm en: env * formula =
    match en.entry_desc with
    | Declare dc -> visit_declare env fm dc
    | Define df -> visit_define env fm df
    (* Note how assertions and assumptions are treated differently
    * We want to encode \exists input, \forall uncontrolled, assume => assert
    * Result: \exists input, uncontrolled: assume & assert & taint(assert)
    *)
    | Assert bl -> env, push_front_assert (visit_bl_term env bl) fm
    | Assume _ | Comment _ -> env, fm
end

(** applies taint based universal quantifiers elimination to the formula, where
 * all variables in [vars] are assumed to be universally quantified, and other
 * ones existentially. *)
let taint vars fm =
  let env = Taint.create vars in
  fold_forward
    (fun en (env, fm) -> Taint.visit_entry env (push_front en fm) en)
    fm (env, empty)
  |> snd

(* A supposedly smarter Taint *)

module Taint2 =
struct

  type env = {
    ai_env : AIInner.env;
    ai_visitor: AIInner.visitor option;
    layer_taint_arrays: ax_var BvTermHashamt.t AxTermHashamt.t;
    to_define : entry list;
    tainted : var -> bool;
    bindenv : BindEnv.t;
    (** size => set of sizes for which we have true/false arrays *)
    const_arrays : Basic_types.Int.Set.t;
  }

  let create ~unfold tainted = {
    ai_env = AIInner.create unfold;
    ai_visitor = None;
    layer_taint_arrays = AxTermHashamt.empty;
    to_define = [];
    tainted;
    bindenv = BindEnv.empty;
    const_arrays = Basic_types.Int.Set.empty;
  }

  (** converts a bv to a bl by testing equality to 1111... *)
  let bl_of_bv bv = mk_bv_equal bv (mk_bv_fill (bv_size bv))
  let bv_of_bl n bl = mk_bv_ite bl (mk_bv_fill n) (mk_bv_zeros n)

  (** converts the boolean variable [foo] to [foo<taint>] *)
  let taintify_bl v = bl_var (v.bl_name ^ "<taint>")

  (** converts the bitvector variable [foo] to [foo<taint>] *)
  let taintify_bv v = bl_var (v.bv_name ^ "<taint>")

  (** converts the array variable [foo] to [foo<taint>] *)
  let taintify_ax v = ax_var (v.ax_name ^ "<taint>") v.idx_size 1

(** the variable name denoting the constant array of value [bl] and of index size [size] *)
  let const_bl_array_var bl size = 
    ax_var (Printf.sprintf "_%b_array_%d" bl size) size 1

  (** same as above, but also declares the array in env *)
  let const_bl_array_var env bl size =
    let var = const_bl_array_var bl size in
    if Basic_types.Int.Set.mem size env.const_arrays then
      env, var
    else
      let mk x = mk_declare (mk_ax_decl x []) in
      let to_define = (mk var) :: (mk (const_bl_array_var (not bl) size)) :: env.to_define in
      let const_arrays = Basic_types.Int.Set.add size env.const_arrays in
      let env = { env with const_arrays; to_define } in
      env, var
  
(** [mk_select len a index] where [a] is the constant array of value [bl] *)
  let const_bl_array_select (env:env) bl len (index: bv_term) =
    let env, var = const_bl_array_var env bl index.bv_term_size in
    env, mk_select len (mk_ax_var var) index

(** the condition to assert when using [const_bl_array_at bl len index] *)
  let const_bl_array_cond env bl len index =
    let env, select = const_bl_array_select env bl len index in
    env, mk_bv_equal (bv_of_bl len (mk_bl_const bl)) select

  let layer_taint_array_for env ax layer_base =
    try
      let layers = AxTermHashamt.find ax env.layer_taint_arrays in
      env, BvTermHashamt.find layer_base layers
    with Not_found -> const_bl_array_var env false ax.idx_term_size

  let layer_taint_select env ax layer_base index len =
    let env, var = layer_taint_array_for env ax layer_base in
    let select = mk_select len (mk_ax_var var) index in
    env, mk_bv_equal (bv_of_bl len mk_bl_true) select

  let layer_taint_counter = ref 0
  let layer_taint_name ax layer_base =
    let ax_name = match is_ax_var ax with
    | Some(v) -> v.ax_name
    | None -> "anon"
    in
    let bv_name = match is_bv_var layer_base with
      | Some(v) -> v.bv_name
      | None -> match is_bv_cst layer_base with
        | Some _ -> "cst"
        | None -> "anon"
    in
    let i = !layer_taint_counter in
    layer_taint_counter := i + 1;
    Format.asprintf "_taint_%s_layer_%s_%i" ax_name bv_name i

  let apply_pending_declarations env fm: env* formula =
    let pending = List.rev env.to_define in
    let fm = List.fold_left (fun acc elt -> push_front elt acc) fm pending in
    let env = { env with to_define = [] } in
    env, fm

  let pp_layers fmt env =
    let open Formula_pp in
    AxTermHashamt.iter (fun ax x ->
        Format.fprintf fmt "* %a -> \n" pp_ax_term ax;
      BvTermHashamt.iter (fun base var ->
            Format.fprintf fmt "** %a -> %a \n" pp_bv_term base pp_ax_term (mk_ax_var var)
          ) x
      ) env.layer_taint_arrays

  (** return the taint value of the argument after applying the specified unary operator *)
  let taint_bl_unop _ (_,bl_t) = bl_t

  (** return the taint value of the argument after applying the specified unary operator *)
  let taint_bv_unop _ (_,bv_t) = bv_t

  (** return the taint value of the arguments after applying the specified binary operator.
   * An argument is specified as a pair (term, associated taint boolean term) *)
  let taint_bl_bnop b (bl1,bl1_t) (bl2,bl2_t) =
    match b with
    | BlImply ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bl1_t (mk_bl_equal bl1 mk_bl_false))
           (mk_bl_and bl2_t (mk_bl_equal bl2 mk_bl_true)))
        (mk_bl_and bl1_t bl2_t)
    | BlAnd ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bl1_t (mk_bl_equal bl1 mk_bl_false))
           (mk_bl_and bl2_t (mk_bl_equal bl2 mk_bl_false)))
        (mk_bl_and bl1_t bl2_t)
    | BlOr ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bl1_t (mk_bl_equal bl1 mk_bl_true))
           (mk_bl_and bl2_t (mk_bl_equal bl2 mk_bl_true)))
        (mk_bl_and bl1_t bl2_t)
    | _ -> mk_bl_and bl1_t bl2_t

  (* return the abstract value of the bitvector *)
  let abstract env bv =
    let fallback = Codex.Ival_basis.Binary_Forward.bunknown ~size:(bv_size bv) in
    match env.ai_visitor with 
    | None -> fallback
    | Some(visitor) -> try BvTermHashamt.find bv visitor.AIInner.bv_mem
      with Not_found -> fallback

  (** return the taint value of the arguments after applying the specified binary operator.
   * An argument is specified as a pair (term, associated taint boolean term) *)
  let taint_bv_bnop env b (bv1,bv1_t) (bv2,bv2_t) =
    let size = bv_size bv1 in
    let restrict_for mk_mask controlled uncontrolled = 
      let _, modulo = congruence_for ~size (abstract env uncontrolled) in
      let trailing = Z.trailing_zeros modulo in
      (* we know that the trailing low order bits of uncontrolled are constant *)
      let x = mk_bv_extract Interval.{lo = trailing; hi = size-1} controlled in
      let mask = mk_mask (size - trailing) in
      mk_bv_equal x mask
    in
    match b with
    | BvMul ->
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bv1_t (mk_bv_equal bv1 (mk_bv_zeros size)))
           (mk_bl_and bv2_t (mk_bv_equal bv2 (mk_bv_zeros size))))
        (mk_bl_and bv1_t bv2_t)
    | BvAnd | BvNand ->
      let restrict = restrict_for mk_bv_zeros in
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bv1_t (restrict bv1 bv2))
           (mk_bl_and bv2_t (restrict bv2 bv1)))
        (mk_bl_and bv1_t bv2_t)
    | BvOr | BvNor ->
      let restrict = restrict_for mk_bv_fill in
      mk_bl_or
        (mk_bl_or
           (mk_bl_and bv1_t (restrict bv1 bv2))
           (mk_bl_and bv2_t (restrict bv2 bv1)))
        (mk_bl_and bv1_t bv2_t)
    | _ -> mk_bl_and bv1_t bv2_t


  (* returns a list of pairs of booleans bl1, bl2 such that bl2 => (= bl1 (op
   * bv1 bv2)). The boolean bl1 is easier to taint when op is an inequality and
   * bv1 and bv2 share a symbolic addend whose abstract value is reasonably
   * precise.  bl1 and bl2 must still be tainted.
   *
   * Designed to simplify the test esp + 1 <= esp + len
   * 
   * The rule is:
   * if a term x is on boths sides of an inequality y1 + x <= y2 + x  (a)
   * and that abstract interpretation says x \in [min, max]           (b)
   * and max - min <= 2^{n/2}                                         (c)
   * and y1+min and y1+max have the same sign                         (d)
   * and y2+min and y2+max have the same sign                         (e)
   * then
   * (y1 + x <= y2 + x) = (y1 + min <= y2 + min)                      (f)
   * if x is not controlled but y1 and y2 are, then this improves taint.
   * 
   * Here sign means the boolean x >=s 0. This is important.
   *
   * (a), (b), (c) are checked in the ocaml code
   * (d), (e) constitute bl2: the condition of validity of the simplification.
   * (f) constitutes bl1: the simplified expression.
   *
   * As the choice of the term x is not necessarily unique, we return a list
   * of all possibilities.
   * *)
  let simplify_equal_terms_from_comparison env op bv1 bv2 =
    let open Term_transformation.Addition in
    let size = bv_size bv1 in
    let e1 = env_of_bv_term bv1 in
    let e2 = env_of_bv_term bv2 in
    (* the set of terms common to both sides of the inequality *)
    let inter = BvTermHashamt.join
        (fun _term a b -> if (a = 1 && b = 1) (* could be generalized to a=b *) then Some(a) else None)
        e1.symbolic
        e2.symbolic
    in
    (* returns a term expressing these terms have the same sign *)
    let same_sign a b =
      let zero = mk_bv_zeros size in
      mk_bl_or 
        (mk_bl_and (mk_bv_sge a zero) (mk_bv_sge b zero))
        (mk_bl_and (mk_bv_slt a zero) (mk_bv_slt b zero))
    in
    (* do the transformation when removing this specific term *)
    let remove key =
      let min, max = min_max_for ~signed:false ~size (abstract env key) in
      let min = match min with | Some x -> x | None -> Bitvector.zeros size in
      let max = match max with | Some x -> x | None -> Bitvector.fill size in
      let dist = Bitvector.sub max min in
      (* condition (c) *)
      let ok = Bitvector.(ult dist (shift_left (ones size) (size - 1))) in
      if not ok then mk_bl_false, mk_bl_false else
        (* transforms y + x into y + min *)
        let f e =
          let econst = {
              symbolic = BvTermHashamt.remove key e.symbolic;
              constant = Bitvector.add e.constant min;
            }
          in
          bv_term_of_env econst
        in
        let bv1const = f e1 in
        let bv2const = f e2 in
        let cond = mk_bl_and
          (same_sign bv1const (mk_bv_add bv1const (mk_bv_cst dist)))
          (same_sign bv2const (mk_bv_add bv2const (mk_bv_cst dist)))
        in
        mk_bv_comp op bv1const bv2const, cond
    in
    BvTermHashamt.fold (fun key _value acc -> (remove key)::acc) inter []

  let taint_bv_comp env (op:bv_comp) (bv1, bv1_t) bv2 =
    let size = bv_size bv1 in
    (* a term for (bv op const) if const is not None *)
    let compare op bv const = match const with
      | None -> mk_bl_false
      | Some(bound) -> mk_bv_comp op bv (mk_bv_cst bound)
    in
    let comparable op1 op2 ~signed =
      let min, max = min_max_for ~size ~signed (abstract env bv2) in
      mk_bl_and bv1_t (mk_bl_or (compare op1 bv1 min) (compare op2 bv1 max))
    in
    match op with
    | Formula.BvEqual
    | Formula.BvDistinct ->
      mk_bl_or 
        (comparable BvUlt BvUgt ~signed:false)
        (comparable BvSlt BvSgt ~signed:true)
    | Formula.BvUle
    | Formula.BvUlt ->
        (comparable op (not_bv_comp op) ~signed:false)
    | Formula.BvUgt
    | Formula.BvUge ->
        (comparable (not_bv_comp op) op ~signed:false)
    | Formula.BvSlt
    | Formula.BvSle ->
        (comparable op (not_bv_comp op) ~signed:true)
    | Formula.BvSgt
    | Formula.BvSge ->
        (comparable (not_bv_comp op) op ~signed:true)

  let taint_bv_comp env op (bv1, bv1_t) (bv2, bv2_t) =
    let edge1 = taint_bv_comp env op (bv1, bv1_t) bv2 in
    let edge2 = taint_bv_comp env (not_bv_comp op) (bv2, bv2_t) bv1 in
    mk_bl_or (mk_bl_and bv1_t bv2_t) (mk_bl_or edge1 edge2)

  let counter = ref 0
  let fresh_name () =
    let i = !counter in
    let res = Format.asprintf "__taint_aux_%d" i |> bl_var in
    counter := i + 1;
    res

  (** env -> bl_term or bv_term -> taint value of the term as a bl_term *)
  let rec visit_term env tm =
    visit_term_desc env tm.term_desc

  and visit_term_desc env = function
    | BlTerm bl -> visit_bl_term env bl
    | BvTerm bv -> visit_bv_term env bv
    | AxTerm _ -> assert false

  and redo_ai_and_visit_bl_term env bl =
    let entry = mk_define (mk_bl_def (fresh_name ()) [] bl) in
    let simplified, ai_env, ai_visitor = AIInner.visit_entry env.ai_env entry in
    let new_env = { env with ai_env; ai_visitor } in
    let res, new_env = match simplified.entry_desc with
    | Define d -> (match d.def_desc with
        | BlDef(_, [], bl2) -> visit_bl_term new_env bl2
        | _ -> assert false)
    | _ -> assert false
    in
    res, { new_env with ai_env = env.ai_env; ai_visitor = env.ai_visitor }

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bl_term env bl: bl_term*env =
    visit_bl_term_desc env bl.bl_term_desc

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bl_term_desc env = function
    | BlTrue -> mk_bl_true, env
    | BlFalse -> mk_bl_true, env

    | BlFun (v,ls) ->
      (match BindEnv.bl_lookup env.bindenv v with
       | BindEnv.Free -> mk_bl_false
       | BindEnv.Declared _ -> mk_bl_fun (taintify_bl v) []
       | BindEnv.Defined _ ->
         let ls_t = List.map (fun term -> visit_term env term |> fst) ls in
         let v_t = mk_bl_fun (taintify_bl v) (ls @ (List.map mk_bl_term ls_t)) in
         mk_bl_and v_t (List.fold_right mk_bl_and ls_t mk_bl_true)), env

    | BlLet (_,_) -> mk_bl_false, env
    | BlUnop (u,bl) ->
      let bl_t, env = visit_bl_term env bl in
      taint_bl_unop u (bl,bl_t), env
    | BlBnop (b,bl1,bl2) ->
      let bl1_t, env = visit_bl_term env bl1 in
      let bl2_t, env = visit_bl_term env bl2 in
      taint_bl_bnop b (bl1,bl1_t) (bl2,bl2_t), env
    | BlComp (_,bl1,bl2) ->
      let bl1_t, env = visit_bl_term env bl1 in
      let bl2_t, env = visit_bl_term env bl2 in
      mk_bl_and bl1_t bl2_t, env
    | BvComp (op,bv1,bv2) ->
      let bv1_t, env = visit_bv_term env bv1 in
      let bv2_t, env = visit_bv_term env bv2 in
      let easy_taint = taint_bv_comp env op (bv1,bv1_t) (bv2,bv2_t) in
      let simp_list = simplify_equal_terms_from_comparison env op bv1 bv2 in
      List.fold_left 
        (fun (acc, env) (simplified_comp, simplification_cond) ->
          let more_precise_taint, env = if is_bl_cst simplification_cond <> None
            then mk_bl_false, env
            (* Termination:
             * contains
             * - comparisons to constants which return false above
             * - 1 smaller term with the metric: number of variables
             *)
            else redo_ai_and_visit_bl_term env (mk_bl_and simplified_comp simplification_cond)
          in
          (mk_bl_or acc (mk_bl_and simplification_cond more_precise_taint)), env
        ) (easy_taint, env) simp_list

    | AxComp _ -> mk_bl_false, env (* sorry *)
    | BlIte (bl,bl1,bl2) ->
      let bl_t, env = visit_bl_term env bl in
      let bl1_t, env = visit_bl_term env bl1 in
      let bl2_t, env = visit_bl_term env bl2 in
      mk_bl_or
        (mk_bl_and bl_t (mk_bl_ite bl bl1_t bl2_t))
        (mk_bl_and (mk_bl_and bl1_t bl2_t) (mk_bl_equal bl1 bl2)), env

  (** env -> term -> taint value of the term as a bl_term *)
  and visit_bv_term env bv: bl_term*env = match bv.bv_term_desc with
    | BvCst _ -> mk_bl_true, env

    | BvFun (v,ls) ->
      (match BindEnv.bv_lookup env.bindenv v with
       | BindEnv.Free -> mk_bl_false
       | BindEnv.Declared _ -> mk_bl_fun (taintify_bv v) []
       | BindEnv.Defined _ ->
         let ls_t = List.map (fun term -> visit_term env term |> fst) ls in
         let v_t = mk_bl_fun (taintify_bv v) (ls @ (List.map mk_bl_term ls_t)) in
         mk_bl_and v_t (List.fold_right mk_bl_and ls_t mk_bl_true)), env

    | BvLet (_,_) -> mk_bl_false, env
    | BvUnop (u,bv) ->
      let bv_t, env = visit_bv_term env bv in
      taint_bv_unop u (bv,bv_t), env
    | BvBnop (b,bv1,bv2) ->
      let bv1_t, env = visit_bv_term env bv1 in
      let bv2_t, env = visit_bv_term env bv2 in
      taint_bv_bnop env b (bv1,bv1_t) (bv2,bv2_t), env
    | BvIte (bl,bv1,bv2) ->
      let bl_t, env = visit_bl_term env bl in
      let bv1_t , env= visit_bv_term env bv1 in
      let bv2_t , env= visit_bv_term env bv2 in
      mk_bl_or
        (mk_bl_and bl_t (mk_bl_ite bl bv1_t bv2_t))
        (mk_bl_and (mk_bl_and bv1_t bv2_t) (mk_bv_equal bv1 bv2)), env

    | Select (n,ax,idx) ->
      let visitor = Utils.unsafe_get_opt env.ai_visitor in
      let _, result = AIInner.(CodexRow.lookup_debug env.ai_env.row (env.ai_env.bindmap, visitor) n ax idx) in
      Formula_options.Logger.debug "Tainting %a: %a" Formula_pp.pp_bv_term bv AIInner.CodexRow.pp_result result;
      let ax_a, env = visit_ax_term env ax in
      let bv_t, env = visit_bv_term env idx in
        (* assert that constant arrays have the value we want *)
      let add_const_bl_array_cond bl env =
        let env, cond = const_bl_array_cond env bl n idx in
        let to_define = (mk_assume cond) :: env.to_define in
        {env with to_define}
      in
      let env = add_const_bl_array_cond true env |> add_const_bl_array_cond false in
      let old_taint = mk_bl_and bv_t (bl_of_bv (mk_select n ax_a idx))
      in
      let new_taint, env = match result with
        | AIInner.CodexRow.Layer base ->
          let offset = mk_bv_sub idx base |> Term_transformation.simplify_additions in
          let offset_taint, env = visit_bv_term env offset in
          let env, layer_taint = layer_taint_select env ax base idx n in
          mk_bl_and offset_taint layer_taint, env
        | AIInner.CodexRow.Conflict (_,_) -> mk_bl_false, env
        | AIInner.CodexRow.Below -> mk_bl_false, env
      in
      mk_bl_or old_taint new_taint, env

  (** env -> term -> taint value of the array as an ax_term
   * As such, arrays don't have a taint value, but if the
   * return value of this function is [ax_a], then
   * [taint(select array index) = taint(index) & select ax_a index] *)
  and visit_ax_term env array : ax_term* env =
    let env, ax_false_var = const_bl_array_var env false array.idx_term_size in
    let ax_false = mk_ax_var ax_false_var in
    match array.ax_term_desc with
    | AxFun (v,ls) ->
      let v_a = taintify_ax v in
      let ax_a = mk_ax_fun v_a ls in
      (
      match BindEnv.ax_lookup env.bindenv v with
       | BindEnv.Free -> ax_false, env
       | BindEnv.Declared _ -> ax_a, env
       | BindEnv.Defined (_, _, def) -> (
           assert (ls = []);
           let layer_taint_arrays = try
             let old = AxTermHashamt.find def env.layer_taint_arrays in
             AxTermHashamt.add array old env.layer_taint_arrays
           with Not_found -> env.layer_taint_arrays in
           ax_a, { env with layer_taint_arrays }
         )
    )
    | AxLet (_,_) -> ax_false, env
    | AxIte (bl,ax1,ax2) ->
      let bl_t, env = visit_bl_term env bl in
      let ax1_t, env = visit_ax_term env ax1 in
      let ax2_t , env= visit_ax_term env ax2 in
      (* loss of precision, we'd better push select inside ite *)
      mk_ax_ite bl_t ax1_t ax2_t, env
    | Store (len,ax,index,value) ->
      let ax_a, env = visit_ax_term env ax in
      Formula_options.Logger.debug "Tainting %a" Formula_pp.pp_ax_term array;
      Formula_options.Logger.debug "layers: %a" pp_layers env;

      let visitor = Utils.unsafe_get_opt env.ai_visitor in
      let _, result = AIInner.(CodexRow.update_debug env.ai_env.row (env.ai_env.bindmap, visitor) array) in
      Formula_options.Logger.debug "Result = %a" AIInner.CodexRow.pp_result result;
      let layer_taint, base, layer_base = match result with
        | AIInner.CodexRow.Layer base ->
          let layer_taint =
            try AxTermHashamt.find ax env.layer_taint_arrays
            with Not_found -> BvTermHashamt.empty
          in
          let layer_base = try
              BvTermHashamt.find base layer_taint |> mk_ax_var
          with Not_found -> ax_false
          in
          layer_taint, base, layer_base
        | AIInner.CodexRow.Conflict (base,conflicts) ->
          let layer_taint =
            try
              let old = AxTermHashamt.find ax env.layer_taint_arrays in
              List.fold_left (fun acc elt -> BvTermHashamt.remove elt acc) old conflicts
            with Not_found -> BvTermHashamt.empty
          in
          layer_taint, base, ax_false
        | AIInner.CodexRow.Below -> assert false
      in
      let layer_def = mk_store len layer_base index (bv_of_bl len mk_bl_true) in
      let layer_name = layer_taint_name ax base in
      let layer = ax_var layer_name ax.idx_term_size 1 in
      let layer_taint = BvTermHashamt.add base layer layer_taint in
      let layer_taint_arrays = AxTermHashamt.add array layer_taint env.layer_taint_arrays in
      let def = mk_ax_def layer [] layer_def in
      let to_define = (mk_define def)::env.to_define in
      let env = { env with layer_taint_arrays; to_define } in
      (* old taint *)
      let index_t, env = visit_bv_term env index in
      let value_t, env = visit_bv_term env value in
      let ax_a' = mk_store len ax_a index (bv_of_bl len value_t) in
      mk_ax_ite index_t ax_a' ax_false, env

  let do_bindenv f env x = { env with bindenv = f env.bindenv x; }

  (** returns the same formula with additional definitions corresponding the the 
   * declaration passed as argument. *)
  let visit_declare env fm dc: env*formula =
    let env = do_bindenv BindEnv.decl env dc in
    match dc.decl_desc with
    | BlDecl (v,_) ->
      env, push_front_define
        (mk_bl_def (taintify_bl v) []
           (if env.tainted (BlVar v)
            then mk_bl_true else mk_bl_false))
        fm
    | BvDecl (v,_) ->
      env, push_front_define
        (mk_bl_def (taintify_bv v) []
           (if env.tainted (BvVar v)
            then mk_bl_true else mk_bl_false))
        fm
    | AxDecl (v,ls) ->
      assert (ls=[]);
      (* declare the taint array *)
      let taint = env.tainted (AxVar v) in
      let env, const = const_bl_array_var env taint v.idx_size in
      let env, fm = apply_pending_declarations env fm in
      let value: ax_term = mk_ax_var (const) in
      env, push_front_define (mk_ax_def (taintify_ax v) [] value) fm

  (* (declare-fun foo) -> (declare-fun foo<taint>) *)
  let taintify_decl dc =
    match dc.decl_desc with
    | BlDecl (v,_) -> mk_bl_decl (taintify_bl v) []
    | BvDecl (v,_) -> mk_bl_decl (taintify_bv v) []
    | AxDecl _ -> assert false

  (** returns the same formula with additional definitions corresponding the the 
   * definition passed as argument. *)
  let visit_define env fm df: env* formula =
    let env = do_bindenv BindEnv.def env df in
    match df.def_desc with
    | BlDef (v,ls,bl) ->
      let ls_t = List.map taintify_decl ls in
      let bl_t, env = visit_bl_term env bl in
      env, push_front_define (mk_bl_def (taintify_bl v) (ls @ ls_t) bl_t) fm
    | BvDef (v,ls,bv) ->
      let ls_t = List.map taintify_decl ls in
      let bv_t, env = visit_bv_term env bv in
      let env, fm = apply_pending_declarations env fm in
      env, push_front_define (mk_bl_def (taintify_bv v) (ls @ ls_t) bv_t) fm
    | AxDef (v,ls,ax) ->
      let env = try
          let old = AxTermHashamt.find ax env.layer_taint_arrays in
          let layer_taint_arrays = AxTermHashamt.add (mk_ax_var v) old env.layer_taint_arrays in
          { env with layer_taint_arrays }
        with Not_found -> env
      in
      let ax_a, env = visit_ax_term env ax in
      let v_a = taintify_ax v in
      let env, fm = apply_pending_declarations env fm in
      env, push_front_define (mk_ax_def v_a ls ax_a) fm

  (** returns the formula with taint things appended *)
  let visit_entry env fm en: env * formula =
    let _, ai_env, ai_visitor = AIInner.visit_entry env.ai_env en in
    let env = { env with ai_env; ai_visitor } in
    match en.entry_desc with
    | Declare dc -> visit_declare env fm dc
    | Define df -> visit_define env fm df
    (* Note how assertions and assumptions are treated differently
    * We want to encode \exists input, \forall uncontrolled, assume => assert
    * Result: \exists input, uncontrolled: assume & assert & taint(assert)
    *)
    | Assert bl ->
      let taint, env = visit_bl_term env bl in
      let env, fm = apply_pending_declarations env fm in
      env, push_front_assert taint fm
    | Assume _ | Comment _ -> env, fm
end

(** applies taint based universal quantifiers elimination to the formula, where
 * all variables in [vars] are assumed to be universally quantified, and other
 * ones existentially. *)
let taint2 ~unfold vars fm =
  let env = Taint2.create ~unfold vars in
  let env, fm = fold_forward
      (fun en (env, fm) -> Taint2.visit_entry env (push_front en fm) en)
      fm (env, empty)
  in
  if env.Taint2.to_define <> [] then Formula_options.Logger.fatal "BUG_ON env.to_define != [], missing apply_pending_declarations";
  fm


module Forall = struct
  type layer = {
    decls : decl list;
    defs : def list;
    assumptions : bl_term;
    assertions : bl_term;
  }
  let empty_layer = {
    decls = [];
    defs = [];
    assumptions = mk_bl_true;
    assertions = mk_bl_true;
  }

  [@@@warning "-32"]
  let pp_layer ppf l = Format.fprintf ppf "@[<v 0>Decls: %a@ Defs:%a@ assume %a@ assert %a@]"
      (Format.pp_print_list Formula_pp.pp_entry) (List.map mk_declare l.decls)
      (Format.pp_print_list Formula_pp.pp_entry) (List.map mk_define l.defs)
      Formula_pp.pp_bl_term l.assumptions
      Formula_pp.pp_bl_term l.assertions

  type env = {
    parameters : layer;
    controlled : layer;
    uncontrolled : layer;
  }

  [@@@warning "-32"]
  let pp_env ppf env =
    Format.fprintf ppf "ENV=@[<v 1> param = %a@ controlled = %a@ uncontrolled %a@]"
      pp_layer env.parameters
      pp_layer env.controlled
      pp_layer env.uncontrolled

  let add_decl_to_layer layer decl = {layer with decls = decl::layer.decls}
  let add_def_to_layer layer decl = {layer with defs = decl::layer.defs}
  let add_assumption_to_layer layer assumption = {layer with assumptions = mk_bl_and assumption layer.assumptions}
  let add_assertion_to_layer layer assertion = {layer with assertions = mk_bl_and assertion layer.assertions }

  let decl_var d = match d.decl_desc with
    | BlDecl (v, _) ->  BlVar v
    | BvDecl (v, _) ->  BvVar v
    | AxDecl (v, _) ->  AxVar v

  let def_var d = match d.def_desc with
    | BlDef (v, _, _) ->  BlVar v
    | BvDef (v, _, _) ->  BvVar v
    | AxDef (v, _, _) ->  AxVar v

  let decl_controlled is_controlled d = is_controlled (decl_var d)

  let def_controlled is_controlled d = 
    let vars = match d.def_desc with
    | BlDef (_, _, t) -> Formula_utils.bl_term_variables t
    | BvDef (_, _, t) -> Formula_utils.bv_term_variables t
    | AxDef (_, _, t) -> Formula_utils.ax_term_variables t
    in
    VarSet.for_all is_controlled vars

  let make_memory_controlled approx is_controlled fm =
    let memories = get_memory fm in
    match memories with
    | memory :: [] when not (is_controlled (AxVar memory)) ->
      if approx then
          let asserts = memory_control_approx (mk_ax_var memory) fm in
          let mem_var = AxVar memory in
          let is_controlled x = (x = mem_var) || is_controlled x in
          let fm = Formula_options.Logger.debug ~level:7 "make memory controlled with %a"
            (Format.pp_print_list Formula_pp.pp_entry) asserts;
          List.fold_left (fun fm assertion -> push_front assertion fm) fm asserts
          in
          is_controlled, fm
        else
          let fm, var = memory_control2 memory fm in
          let mem_var = AxVar var in
          let is_controlled x = (x = mem_var) || is_controlled x in
          is_controlled, fm
    | _ -> 
      Formula_options.Logger.debug ~level:7 "Could not make memory controlled. Memories = %a"
        (Format.pp_print_list Formula_pp.pp_ax_var) memories;
      is_controlled, fm

  let add_bl_term_to_highest_layer env is_controlled is_parameter bl f =
    let vars = bl_term_variables bl in
    let all_param = VarSet.for_all is_parameter vars in
    let layer, replace = if all_param then env.parameters, fun parameters -> {env with parameters} else
        let all_controlled = VarSet.for_all is_controlled vars in
        if all_controlled then env.controlled, fun controlled -> {env with controlled} else
          env.uncontrolled, fun uncontrolled -> {env with uncontrolled} in
    f layer bl |> replace

  let add_defs content layer = 
    List.fold_left (fun acc def -> mk_bl_let [def] acc) content layer.defs

  (** makes a list of variable suitable to be quantified *)
  let quantify_declarations =
    List.rev_map (fun d ->
        let en = mk_declare d in
        let command = Formula_to_smtlib.entry en in
        let symbol, sort = match command.Smtlib.command_desc with
          | Smtlib.CmdDeclareConst(symbol, sort)
          | Smtlib.CmdDeclareFun(symbol, None, [], sort) -> (symbol, sort)
          | _ -> failwith "to_universal_quantifier of function"
        in
        Smtlib_utils.mk_sorted_var symbol sort)

  (** builds (op a b) *)
  let mk_smtlib_bin op a b =
    let open Smtlib_utils in
    let bl_and = mk_symbol op |> mk_id_symbol |> mk_qual_identifier_identifier
    in
    mk_term_qual_identifier_terms bl_and [a; b]

  let rec replace_true_in_let inner t = 
    let desc = match t.Smtlib.term_desc with
    | Smtlib.TermLetTerm (bindings, term) -> Smtlib.TermLetTerm (bindings, replace_true_in_let inner term)
    | Smtlib.TermSpecConstant _ -> inner.Smtlib.term_desc
    | _ -> failwith "replace_true_in_let"
    in
    { t with Smtlib.term_desc = desc; }

  let add_defs_smtlib content layer =
    let defs_with_true = add_defs mk_bl_true layer |> Formula_to_smtlib.bl_term in
    replace_true_in_let content defs_with_true

  let is_recursively layers env v =
      let l = layers env in
      List.exists (fun layer ->
          List.exists (fun decl -> decl_var decl = v) layer.decls
        || (List.exists (fun def -> def_var def = v) layer.defs)
        ) l


  let executability_layer layer acc =
    let bl = mk_smtlib_bin "and" (Formula_to_smtlib.bl_term layer.assumptions) acc in
    let content = add_defs_smtlib bl layer in
    content

  (* a term expressing that the corresponding assumptions are satisfiable *)
  let executability env =
    let content =  mk_bl_true |> Formula_to_smtlib.bl_term
                   |> executability_layer env.uncontrolled
                   |> executability_layer env.controlled
                   |> executability_layer env.parameters
    in
    let quantified = quantify_declarations (List.concat [env.uncontrolled.decls; env.controlled.decls; env.parameters.decls]) in
    Smtlib_utils.mk_term_exists_term quantified content





  let to_universal ?start_time is_controlled is_parameter fm =
    (* resulting formula:
     * ∀ parameter_decl parameter_assume => ∃ controlled_decl controlled_assert ∧ ∀ forall_decl (let (let_def) assumption => assertion *)
    (* if start_time is not None, register stats *)
    let env = {
      parameters = empty_layer;
      controlled = empty_layer;
      uncontrolled = empty_layer;
    } in
    let is_recursively_param = is_recursively (fun env -> [env.parameters]) in
    let is_recursively_controlled = is_recursively (fun env -> [env.parameters; env.controlled]) in
    let quantified_bool = ref 0 in
    let quantified_bv = ref 0 in
    let quantified_array = ref 0 in
    let env = fold_forward (fun en env -> match en.entry_desc with
        | Assert bl -> add_bl_term_to_highest_layer env (is_recursively_controlled env) (is_recursively_param env) bl add_assertion_to_layer
        | Assume bl -> add_bl_term_to_highest_layer env (is_recursively_controlled env) (is_recursively_param env) bl add_assumption_to_layer
        | Define d when def_controlled (is_recursively_param env) d -> { env with parameters = add_def_to_layer env.parameters d }
        | Define d when def_controlled (is_recursively_controlled env) d -> { env with controlled = add_def_to_layer env.controlled d }
        | Define d -> { env with uncontrolled = add_def_to_layer env.uncontrolled d }
        | Declare d when decl_controlled is_parameter d -> { env with parameters = add_decl_to_layer env.parameters d }
        | Declare d when decl_controlled is_controlled d -> { env with controlled = add_decl_to_layer env.controlled d }
        | Declare d -> begin
            let desc = d.decl_desc in
            (match desc with
             | Formula.BlDecl (_, _) -> quantified_bool := !quantified_bool+1
             | Formula.BvDecl (_, _) -> quantified_bv := !quantified_bv+1
             | Formula.AxDecl (_, _) -> quantified_array := !quantified_array+1);
            { env with uncontrolled = add_decl_to_layer env.uncontrolled d }
          end
        | Comment _ -> env
      ) fm env
    in
    (* Formula_options.Logger.warning "%a" pp_env env; *)
    let content = mk_bl_imply env.uncontrolled.assumptions env.uncontrolled.assertions in
    let content = add_defs content env.uncontrolled in
    let content_smtlib = Formula_to_smtlib.bl_term content in
    let forall_vars = quantify_declarations env.uncontrolled.decls in
    let forall_smtlib = if forall_vars = [] then begin
        if (env.uncontrolled.assertions <> mk_bl_true || env.uncontrolled.assumptions <> mk_bl_true || env.uncontrolled.decls <> [] || env.uncontrolled.defs <> [])
        then Formula_options.Logger.fatal "uncontrolled layer has not declarations but is not empty %a" pp_env env;
        content_smtlib
      end else Smtlib_utils.mk_term_forall_term forall_vars content_smtlib 
    in
    let exists_assume = Formula_to_smtlib.bl_term (mk_bl_and env.controlled.assertions env.controlled.assumptions) in
    let exists_content = mk_smtlib_bin "and" exists_assume forall_smtlib in
    let exists_content = add_defs_smtlib exists_content env.controlled in
    let exists_vars = quantify_declarations env.controlled.decls in
    let exists_smtlib = if exists_vars = []
      then exists_content
      else Smtlib_utils.mk_term_exists_term exists_vars exists_content in
    let param_content = mk_smtlib_bin "and" (Formula_to_smtlib.bl_term env.parameters.assertions) exists_smtlib in
    let param_content = mk_smtlib_bin "=>" (Formula_to_smtlib.bl_term env.parameters.assumptions) param_content in
    let param_content = add_defs_smtlib param_content env.parameters in
    let param_vars = quantify_declarations env.parameters.decls in
    let script_commands = if param_vars = [] then
        let forall_assertion = Smtlib_utils.mk_command (Smtlib.CmdAssert forall_smtlib) in
        let prefix =
          List.map mk_declare env.controlled.decls
          @ List.rev_map mk_define env.parameters.defs
          @ List.rev_map mk_define env.controlled.defs
          @ (List.map mk_assert [env.parameters.assumptions; env.parameters.assertions; env.controlled.assumptions; env.controlled.assertions]) in
        (Formula_to_smtlib.formula ~logic:None (List.fold_left (fun acc elt -> push_front elt acc) empty prefix)).Smtlib.script_commands @ [forall_assertion]
      else
        let assert_content = Smtlib_utils.mk_term_forall_term param_vars param_content in
        let cmd = Smtlib_utils.mk_command (Smtlib.CmdAssert assert_content) in
        [cmd]
    in
    (* add executability *)
    let script_commands =
        let assertion = Smtlib_utils.mk_command (Smtlib.CmdAssert (executability env)) in
        assertion::script_commands
    in
    (* let script_commands = (Smtlib_utils.mk_command (Smtlib.CmdComment (Format.asprintf "%a" Formula_pp.pp_formula fm)))::script_commands in *)
    let res = Smtlib.{script_commands; script_loc = Location.dummy_loc } in
    (match start_time with
     | None -> ()
     | Some time -> Stats.add_universal_call (Unix.gettimeofday () -. time) ~quantified_array:!quantified_array ~quantified_bool:!quantified_bool ~quantified_bv:!quantified_bv);
    res
end

module ReadOverWrite = Make(ReadOverWriteInner)

let apply_if cond f x = if cond then f x else x

let optimize ?(keep=VarSet.empty)
    ?lst ?(cst=true) ?(itv=false) ?(prn=true) ?(rbs=true) ?(row=true) ?(ssa=true) ?(ai=false) ?(unfold=true) fm =
    fm
    |> apply_if prn (prune_and_inline ~keep)
    |> apply_if (row || ai) move_asserts_early
    |> apply_if ai (AI.simplify_formula unfold)
    |> apply_if (cst && (not ai)) (ConstantPropagation.simplify_formula (Some(keep)))
    |> apply_if ssa static_single_assignment
    |> apply_if row (ReadOverWrite.(simplify_formula {lst; rbs; itv; unfold;}))
    |> apply_if (ssa && (row || ai)) static_single_assignment
    |> apply_if (prn && (row || ai)) (prune_and_inline ~keep)
    |> apply_if (cst && (row || ssa || ai)) (ConstantPropagation.simplify_formula (Some(keep)))
    |> apply_if (prn && (cst || row || ssa || ai)) (prune_and_inline ~keep)

let optimize_from_options ?(keep=VarSet.empty) fm =
  let open Formula_options in
  let cst = OptimAll.get () || OptimCst.get () in
  let itv = OptimAll.get () || OptimItv.get () in
  let prn = OptimAll.get () || OptimPrn.get () in
  let rbs = OptimAll.get () || OptimRbs.get () in
  let row = OptimAll.get () || OptimRow.get () in
  let ssa = OptimAll.get () || OptimSsa.get () in
  let ai  = OptimAll.get () || OptimAi.get() in
  let unfold  = OptimAll.get () || OptimRowUnfold.get() in
  let lst =
    let i = OptimLst.get () in
    if i = 0 then None else Some i
  in optimize ~keep ?lst ~cst ~itv ~prn ~rbs ~row ~ssa ~ai ~unfold fm

let to_universal_formula ?(taintrow=false) ?(unfold=false) is_controlled fm =
  (if taintrow then taint2 ~unfold else taint) is_controlled fm

let to_universal ?(control_mem=false) ?(control_mem_approx=false) ?(quantifier=false) ?taintrow optimize is_controlled is_parameter fm =
  if quantifier
  then 
    let is_controlled, fm = if control_mem
      then Forall.make_memory_controlled control_mem_approx is_controlled fm
      else is_controlled, fm
    in
    Forall.to_universal is_controlled is_parameter (optimize fm)
  else Formula_to_smtlib.formula (to_universal_formula ?taintrow is_controlled fm)

let to_universal_from_options ?keep is_controlled is_parameter fm =
  let taintrow = Formula_options.TaintRow.get() in
  let unfold = Formula_options.OptimRowUnfold.get() in
  let quantifier = Formula_options.UniversalMode.get() in
  let control_mem = Formula_options.UnquantifyMemory.get() in
  let approx = Formula_options.UnquantifyMemoryApprox.get() in
  let start_time = Unix.gettimeofday () in
  let fm = optimize_from_options ?keep fm in
  match quantifier with
  | Formula_options.Quantifier ->
    let is_controlled, fm = if control_mem
      then Forall.make_memory_controlled approx is_controlled fm
      else is_controlled, fm
    in
    let fm, _selectors, is_controlled = if Formula_options.InvertExistentials.get() then
        synthetise_solutions_with_function_inversion ~complete:(not (Formula_options.InvertExistentialsIncomplete.get())) is_parameter is_controlled fm
      else
        fm, VarSet.empty, is_controlled
    in
    Forall.to_universal ~start_time is_controlled is_parameter (optimize_from_options ?keep fm)
  | Formula_options.Taint ->
    let res = fm
              |> to_universal_formula ~taintrow ~unfold is_controlled
              |> optimize_from_options ?keep 
              |> Formula_to_smtlib.formula ~logic:None
    in
    Stats.add_normal_call (Unix.gettimeofday () -. start_time);
    res


let optimize_from_options ?keep fm =
  let time = Unix.gettimeofday () in
  let res = optimize_from_options ?keep fm in
  Stats.add_normal_call (Unix.gettimeofday () -. time);
  res

