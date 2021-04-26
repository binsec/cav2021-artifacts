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

let byte_size = Natural.to_int Basic_types.Constants.bytesize

module F = struct
  let full name index = name ^ "_" ^ (string_of_int index)
  let memory = "__memory"

  let full_mem = full memory
  let memory_type word_size = Formula.ax_sort word_size byte_size

  let pc = "__pc"
  let full_pc = full pc

  let pa = "__pa"
  let full_pa = full pa

  let var name =
    let open Formula in
    function
    | BlSort       -> BlVar (bl_var name)
    | BvSort i     -> BvVar (bv_var name i)
    | AxSort (i,j) -> AxVar (ax_var name i j)
  ;;

  let decl =
    let open Formula in
    function
    | BlVar v -> mk_bl_decl v []
    | BvVar v -> mk_bv_decl v []
    | AxVar v -> mk_ax_decl v []
  ;;

  let def value var =
    let open Formula in
    match value.term_desc, var with
    | BlTerm value, BlVar v -> mk_bl_def v [] value
    | BvTerm value, BvVar v -> mk_bv_def v [] value
    | AxTerm value, AxVar v -> mk_ax_def v [] value
    | _ -> failwith "F.def has incompatible types"
  ;;

end

(* global mutable state to assign unique indices *)
let next_index = ref 0;;
let mk_index () =
  next_index := !next_index + 1;
  !next_index

(* variable bindings but one per branch *)
module State = struct
  module S = Basic_types.String.Map
  module B = Bitvector.Collection.Map
  type infos = {
    current_index: int option; (** index of current value, unless stripped *)
    first_index: int option; (** index of declaration, if the variable was declared *)
    sort: Formula.sort;
    controlled: Dba.Non_deterministic_tag.control;
  }

  type simpl_env = {
    row: Formula_transformation.AI.env;
  }
  type t = {
    fml : Formula.formula; (** a formula with only declarations and definitions *)
    var_infos : infos S.t; (** maps a DBA variable name to the its current value
                               in fml: the index to append to the variable name
                               to get the SMT variable name, and its type *)
    initialisation : int B.t; (** list of memory locations to initialize retroactively *)
    simpl: simpl_env option; (** if None, no simplification *)
    unsat: bool; (* true if simplifications prove that the formula is unsat *)
  }

  let pp ppf state =
    let open Format in
    fprintf ppf
    "@[<v 0># State var_infos @ @[<hov 0>%a@]@]"
    (fun ppf m -> S.iter (fun name {current_index; first_index; sort; controlled} -> fprintf ppf "%a %a %s: %a -> %a;@ " Dba.Non_deterministic_tag.pp_control controlled Formula_pp.pp_sort sort name (Print_utils.pp_opt Format.pp_print_int) first_index (Print_utils.pp_opt Format.pp_print_int) current_index) m)
    state.var_infos


  let add_entry entry state =
    let open Formula_transformation in
    let simpl, entry = match state.simpl with
      | None -> None, Some(entry)
      | Some(e) ->
        let row, entry = AI.process_entry e.row entry in
        let e = { row; } in
        Some(e), entry
    in
    let unsat, fml = match entry with
      | None -> state.unsat, state.fml
      | Some(entry) ->
        let unsat = state.unsat || match entry.Formula.entry_desc with
          | Formula.Define({Formula.def_desc=Formula.BlDef(v, [], {Formula.bl_term_desc=Formula.BlFalse;_}); _}) when String.sub v.Formula.bl_name 0 5 = "__pc_" -> true
          | Formula.Assert({Formula.bl_term_desc=Formula.BlFalse;_}) -> true
          | _ -> false in
        unsat, Formula.push_front entry state.fml
    in
    { state with fml; simpl; unsat; }

  let default_control () = Dba.Non_deterministic_tag.(if Sse_options.AllControlled.get() then Controlled else Uncontrolled)
  let min_control = function
      | Dba.Non_deterministic_tag.Uncontrolled when Sse_options.AllControlled.get() -> Dba.Non_deterministic_tag.Controlled
      | x -> x


  let declare ~controlled name var_type state =
    let open Formula in
    if S.mem name state.var_infos then failwith "variable already declared";
    let controlled = min_control controlled    in
    let index = mk_index () in
    let var = F.var (F.full name index) var_type in
    let declaration = F.decl var  in
    let value = { current_index = Some index; first_index = Some index; sort=var_type; controlled} in
    let var_infos = S.add name value state.var_infos in
    add_entry (mk_declare declaration) { state with var_infos }


  let assign name var_type value state =
    let open Formula in
    let current_index = mk_index () in
    let controlled, first_index = match S.find name state.var_infos with
      | {sort=vtype; _} when vtype <> var_type ->
        Sse_options.Logger.fatal "State.assign of %s has type %a when %a was expected"
          name Formula_pp.pp_sort vtype Formula_pp.pp_sort var_type
      | {controlled; first_index; current_index; _} ->
        if current_index = None
        then Sse_options.Logger.fatal "Tried to assigned on top of stripped var %s" name
        else controlled, first_index;
      | exception Not_found -> (default_control()), None
    in
    let var = F.var (F.full name current_index) var_type in
    let definition = F.def value var in
    let value = { current_index = Some current_index; first_index; sort=var_type; controlled} in
    let var_infos = S.add name value state.var_infos in
    add_entry (mk_define definition) { state with var_infos }

  let create () =
    let open Formula in
    let simpl = if Sse_options.LoadROSections.get() || Sse_options.LoadSections.is_set() || (not (Sse_options.OnTheFlyAI.get())) then None else
        let open Formula_transformation in
        Some({
            row = AI.create (Formula_options.OptimRowUnfold.get());
          }) in
    let memory_type = F.memory_type (Kernel_options.Machine.word_size ()) in
    let fml = Formula.empty
    and var_infos = if simpl = None then S.singleton F.memory
          {first_index = Some 0; current_index = Some 0; sort= memory_type; controlled=default_control ();}
      else S.empty in
    let initialisation = B.empty in
    { fml; var_infos ; initialisation; simpl; unsat = false; } |>
    (* Add path assumptions *)
    assign F.pa bl_sort (mk_bl_term mk_bl_true) |>
    (* Add path assertions *)
    assign F.pc bl_sort (mk_bl_term mk_bl_true) |>
    (if simpl = None then fun x -> x else declare ~controlled:(default_control()) F.memory memory_type)

  let initializations st = st.initialisation

  let comment cmt state =
    add_entry (Formula.mk_comment cmt) state

  let unsat state = state.unsat

  let get_last_index state name var_type =
    match S.find name state.var_infos with
    | {sort=v; _} when v <> var_type ->
      Sse_options.Logger.fatal "State.get_last_index of %s has type %a when %a was expected"
        name Formula_pp.pp_sort v Formula_pp.pp_sort var_type
    | {current_index=None;_} -> Sse_options.Logger.fatal "Getting last index of %s on stripped symbolic state" name
    | {current_index=Some(n);_} -> n

  let get_memory state =
    let word_size = Kernel_options.Machine.word_size () in
    let index = get_last_index state F.memory (F.memory_type word_size) in
    let name = F.full_mem index in
    Formula.(mk_ax_var (ax_var name word_size byte_size))

  let get_path_constraint state =
    let index = get_last_index state F.pc Formula.bl_sort in
    let name = F.full_pc index in
    Formula.(mk_bl_var (bl_var name))
  ;;

  let get_path_assumption state =
    let index = get_last_index state F.pa Formula.bl_sort in
    let name = F.full_pa index in
    Formula.(mk_bl_var (bl_var name))
  ;;

  let constrain ?(assumption=false) cond state =
    let open Formula in
    let constraint_name, c =
      if assumption then F.pa, get_path_assumption state
      else F.pc, get_path_constraint state in
    let cterm = mk_bl_and c cond |> mk_bl_term in
    assign constraint_name bl_sort cterm state
  ;;

  let get_bv name size state =
    let sort = Formula.bv_sort (Size.Bit.to_int size) in
    let (index, state'') = match get_last_index state name sort with
      | n -> (n, state)
      | exception Not_found -> 
        let state' = declare ~controlled:(default_control()) name sort state in
        (get_last_index state' name sort, state')
    in
    let name = F.full name index in
    let value = Formula.(mk_bv_var (bv_var name (Size.Bit.to_int size))) in
      (value, state'')

  (** remove all variables except pa/pc *)
  (* Use case: after strip, merge will not create any ite terms. We need to
   *)
  let strip state =
    let var_infos = S.mapi (fun name info -> if 
  (name = F.pc) || (name = F.pa) then info else { info with current_index = None; }) state.var_infos in
    {state with var_infos}

  let merge s1 s2 =
    let open Seq in
    let open Formula in
    (* je n'ai pas envie de m'occuper de ce cas *)
    assert (initializations s1 |> B.is_empty);
    assert (initializations s2 |> B.is_empty);
    (* invariant on formulas:
     * if two entries are common to two path_states, then they are in the same
     * order *)
    let rec merge_fml seq1 seq2 acc = match (seq1 ()), (seq2 ()) with
      | Nil, Nil -> acc
      | Nil, Cons(e2, n2) -> merge_fml seq1 n2 (push_front e2 acc)
      | Cons(e1, n1), Nil -> merge_fml n1 seq2 (push_front e1 acc)
      | Cons(e1, n1), Cons(e2, n2) ->
        let acc = push_front e1 acc in
        if e1 = e2
        then merge_fml n1 n2 acc
        else merge_fml n1 seq2 acc
    in
    let merged_fml = merge_fml (Sequence.to_seq_forward s1.fml.entries) (Sequence.to_seq_forward s2.fml.entries) Formula.empty in
    let merged = {
      fml = merged_fml;
      var_infos = s1.var_infos;
      simpl = None;
      initialisation = B.empty;
      unsat = false;
    } in
    (* update the variable definitions to make ite *)
    let pc1 = get_path_constraint s1 in
    let has_ax_ite = ref false in
    let update var info1 merged =
      match S.find_opt var s2.var_infos with
      | None -> merged
      | Some(_) when info1.current_index = None -> merged
      | Some({current_index=None; _}) -> merged
      | Some(info2) ->
        assert (info1.sort = info2.sort);
        assert (info1.controlled = info2.controlled);
        if info1.first_index != info2.first_index then
          Sse_options.Logger.fatal "Cannot merge variable %s declared at index %a and %a"
            var
            (Print_utils.pp_opt Format.pp_print_int) info1.first_index
            (Print_utils.pp_opt Format.pp_print_int) info2.first_index;
        if info1.current_index = info2.current_index
        then merged
        else
          let v1 = F.full var (Utils.unsafe_get_opt info1.current_index) in
          let v2 = F.full var (Utils.unsafe_get_opt info2.current_index) in
          let term = match info1.sort with
            | Formula.BlSort ->
              let v1 = Formula.bl_var v1 |> Formula.mk_bl_var in
              let v2 = Formula.bl_var v2 |> Formula.mk_bl_var in
              let v = mk_bl_ite pc1 v1 v2 in
              Formula.mk_bl_term v
            | Formula.BvSort n ->
              let v1 = Formula.bv_var v1 n |> Formula.mk_bv_var in
              let v2 = Formula.bv_var v2 n |> Formula.mk_bv_var in
              let v = mk_bv_ite pc1 v1 v2 in
              Formula.mk_bv_term v
            | Formula.AxSort (n, m) ->
              has_ax_ite := true;
              let v1 = Formula.ax_var v1 n m |> Formula.mk_ax_var in
              let v2 = Formula.ax_var v2 n m |> Formula.mk_ax_var in
              let v = mk_ax_ite pc1 v1 v2 in
              Formula.mk_ax_term v
          in
          assign var info1.sort term merged
    in
    let res = S.fold update s1.var_infos merged in
    let fml = if !has_ax_ite then Formula_transformation.remove_ax_ite res.fml else res.fml in
    let res = { res with fml } in
    let declare_from other acc =
      S.fold (fun var {current_index=_; first_index; sort; controlled} acc ->
          if S.mem var acc.var_infos then acc else
            match first_index with
            | None -> acc
            | Some(_) ->
            let var_infos = S.add var {first_index; sort; controlled; current_index=None} acc.var_infos in
            { acc with var_infos }
        ) other.var_infos acc
    in
    res
    |> declare_from s1
    |> declare_from s2

  (* FIXME reenable on the fly simplification *)

  let init_mem_at ~addr ~size state =
    assert(state.simpl = None);
    { state with initialisation = B.add addr size state.initialisation }

(** load [size] bytes in image at the specified address and return the new memory *)
  let load_memory_from_image addr size mem =
    let word_size = Kernel_options.Machine.word_size () in
    assert (word_size = Bitvector.size_of addr);
    let read_bitvector addr sz =
      let b = Buffer.create (2 * sz) in
      (* The loop below is little-endian *)
      let rec loop offset =
        if offset < 0 then
          let v = Bigint.big_int_of_string ("0x" ^ Buffer.contents b) in
          Bitvector.create v (byte_size * sz)
        else
          let off_bv = Bitvector.of_int ~size:word_size offset in
          let load_addr = Bitvector.add addr off_bv in
          let img = Kernel_functions.get_img () in
          let byte = Loader_utils.get_byte_at img load_addr in
          let byte_str = Format.sprintf "%02x" byte in
          Buffer.add_string b byte_str;
          loop (offset - 1)
      in loop (sz - 1)
    in
    Formula.(mk_store size mem (mk_bv_cst addr) (mk_bv_cst (read_bitvector addr size)))

(** compute the initial memory term from [state.initialisation] *)
  let initial_memory_value state =
    let open Formula in
    let word_size = Kernel_options.Machine.word_size () in
    let symbolic_memory = mk_ax_var (ax_var F.memory word_size byte_size) in
    B.fold load_memory_from_image state.initialisation symbolic_memory

  (** like a regular memory assignment, but with a value from the image *)
  let assign_from_image ~addr ~size state =
    let word_size = Kernel_options.Machine.word_size () in
    let mem = get_memory state in
    let new_mem = load_memory_from_image addr size mem in
    assign F.memory (F.memory_type word_size) (Formula.mk_ax_term new_mem) state

  let get_entries state =
    if state.simpl <> None then state.fml else
    let open Formula in
    let word_size = Kernel_options.Machine.word_size () in
    let var = F.(var memory (memory_type word_size)) in
    let declaration = F.decl var in
    let definition =
      F.var (F.full_mem 0) (F.memory_type word_size)
      |> F.def (mk_ax_term (initial_memory_value state)) in
    state.fml
    |> Formula.push_back_define definition
    |> Formula.push_back_declare declaration


  let formula state =
    Formula.push_front_assume (get_path_assumption state) (get_entries state)
    |> Formula.push_front_assert (get_path_constraint state)
  ;;

  let vars_with c st = 
    let only_those = S.filter (fun _ {controlled; _} -> controlled = c) st.var_infos in
    S.fold (fun name {first_index; sort; _} acc -> match first_index with
          None -> acc
        | Some n -> Formula.VarSet.add (F.var (F.full name n) sort) acc) only_those Formula.VarSet.empty
  let controlled_vars st = vars_with Dba.Non_deterministic_tag.Controlled st
  let uncontrolled_vars st = vars_with Dba.Non_deterministic_tag.Uncontrolled st
  let parameter_vars st = vars_with Dba.Non_deterministic_tag.Parameter st

  let memory_term fml =
    F.memory, F.memory_type (Kernel_options.Machine.word_size ()), Formula.mk_ax_term fml

end
