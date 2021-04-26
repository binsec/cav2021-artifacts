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

open Sse_types
open Sse_options

(* Enumerate jumps targets *)
let get_entry_point () =
  match Kernel_functions.get_ep () with
  | Some v -> v
  | None ->
    Kernel_functions.get_img ()
    |> Loader.Img.entry
    |> Virtual_address.create

module type SSE_RUNNER = sig val start: unit -> unit end

module Env_make(G: GLOBAL_ENV): SSE_RUNNER =
struct
  module Stats = struct
    type t = {
      paths : int;
      forks : int; (* number of dba instructions creating a branch *)
      instructions : int; (* processor instructions executed *)
      dba_instructions : int;
      asserts_unknown : int;
      asserts_failed : int;
      merging_not_even_started : int; (* number of times we wanted to start a merge but no merge point was found *)
      merging_started : int;
      merging_completed : int;
      merged_paths : int;
      merging_time : float; (* total time needed to merge the [merged_paths] paths *)
      covered_instructions : Virtual_address.Set.t;
      covered_dba_instructions: Dba_types.Caddress.Set.t;
    }

    let empty = {
      paths = 1;
      forks = 0;
      instructions = 0;
      dba_instructions = 0;
      covered_instructions = Virtual_address.Set.empty;
      covered_dba_instructions = Dba_types.Caddress.Set.empty;
      asserts_unknown = 0;
      asserts_failed = 0;
      merging_not_even_started = 0;
      merging_started = 0;
      merging_completed = 0;
      merged_paths = 0;
      merging_time = 0.;
    }

    let add_path s = { s with paths = s.paths + 1 }
    let add_fork  s = { s with forks = s.forks + 1 }
    let add_instruction  s vaddr =
      { s with
        instructions = s.instructions + 1;
        covered_instructions = Virtual_address.Set.add vaddr s.covered_instructions;
      }
    let add_dba_instruction  s addr = 
      { s with
        dba_instructions = s.dba_instructions + 1;
        covered_dba_instructions = Dba_types.Caddress.Set.add addr s.covered_dba_instructions;
      }
    let add_assert_unknown s = { s with asserts_unknown = s.asserts_unknown + 1 }
    let add_assert_failed  s = { s with asserts_failed = s.asserts_failed + 1 }
    let add_merging_not_even_started  s = { s with merging_not_even_started = s.merging_not_even_started + 1 }
    let start_merging  s = { s with merging_started = s.merging_started + 1 }
    let complete_merging  s = { s with merging_completed = s.merging_completed + 1 }
    let add_merged_path s time = { s with
                                   merged_paths = s.merged_paths + 1;
                                   merging_time = s.merging_time +. time;
                                 }

    let pp ppf s complete =
      Format.fprintf ppf
        "@[<v 0>\
         @[<h>exploration was complete %b@]@,\
         @[<h>paths %d@]@,\
         @[<h>forks %d@]@,\
         @[<h>executed instructions %d@]@,\
         @[<h>executed dba instructions %d@]@,\
         @[<h>covered instructions %d@]@,\
         @[<h>covered dba instructions %d@]@,\
         @[<h>unknown assertions %d@]@,\
         @[<h>failed assertions %d@]@,\
         @[<h>merging not even started %d@]@,\
         @[<h>merging started %d@]@,\
         @[<h>merging completed %d@]@,\
         @[<h>merged paths %d@]@,\
         @[<h>total merging time %f@]@,\
         @[<h>avg merging time %f@]\
         @]"
        complete
        s.paths
        s.forks
        s.instructions
        s.dba_instructions
        (Virtual_address.Set.cardinal s.covered_instructions)
        (Dba_types.Caddress.Set.cardinal s.covered_dba_instructions)
        s.asserts_unknown
        s.asserts_failed
        s.merging_not_even_started
        s.merging_started
        s.merging_completed
        s.merged_paths
        s.merging_time
        (s.merging_time /. (float_of_int s.merged_paths))
    ;;

    let pp_json ppf s complete =
      Format.fprintf ppf
       {|@[<v 0>{
         @[<h>"complete": %b@],@,
         @[<h>"paths": %d@],@,
         @[<h>"forks": %d@],@,
         @[<h>"instructions": %d@],@,
         @[<h>"dba_instructions": %d@],@,
         @[<h>"covered_instructions": %d@],@,
         @[<h>"covered_dba_instructions": %d@],@,
         @[<h>"unknown_assertions": %d@],@,
         @[<h>"failed_assertions": %d@],@,
         @[<h>"merging_not_even_started": %d@],@,
         @[<h>"merging_started": %d@],@,
         @[<h>"merging_completed": %d@],@,
         @[<h>"merging_time": %f@],@,
         @[<h>"merged_paths": %d@]
         }@] |}
        complete
        s.paths
        s.forks
        s.instructions
        s.dba_instructions
        (Virtual_address.Set.cardinal s.covered_instructions)
        (Dba_types.Caddress.Set.cardinal s.covered_dba_instructions)
        s.asserts_unknown
        s.asserts_failed
        s.merging_not_even_started
        s.merging_started
        s.merging_completed
        s.merging_time
        s.merged_paths
    ;;


    module R = struct
      let value = ref empty
      let add_fork () = value := add_fork !value
      let add_path  () = value := add_path !value
      let add_instruction vaddr = value := add_instruction !value vaddr
      let add_dba_instruction caddr = value := add_dba_instruction !value caddr
      let add_assert_unknown () = value := add_assert_unknown !value
      let add_assert_failed  () = value := add_assert_failed !value
      let add_merging_not_even_started  () = value := add_merging_not_even_started !value
      let add_merged_path time = value := add_merged_path !value time
      let start_merging  () = value := start_merging !value
      let complete_merging () = value := complete_merging !value
      let pp ppf complete = pp ppf !value complete
      let pp_json ppf complete = pp_json ppf !value complete
    end
    include R
  end

  module Env = struct
    type t = {
      global : G.t;
      local  : Path_state.t;
      cfg : C.t;
      entrypoint : Virtual_address.t ;
    }

    let create ~global ~local =
      { global;
        local;
        cfg = Path_state.cfg local;
        entrypoint = Path_state.entrypoint local;
      }

    let local e = e.local
    let global e = e.global

    let of_global g =
      let global, local = G.Path.choose g in
      create ~global ~local

    let mark_as_incomplete e = G.mark_as_incomplete e.global

    let current_address e =
      Path_state.virtual_address @@ local e

    let goals_here e =
      G.Directives.at (current_address e) (global e)

    let choice_goals_here e =
      match goals_here e with
      | None -> []
      | Some q ->
        Queue.fold
          (fun l d -> if Directive.is_choice d then d :: l else l) [] q

    let pick_path e = of_global (global e)

    let check_sat ~explore e =
      if Sse_symbolic.State.unsat (Path_state.symbolic_state e.local) then
        begin
          Logger.debug ~level:1 "Branch at %a is UNSAT by simplifications" Path_state.pp_loc e.local;
          Sse_smt.Robust_status.const Formula.UNSAT, Some false, e
        end
      else
        let res, local = Sse_smt.Solver.check_satistifiability ~explore e.local in
        let e = { e with local } in
        let merging = Path_state.merge_point e.local <> None in
        let simple_result =
          let open Sse_smt.Robust_status in
          if merging then translate ~universal:false res else
            if explore then continue_exploration res
            else accept_reach res
        in
        if simple_result = None then mark_as_incomplete e;
        res, simple_result, e

    let pick_alternative ~consequent ~alternative e =
      Stats.add_path ();
      let do_pick ~first ~second =
        let global = G.(Path.add first e.global |> Path.add second) in
        pick_path { e with global }
      in
      let open Directive in
      match choice_goals_here e with
      | d :: ds ->
        if ds <> [] then
          Logger.warning "@[<h>%@ Ignoring %d choice directives@]"
            (List.length ds);
        begin match directive d with
          | Choice c ->
            let first, second =
              if Choice.is_alternative c then alternative, consequent
              else consequent, alternative in
            Choice.do_alternate c;
            do_pick ~first ~second
          | _ -> assert false
        end
      | [] ->
        let first, second =
          if Sse_options.Randomize.get () && Random.bool ()
          then alternative, consequent
          else consequent, alternative in
        do_pick ~first ~second
  end

  let printed_stats = ref false;;

  let print_stats complete =
    if !printed_stats then () else begin
      printed_stats := true;
      Logger.info
        "@[<v 0>\
         @[<v 2>SMT queries@,%a@]@,\
         @[<v 2>Simplification passes@,%a@]@,\
         @[<v 2>Exploration@,%a@]@,\
         @]"
        Sse_smt.Query_stats.pp ()
        Formula_transformation.Stats.pp ()
        Stats.pp complete;
      begin match Sse_options.StatsFile.get() with
        | None -> ()
        | Some(outc) ->
          Format.fprintf
            (Format.formatter_of_out_channel outc)
            {|{"sse": %a, "solver": %a, "formula_transformation": %a}|}
            Stats.pp_json complete
            Sse_smt.Query_stats.pp_json ()
            Formula_transformation.Stats.pp_json ()
      end
    end
  ;;

  let halt e =
    if (Robust.get()) && (RobustMode.get() = Validation) && not (RobustIncremental.get()) then
      begin
        Logger.info "@[<h>Non-incremental mode: examining deffered reaches @]";
        let p = G.drain_merged_paths (Env.global e) in
        match p with
        | None ->
          Logger.warning "@[<h>Directive :: did not reach: exploration never reached a goal @]"
        | Some p -> begin
            let status, p = Sse_smt.Solver.check_satistifiability ~explore:false p in
            let simple_status = Sse_smt.Robust_status.accept_reach status in
            match simple_status with
            | Some true -> begin
                Logger.result
                  "@[<h>Directive :: reached a goal @]";
                Sse_types.Path_state.dump_path p;
                let model = if Formula.VarSet.is_empty (Sse_symbolic.State.parameter_vars (Path_state.symbolic_state p)) then
                    Sse_smt.Solver.get_model ~universal:(Robust.get()) p
                  else None
                in
                (match model with
                 | Some m ->
                   Logger.result "@[<v 0>Model@ %a@]"
                     Smt_model.pp m;
                 | None ->
                   Logger.result
                     "@[<h>No model @]");
              end
            | _ -> 
              begin
                Logger.warning "@[<h>Directive :: did not reach: path constraint is not satisfiable %a @]"
                  Sse_smt.Robust_status.pp status;
                if simple_status = None then Env.mark_as_incomplete e;
              end
          end
      end;
    print_stats (G.complete (Env.global e));
    if Sse_options.Dot_filename_out.is_set () then
      let filename = Sse_options.Dot_filename_out.get () in
      Logger.info "Outputting CFG in %s" filename;
      let oc = open_out_bin filename in
      let cfg = e.Env.cfg in
      begin
        match C.mem_vertex_a cfg e.Env.entrypoint with
        | None -> ()
        | Some entry -> C.output_graph oc e.Env.cfg [] ~entry
      end;
      close_out oc

  module Eval = struct

    let static_jump ~jump_target global path_state =
      match jump_target with
      | Dba.JInner idx ->
        Some (Path_state.set_block_index idx path_state)
      | Dba.JOuter addr ->
        let vaddr = Dba_types.Caddress.to_virtual_address addr in
        Logger.debug ~level:5 "Jumping to new address %a"
          Sse_types.vaddr_pp vaddr;
        let open Sse_types.Path_state in
        match Path_state.counter vaddr path_state with
        | Some c ->
          begin match Address_counter.check_and_decr c with
            | Some c ->
              let p =
                Path_state.set_counter vaddr c path_state
                |> goto_vaddr vaddr in
              Some p
            | None ->
              Logger.info "Cutting path at address %a : we reached the limit ..."
                Sse_types.vaddr_pp vaddr;
              G.mark_as_incomplete global;
              None
              (* Could not decrement counter: should stop *)
          end
          (* no limit on the number of visits of this address *)
        | None -> Some (goto_vaddr vaddr path_state)

    (* lvalue <- e *)
    let assignment ~lvalue ~rvalue e =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      let open Sse_smt in
      Env.{ e with local = Translate.assignment lvalue rvalue e.local }

    let nondet ~lvalue ~tag e =
      (* generate the logical constraint, add it to the path predicate,
       * update symbolic_state.
       *)
      let open Sse_smt in
      Env.{ e with
            local =
              let open Dba.Non_deterministic_tag in
              let controlled = controlled tag in
              let naming_hint = name tag in
              Translate.nondet ?naming_hint lvalue ~controlled e.local }


    let ite ~condition ~jump_target ~local_target e =
      (* expand path with assert condition and go to jump_target *)
      (* push path with assert not condition and go to local_target *)
      Stats.add_fork();
      let old_state = Env.local e in
      let global = Env.global e in
      let raw_cond, state = Sse_smt.Translate.expr' old_state condition in
      let condition = Formula.(mk_bv_equal raw_cond mk_bv_one) in
      let alternate_condition = Formula.mk_bl_not condition in
      let consequent =
        Path_state.add_assertion condition state |> static_jump ~jump_target global
      in
      let alternate_state = Path_state.branch state in
      let alternative =
        Path_state.add_assertion alternate_condition alternate_state
        |> Sse_types.Path_state.set_block_index local_target
      in
      match consequent with
      | None -> Env.{ e with local = alternative } (* other branch goes over depth *)
      | Some(consequent) ->
        let open Sse_smt.Robust_status in
        let open Formula in
        let easy_unsat p = Sse_symbolic.State.unsat (Path_state.symbolic_state p) in
        if easy_unsat consequent then
          begin
            Logger.debug ~level:2 "True branch %a is UNSAT by simplifications" Path_state.pp_loc consequent;
            Env.{e with local = alternative;}
          end
        else if easy_unsat alternative then
          begin
            Logger.debug ~level:2 "False branch %a is UNSAT by simplifications" Path_state.pp_loc alternative;
            Env.{e with local = consequent;}
          end
        else
        if Yolo.get() then
          Env.pick_alternative ~consequent ~alternative e
        else
          let c_status, c_simple_status, c_env = Env.check_sat ~explore:true Env.{e with local = consequent;} in
          let a_status, a_simple_status, a_env = Env.check_sat ~explore:true Env.{e with local = alternative;} in
          Logger.debug ~level:3 "branch %a is %a->%a %a->%a"
            Path_state.pp_loc alternative
            Sse_smt.Robust_status.pp c_status
            (Print_utils.pp_opt Format.pp_print_bool) c_simple_status
            Sse_smt.Robust_status.pp a_status
            (Print_utils.pp_opt Format.pp_print_bool) a_simple_status
          ;
          match c_simple_status, a_simple_status with
          | _, None when c_status.existential = UNSAT ->
            let alternative_without_assert = Sse_types.Path_state.set_block_index
                local_target alternate_state
            in
            Env.{ e with local = alternative_without_assert }
          | None, _ when a_status.existential = UNSAT ->
            let consequent_without_assert = static_jump ~jump_target global state in
            Env.{ e with local = Utils.unsafe_get_opt consequent_without_assert }
          | Some true, Some true ->
            Env.pick_alternative ~consequent:c_env.Env.local ~alternative:a_env.Env.local e
          | Some true, _ -> c_env
          | _, Some true -> a_env
          | Some(false), Some(false) when Sse_options.Robust.get() && Sse_options.RobustMode.get() = Exploration && Sse_options.RobustMerge.get() = OpportunisticMerge ->
            begin
              let merge_point = Sse_dom.compute_merge_point (Path_state.location e.Env.local) (Sse_options.MaxDepth.get()) in
              match merge_point with
              | None -> begin
                  Logger.info "No merge point at %a" Path_state.pp_loc e.Env.local;
                  Stats.add_merging_not_even_started ();
                  Env.pick_path e
                end
              | Some(a) -> begin
                  Stats.start_merging ();
                  Logger.info "split at %a, to be merged at %a" Path_state.pp_loc e.Env.local Dba_types.Caddress.pp_base a;
                  let set e = Env.{ e with local = Path_state.set_merge_point merge_point e.local } in
                  let c_env = set c_env in
                  let a_env = set a_env in
                  Env.pick_alternative ~consequent:c_env.Env.local ~alternative:a_env.Env.local e
                end
            end
          | _ -> Env.pick_path e


    let dynamic_jump ~jump_expr e =
      Stats.add_fork();
      let img = Kernel_functions.get_img () in
      let path_state = Env.local e in
      let target, path_state = Sse_smt.Translate.expr' path_state jump_expr in
      let n = Sse_options.JumpEnumDepth.get () in
      let concretes, path_state =
        Sse_smt.Solver.enumerate_values n target path_state in
      if List.length concretes = n then
        Env.mark_as_incomplete e;
      let printed_trace = ref false in
      let with_bv env bv =
        let condition = Formula.(mk_bv_equal (mk_bv_cst bv) target)
        and addr = Virtual_address.of_bitvector bv
        and invalid bv =
          Logger.warning
            "@[<hov>Dynamic jump@ %a@ could have led to invalid address %a;@ \
             skipping@]"
            Path_state.pp_loc path_state Bitvector.pp_hex bv;
          if not (!printed_trace) then begin
            printed_trace := true;
            Logger.warning "Trace to reach the jump:@ %a" Path_state.pp_path path_state
          end;
          env
        in
        Logger.debug ~level:4 "@[<hov>Dynamic jump@ %a@ could lead to@ %a@]"
          Path_state.pp_loc path_state Bitvector.pp_hex bv;
        let address = Virtual_address.to_int addr in
        let section = Loader_utils.find_section_by_address ~address img in
        match section with
        | Some s when
            Loader.Section.has_flag Loader_types.Read s &&
            Loader.Section.has_flag Loader_types.Exec s ->
          (* Important to add an assertion here, not an assumption,
           * especially because enumerate_values does not do tainting *)
          let ps = Path_state.add_assertion condition path_state in
          let ps = Sse_types.Path_state.goto_vaddr addr ps in
          Stats.add_path ();
          Env.{ env with global = G.Path.add ps env.global }
        | Some _ | None -> invalid bv
      in
      let env = List.fold_left with_bv e concretes in
      Env.pick_path env

  let check f cond idx e =
    let state = Env.local e in
    let cond, symbols =
      Sse_smt.Translate.expr (Path_state.symbolic_state state) cond in
    let state = Path_state.set_symbolic_state symbols state in
    let state = f cond state in
    { e with Env.local = Path_state.set_block_index idx state }

  let assertion test state =
    let result, _ = Sse_smt.Solver.check_satistifiability ~explore:false
      (Path_state.add_assertion
         Formula.(mk_bv_equal test Formula.mk_bv_zero) state)
    in
    begin
      match result.Sse_smt.Robust_status.existential with
      | Formula.UNSAT -> ()
      | Formula.UNKNOWN | Formula.TIMEOUT ->
        Sse_options.Logger.error
          "@[Assertion got unknown status %@ %a@]"
          Path_state.pp_loc state;
        Stats.add_assert_unknown ()
      | Formula.SAT ->
        Sse_options.Logger.error
          "@[<v 2> Assertion failed %@ %a@]"
          Path_state.pp_loc state;
          Stats.add_assert_failed ()
    end;
    state

    (* If comment is activated, this will add, for every formula entry, a
       comment about where it comes from.

       This can be useful to debug the path predicate translation.  *)
    let maybe_add_comment ps =
      if Sse_options.Comment.get () then
        let syst = Path_state.symbolic_state ps in
        let comment =
          Print_utils.string_from_pp Path_state.pp_loc ps
        in
        let symbolic_state = Sse_symbolic.State.comment comment syst in
        Path_state.set_symbolic_state symbolic_state ps
      else ps

    let undef_counter = ref 0
    let get_undef_counter () =
      let res = !undef_counter in
      undef_counter := res + 1;
      res

    let interpret_instruction instr e =
      Logger.debug ~level:6 "@[Evaluating@ %a@]" Dba_printer.Unicode.pp_instruction instr;
      match instr with
      | Dba.Instr.Assign (lvalue, rvalue, idx) ->
        let e = assignment ~lvalue ~rvalue e in
        Env.{ e with local = Path_state.set_block_index idx e.local }

      | Dba.Instr.Nondet(lvalue, tag,idx) ->
        let e = nondet ~lvalue ~tag e in
        Env.{ e with local = Path_state.set_block_index idx e.local }

      | Dba.Instr.Undef(lvalue, idx) ->
        let str = Format.asprintf "undef_%a_%a_%i"
            Dba_printer.Ascii.pp_lhs lvalue
            Dba_printer.Ascii.pp_code_address (Path_state.location e.Env.local)
            (get_undef_counter ())
        in
        (* remove special characters *)
        let name = String.map (fun c -> if
                                 ('a' <= c & c <= 'z') ||
                                 ('A' <= c & c <= 'Z') ||
                                 ('0' <= c & c <= '9')
                                 then c else '_') str
        in
        let tag = Dba.Non_deterministic_tag.create ~name ~controlled:Dba.Non_deterministic_tag.Uncontrolled () in
        let e = nondet ~lvalue ~tag e in
        Env.{ e with local = Path_state.set_block_index idx e.local }

      | Dba.Instr.SJump (jump_target, _) -> begin
          match static_jump ~jump_target e.Env.global e.Env.local with
          | None -> (* This jump has been forbidden *)
            Env.pick_path e
          | Some local -> {e with Env.local}
        end
      | Dba.Instr.If (condition, jump_target, local_target) ->
        ite ~condition ~jump_target ~local_target e

      | Dba.Instr.DJump (je, _) -> dynamic_jump ~jump_expr:je e
      | Dba.Instr.Stop reason as dba_instruction ->
        (* does stopping exploration here break completeness ? *)
        let legitimate_stop = match reason with
          | Some Dba.OK | Some (Dba.Undefined _)-> true
          | Some (Dba.Unsupported _) -> false
          | Some Dba.KO (* No idea of the actual semantics *)
          | None -> assert false
        in
        if not legitimate_stop then
          begin
            Logger.warning "Cutting branch on instruction %a at %a"
              Dba_printer.Ascii.pp_instruction dba_instruction
              Sse_types.vaddr_pp (Path_state.virtual_address (Env.local e));
            Env.mark_as_incomplete e
          end;
        (* Discard current path, choose a new one *)
        Env.pick_path e
      | Dba.Instr.Assert (condition, idx) ->
        check assertion condition idx e
      | Dba.Instr.Assume (condition, idx) ->
        Env.{e with local = Path_state.set_block_index idx (Sse_smt.Translate.assume condition e.local)}
      | Dba.Instr.NondetAssume _
      | Dba.Instr.Malloc _
      | Dba.Instr.Free _
      | Dba.Instr.Print _ as dba_instruction ->
        let msg =
          Format.asprintf "%a" Dba_printer.Ascii.pp_instruction dba_instruction in
        Errors.not_yet_implemented msg

    let go e =
      if not (Path_state.is_depth_ok e.Env.local) then
        begin
          Logger.info "Depth exceeded at %a" Path_state.pp_loc e.Env.local;
          Env.mark_as_incomplete e;
          Env.pick_path e
        end
      else if Some(Path_state.location e.Env.local) = Path_state.merge_point e.Env.local then
        begin
          Logger.info "Merging at %a" Path_state.pp_loc e.Env.local;
          let time = Unix.gettimeofday () in
          let res = G.merge_ready_path_state e.Env.global e.Env.local in
          Stats.add_merged_path (Unix.gettimeofday() -. time);
          match res with
          | None -> (Logger.info "not yet!"; Env.pick_path e)
          | Some(l) -> (Logger.info "ready!"; Stats.complete_merging (); Env.{e with local = l})
        end
      else
      let path_state = maybe_add_comment @@ Env.local e in
      let e = { e with Env.local = path_state } in
      Logger.debug ~level:5 "@[Evaluating@ %a@]" Path_state.pp_loc path_state;
      Stats.add_dba_instruction (Path_state.location path_state);
      interpret_instruction (Path_state.dba_instruction path_state) e
  end

  type path_directive =
    | Continue
    | Discard

  let reach_count = ref 0

  let loop_until ~halt e =
    let get_id e = 
      let ps = Env.local e in
      Path_state.(id ps, virtual_address ps)
    in
    let last_id = ref (get_id e) in

    (* run reloop to explore, and on each new instruction, call do_directives *)
    let rec loop_aux e =
      let id = get_id e in
      if id <> !last_id then begin
        (* we just reached a new instruction OR we just changed path, process directives *)
        let vaddr = snd id in
        Logger.debug ~level:2 "%@%a %a"
          Sse_types.vaddr_pp vaddr
          Mnemonic.pp (Env.local e |> Path_state.inst |> Instruction.mnemonic)
        ;
        Stats.add_instruction vaddr;
        last_id := id;
        do_directives vaddr e
      end
      (* When the last virtual address has not changed and we are on the same
       * path state, we are still in the same DBA block, hence no user action
       * can have been performed.  So, we just continue.
      *)
      else reloop e Continue

    and reloop e directive =
      if not @@ G.Directives.has (Env.global e) then halt e
      else
        let e_action =
          match directive with
          | Continue -> Eval.go
          | Discard  -> Env.pick_path in
        match e_action e with
        | e -> loop_aux e
        | exception G.Path.Empty_worklist -> halt e

    (* process directives at this address, and call reloop with Continue or Discard
     * depending on the result *)
    and do_directives vaddr e =
      let glob = Env.global e in
      match G.Directives.at vaddr e.Env.global with
      | None -> reloop e Continue
      | Some q ->
        let open Directive in
        (* q' is the new queue that will replace the old one.
           Uses side effects. *)
        let q' = Queue.create () in
        let rec handle_directives e path_directive =
          if Queue.is_empty q then begin
            if Queue.is_empty q' then G.Directives.remove vaddr glob
            else G.Directives.update vaddr q' glob;
            reloop e path_directive
          end
          else
            let g = Queue.take q in
            let p = Env.local e in
            match directive g with
            | Choice _ ->
              (* Branch choice is handled later on the DBA instruction itself *)
              Queue.add g q';
              handle_directives e path_directive
            | Cut ->
              Queue.add g q';
              Logger.result "@[<h>Directive :: cut %@ %a@]"
                Sse_types.vaddr_pp vaddr;
              handle_directives e Discard
            | Reach (count, cond) ->
              begin
                reach_count := !reach_count + 1;
                (* without merge, validation mode is only complete if only one path reaches the target *)
                if !reach_count >= 2 && RobustMode.get() = Validation && RobustMerge.get() <> FullMerge then
                  Env.mark_as_incomplete e;
                let p = match cond with
                  | Some(x) -> Sse_smt.Translate.assertion x p
                  | None -> p
                in
                let p = Path_state.strip p in
                let merge p =
                  let time = Unix.gettimeofday () in
                  let merged = G.reach_and_merge (Env.global e) p in
                  Stats.add_merged_path (Unix.gettimeofday () -. time);
                  merged
                in
                if (Robust.get()) && (RobustMode.get() = Validation) && not (RobustIncremental.get()) then
                  begin
                    Logger.info "Reach goal at %a:@ Storing for later statisfiability check..."
                      Sse_types.vaddr_pp vaddr;
                    let _ = merge p in
                    (* requeue the directive forever *)
                    Queue.add g q';
                    (* and continue exploration *)
                    handle_directives e Continue
                  end
                else
                  (* compute status of the best between 1/ this path 2/ the merged one *)
                  let status, simple_status, p =
                    let status, p = Sse_smt.Solver.check_satistifiability ~explore:false p in
                    let simple_status = Sse_smt.Robust_status.accept_reach status in
                    if (Robust.get()) && (RobustMode.get() = Validation) && (RobustMerge.get() = FullMerge) then
                      (* always store *)
                      let merged = merge p in
                      if p == merged then
                        status, simple_status, p
                      else
                        match simple_status with
                        | Some true -> status, simple_status, p
                        | _ -> begin
                            Logger.info "Reach goal at %a:@ Non merged path is %a, trying again with merged path..."
                              Sse_types.vaddr_pp vaddr
                              Sse_smt.Robust_status.pp status;
                            let status, p = Sse_smt.Solver.check_satistifiability ~explore:false merged in
                            let simple_status = Sse_smt.Robust_status.accept_reach status in
                            status, simple_status, p
                          end
                    else
                      status, simple_status, p
                  in
                  match simple_status with
                  | Some true ->
                    begin
                      let c' = Count.decr count in
                      Logger.result
                        "@[<h>Directive :: reached address %a with %a (%a to go)@]"
                        Sse_types.vaddr_pp vaddr
                        (Print_utils.pp_opt Dba_printer.EICAscii.pp_expr) cond
                        Count.pp c';
                      Sse_types.Path_state.dump_path p;
                      (match Sse_smt.Solver.get_model ~universal:(Robust.get()) p with
                       | Some m ->
                         Logger.result "@[<v 0>Model %@ %a@ %a@]"
                           Sse_types.vaddr_pp vaddr
                           Smt_model.pp m;
                       | None ->
                         Logger.result
                           "@[<h>No model %@ %a@]" Sse_types.vaddr_pp vaddr);
                      (match c' with
                       (* Do not add it to q', this is done*)
                       | Count.Count 0 -> ()
                       | Count.Count n ->
                         let loc = Loader_utils.Binary_loc.address vaddr in
                         Queue.add (Directive.reach ~n loc) q'
                       | Count.Unlimited ->
                         Queue.add g q') ;
                      handle_directives e path_directive
                    end
                  | _ ->
                    (* here we don't reuse the path state because we added an assert
                     * statement for `reach if` directives *)
                    begin
                      Logger.warning
                        "@[<h>Directive :: reach \
                         reached address %a if %a with %a \
                         (still %a to go)@]"
                        Sse_types.vaddr_pp vaddr
                        (Print_utils.pp_opt Dba_printer.EICAscii.pp_expr) cond
                        Sse_smt.Robust_status.pp status
                        Count.pp count ;
                      if simple_status = None then Env.mark_as_incomplete e;
                      Queue.add g q';
                      (* It's not SAT - not counted as a reach *)
                      let path_directive = match cond with
                        (* if there was a condition, don't stop exploring *)
                        | Some(_) -> path_directive
                        (* if we only wanted to reach the address, and it's not reachable, stop here *)
                        | None -> Discard
                      in
                      handle_directives e path_directive
                    end
              end
            | Enumerate (k, ex) ->
              let e_fml, p = Sse_smt.Translate.expr' p ex in
              let enumerate_at_most k =
                let bv_vs, _p =
                  Sse_smt.Solver.enumerate_values k e_fml p in
                G.Directives.Enumeration.record vaddr ex bv_vs glob;
                let n = G.Directives.Enumeration.count vaddr ex glob in
                let vs = G.Directives.Enumeration.get vaddr ex glob in
                Logger.result
                  "@[<hov 0>Directive :: enumerate@ \
                   possible values (%d) for %a %@ %a:@ @[<hov 0>%a@]@]"
                  n
                  Dba_printer.EICAscii.pp_bl_term ex
                  Sse_types.vaddr_pp vaddr
                  (Print_utils.pp_list ~sep:",@ " Bitvector.pp) vs;
              in
              (
                match k with
                | Count.Count k ->
                  begin
                    enumerate_at_most k;
                    let got = (G.Directives.Enumeration.count vaddr ex glob) in
                    if got < k then
                      let loc = Loader_utils.Binary_loc.address vaddr in
                      Queue.add (Directive.enumerate ~n:k ex loc) q'
                  end
                | Count.Unlimited ->
                  begin
                    Queue.add g q';
                    enumerate_at_most max_int
                  end
              )
              ;
              handle_directives e path_directive
            | Assume expr ->

              let p =
                (* add comment *)
                let comment =
                  Format.asprintf
                    "@[<h>user constraint : assume %a @]"
                    Dba_printer.EICAscii.pp_bl_term expr in
                Logger.debug "Assume %@ %a" Sse_types.vaddr_pp vaddr;
                let symb_state =
                  Path_state.symbolic_state p
                  |> Sse_symbolic.State.comment comment in
                Path_state.set_symbolic_state symb_state p in
              (* Adding the formula itself *)
              let local = Sse_smt.Translate.assume expr p in
              let e = Env.create ~global:glob ~local in
              Queue.add g q';
              handle_directives e path_directive
        in handle_directives e Continue

    in
    loop_aux e



  let interval_or_set_to_cond expr is =
    let open Parse_helpers.Initialization in
    match is with
    | Singleton _ -> assert false
    | Signed_interval (e1, e2) ->
      Dba.Expr.logand (Dba.Expr.sle e1 expr) (Dba.Expr.sle expr e2)
    | Unsigned_interval (e1, e2) ->
      Dba.Expr.logand (Dba.Expr.ule e1 expr) (Dba.Expr.ule expr e2)
    | Set l ->
      match l with
      | [] -> assert false
      | a :: b ->
        let f = Dba.Expr.equal expr in
        List.fold_left (fun acc e -> Dba.Expr.logor acc @@ f e) (f a) b


  let initialize_state ~filename e =
    let e =
      let cli_counters = Visit_address_counter.get () in
      match cli_counters with
      | [] -> e
      | cs ->
        Logger.info "Found some address counters ... great";
        let m =
          let open! Virtual_address in
          List.fold_left
            (fun m c -> Map.add c.Address_counter.address c m) Map.empty cs in
        Env.{e with local = Path_state.set_address_counters m e.local}
    in
    if not (Sys.file_exists filename) then begin
      Logger.warning "Cannot find sse configuration file %s" filename;
      e
    end
    else
      let initials =
        Logger.debug "Reading initialization from %s" filename;
        let parser = Parser.initialization
        and lexer = Lexer.token in
        Parse_utils.read_file ~parser ~lexer ~filename
      in
      let f e init =
        let open Parse_helpers.Initialization in
        match init.operation with
        | Mem_load (addr, size) ->
          Env.{e with local = Path_state.assign_from_image e.local ~addr ~size}
        | Declaration lval ->
          begin
            match Dba_types.LValue.name_of lval with
            | Some name ->
              let symb = Path_state.symbolic_state e.Env.local in
              let size = Dba.LValue.size_of lval in
              let sort = Formula.BvSort size in
              if (not @@ Sse_options.IgnoreControlled.get()) && (not @@ Sse_options.Robust.get()) then
                Logger.fatal "Variable %s is controlled but robust sse is not enabled. Please specify option -sse-robust" name;
              let symb =
                Sse_symbolic.State.declare ~controlled:init.controlled name sort symb in
              Env.{e with local = Path_state.set_symbolic_state symb e.local}
            | None ->
              Logger.fatal "Cannot declare unnamed %a" Dba_printer.Unicode.pp_lhs lval
          end
        | Instruction instr ->
          let caddr = Path_state.location e.Env.local in
          (* the parser gives instructions with -1 (invalid) successor *)
          let instr_no_jump = Dba_types.Instruction.set_successor instr caddr.Dba.id in
          Eval.interpret_instruction instr_no_jump e
        | Assignment (lval, rval, naming_hint) ->
          let controlled = init.controlled in
          match rval with
          | Singleton rv ->
            Env.{e with local = Sse_smt.Translate.assignment lval rv e.local}
          | x ->
            let state =
              Sse_smt.Translate.nondet ~controlled ?naming_hint lval e.Env.local in
            let eval = Dba.LValue.to_expr lval in
            let cond = interval_or_set_to_cond eval x in
            Env.{e with local = Sse_smt.Translate.assume cond state}
      in List.fold_left f e initials
  ;;


  let do_sse ~filename =
    let level = 3 in
    Logger.debug ~level "Running SSE on %s" filename;
    let entrypoint = get_entry_point () in
    Logger.debug ~level "Starting from %a" Sse_types.vaddr_pp entrypoint;
    let initialize_fun =
      initialize_state ~filename:(Sse_options.MemoryFile.get ()) in
    Logger.debug ~level "Initialization done ...";
    Logger.debug ~level "Driver set ...";
    let g = G.from_address ~entrypoint in
    let env = Env.of_global g|> initialize_fun in
    if Robust.get() && (Formula_options.InvertExistentialsIncomplete.get()) then
      G.mark_as_incomplete env.Env.global;
    if Robust.get() && (RobustMode.get() = Exploration) && (RobustMerge.get() <> FullMerge) then
      G.mark_as_incomplete env.Env.global;
    WarnOnLostControl.set (Robust.get() && (WarnOnLostControl.get() || (Formula_options.(UniversalMode.get() = Taint))));
    if Robust.get() && (RobustMode.get() = Exploration) && (RobustMerge.get() = FullMerge) then
      Logger.fatal "-sse-robust-merge yes is not yet implemented";
    if Robust.get() && Formula_options.(UniversalMode.get() = Quantifier) && Formula_options.(Solver.get() = Boolector) then
      Logger.warning "Boolector does not support quantified AFBV formulas so this will likely fail";
    let print_stats_at_end () = print_stats false in
    at_exit print_stats_at_end;
    loop_until ~halt env


  let start () =
    let filename = Kernel_options.ExecFile.get () in
    do_sse ~filename

end

let run () =
  if Sse_options.is_enabled () && Kernel_options.ExecFile.is_set () then begin
    let (module H) =
      match Search_heuristics.get () with
      | Dfs -> (module Dfs_global:GLOBAL_ENV)
      | Bfs -> (module Bfs_global:GLOBAL_ENV)
      | Nurs ->
        let seed =
          match Seed.get_opt () with
          | Some s -> s
          | None ->
            let v = Utils.random_max_int () in
            Logger.info "Random search seed is %d" v;
            Seed.set v;
            v
        in
        Random.init seed;
        (module Nurs_global:GLOBAL_ENV)
    in let module S = Env_make(H) in S.start ()
  end


let _ =
  Cli.Boot.enlist ~name:"SSE" ~f:run
