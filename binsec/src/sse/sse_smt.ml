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

open Sse_options

module Robust_status = struct
  open Formula
  type t = {
    universal: status;
    existential: status;
  }

  let translate ~universal t =
    match t.universal, t.existential, universal with
    | _, UNSAT, _ -> Some false
    | SAT, _, _ -> Some true
    | _, SAT, false -> Some true
    | UNSAT, _, true when Formula_options.(UniversalMode.get() = Quantifier) -> Some false
    | UNSAT, _, true -> None
    | UNKNOWN, _, _ -> None
    | TIMEOUT, _, _ -> None
    | _, UNKNOWN, false -> None
    | _, TIMEOUT, false -> None

  (** returns whether this branch should be explored further. None if the solver returned UNKNOWN or similar *)
  let continue_exploration t = translate ~universal:(Robust.get() && RobustMode.get() = Exploration) t

  (** returns whether the goal was successfully reached *)
  let accept_reach t = translate ~universal:(Robust.get()) t

  let pp ppf t = Format.fprintf ppf "(∀=%a, ∃=%a)" Formula_pp.pp_status t.universal Formula_pp.pp_status t.existential

  let const s = { universal = s; existential = s; }
end


module S = Bitvector.Collection.Set

module Query_stats = struct
  type t = {
    unsat : int;
    sat   : int;
    err   : int;
    enums : int;
    time  : float;
    }

  let empty = { unsat = 0; sat = 0; err = 0; time = 0.; enums = 0; }
  let total q = q.unsat + q.sat + q.err + q.enums

  let avg_time q =
    q.time /. (float @@ total q)

  let add_enumeration q =
    { q with enums = q.enums + 1 }

  let add q time res =
    let q = { q with time = q.time +. time } in
    match res with
    | Some Formula.UNSAT -> { q with unsat = q.unsat + 1 }
    | Some Formula.SAT -> { q with sat = q.sat + 1 }
    | Some Formula.TIMEOUT
    | Some Formula.UNKNOWN -> { q with err = q.err + 1 }
    | None -> q

  let pp ppf q =
    let open Format in
    fprintf ppf
    "@[<v 0>\
     @[<h>sat            %d@]@,\
     @[<h>unsat          %d@]@,\
     @[<h>unknown        %d@]@,\
     @[<h>total          %d@]@,\
     @[<h>enumerations   %d@]@,\
     @[<h>time           %.2f@]@,\
     @[<h>average        %.2f@]@,\
     @]"
    q.sat q.unsat q.err (total q)
    q.enums
    q.time (avg_time q)

  let pp_json ppf q =
    let open Format in
    fprintf ppf
   {|@[<v 0>{
     @[<h>"sat":            %d@],@,
     @[<h>"unsat":          %d@],@,
     @[<h>"unknown":        %d@],@,
     @[<h>"total":          %d@],@,
     @[<h>"enumerations":   %d@],@,
     @[<h>"time":           %.2f@]@,
     }@]|}
    q.sat q.unsat q.err (total q)
    q.enums
    q.time

  module Q = struct
    let value = ref empty

    let _reset () = value := empty
    let add time res = value := add !value time res
    let add_enumeration () = value := add_enumeration !value
    let pp ppf () = pp ppf !value
    let pp_json ppf () = pp_json ppf !value
  end

  include Q
end

module Solver = struct
  open Sse_types

  let do_optimization
        ?(keep=Formula.VarSet.empty)
        ?controlled ?parameters fm =
    let level = 3 in
    if Formula.VarSet.is_empty keep then
      Logger.debug ~level "Optimize"
    else
      Logger.debug ~level
        "@[<v 2>Optimize but keep intact these variables:@ %a@]"
        Formula_pp.pp_varset keep;
    (match controlled with
     | Some controlled when not @@ Formula.VarSet.is_empty controlled ->
        Logger.debug ~level:3
          "@[<hov 2>Existential variables: %a@]" Formula_pp.pp_varset controlled
     | None | Some _ -> Logger.debug ~level:3 "No controlled variables"
    );
    match controlled with
      | None -> Formula_to_smtlib.formula (Formula_transformation.optimize_from_options ~keep fm)
      | Some set ->
        let is_controlled =
          if Formula.VarSet.is_empty set then fun _ -> false
          else fun var -> Formula.VarSet.mem var set
        in
        let is_parameter = match parameters with
          | None -> fun _ -> false
          | Some(x) when x = Formula.VarSet.empty -> fun _ -> false
          | Some(set) -> fun var -> Formula.VarSet.mem var set
        in
        Formula_transformation.to_universal_from_options ~keep is_controlled is_parameter fm
  ;;

  module Context = struct
    type t = {
        session: Solver.Session.t;
        keep : Formula.VarSet.t option;
        (** universally quantify the formula *)
        universal : bool;
        start_time : float;
      }

    let create ?keep ~universal () =
      let start_time = Unix.gettimeofday() in
      let timeout = Formula_options.Solver.Timeout.get () in
      let file =
        if Sse_options.SMT_dir.is_set() then
          let filename = Sse_utils.temp_file () in
        Some filename
      else None in
      let solver = Formula_options.Solver.get () in
      let interactive = not (Prover.is_boolector solver && universal) in
      let session = Solver.Session.create ~interactive ?file ~timeout solver in
      { session; keep; universal; start_time; } ;;

    let session c = c.session
    let destroy ~result c =
      let time = (Unix.gettimeofday()) -. c.start_time in
      Query_stats.add time result;
      Solver.Session.destroy c.session

    let debug_if_unknown ctx status =
      let file = Solver.Session.file ctx.session in
      match file, status with 
      | Some(f), Formula.UNKNOWN | Some(f), Formula.TIMEOUT ->
        Logger.warning "Formula %s yields %a" f Formula_pp.pp_status status
      | _ -> ()
  end

  (** write the formula corresponding to the path state to the solver *)
  let do_state context ps =
    let open Context in
    let keep = context.keep in
    let symb = Path_state.symbolic_state ps in
    let controlled =
      if context.universal
      then
        let controlled = Sse_symbolic.State.controlled_vars symb in
        if Formula.VarSet.is_empty controlled then
          Logger.fatal "Robust SE is enabled but no variable is controlled."
        else Some controlled
      else None
    in
    let parameters = Sse_symbolic.State.parameter_vars symb in
    let script = do_optimization ?keep ?controlled ~parameters (Path_state.formula ps) in
    List.iter (fun command -> assert ((Solver.Session.run context.session command) = Solver.Session.Nil)) script.Smtlib.script_commands
  ;;

  type ex =
    | E_failure
    | E_exn of (exn * string) (** exception backtrace *)

  let run_session ~ctx ~on_ctx ~result ps =
    let session = Context.session ctx in
    try
      do_state ctx ps;
      let v = on_ctx ctx in
      Context.destroy ~result:(result v) ctx;
      Ok v
    with
    | Failure msg ->
      Logger.warning "SMT solver failed on %s" msg;
      let file = Solver.Session.file session in
      Context.destroy ~result:(Some(Formula.UNKNOWN)) ctx;
      if not (Sse_options.KeepGoing.get ())
      then begin
        Logger.error
          "@[<v 0>\
           @[SMT solver failed in %a@ with message:@].@ \
           @[%s@]@ \
           @[Aborting. Use -keep-going to ignore@]@]"
          (Print_utils.pp_opt Format.pp_print_string) file  msg;
        failwith msg
      end;
      Error (E_failure)
    | Assert_failure _ as e -> raise e
    | e ->
      let file = Solver.Session.file session in
      let bt = Printexc.get_backtrace() in
      Context.destroy ~result:(Some(Formula.UNKNOWN)) ctx;
      Logger.warning "Destroyed session (current SMT file %a)"
        (Print_utils.pp_opt Format.pp_print_string) file ;
      Error (E_exn (e, bt))
  ;;

  let with_solver ?keep ~universal path_state on_ctx =
    let ctx = Context.create ?keep ~universal () in
    let result (st, _) = Some st in
    run_session ~ctx path_state ~on_ctx ~result
  ;;

  let no_address = S.empty

  let get_addresses_to_load session path_state =
    if not (LoadSections.is_set () || LoadROSections.get())
    then no_address
    else
      let model = Solver.Session.get_model session in
      let addresses = Smt_model.memory_addresses model in
      let loader = Kernel_functions.get_img () in
      let keep_addr (addr:Bitvector.t) =
        (* load addr iff
          *  (we don't have loaded it yet)
          *  && (it is in a (read-only || specified in cmdline) section)
        *)
        not (Path_state.address_belongs_to_init ~addr path_state) &&
          let address = Bitvector.value_of addr |> Bigint.int_of_big_int in
          match Loader_utils.find_section_by_address ~address loader with
          | None -> false
          | Some section ->
             let name = Loader.Section.name section in
             Basic_types.String.Set.mem name (LoadSections.get()) ||
               LoadROSections.get() &&
                 Loader.Section.(has_flag Loader_types.Read section &&
                                   not (has_flag Loader_types.Write section))
      in
      List.fold_left
        (fun set bv -> if keep_addr bv then S.add bv set else set)
        S.empty addresses

  let maybe_load_and_recurse ~k result path_state to_load =
    if S.is_empty to_load then result, path_state
    else
      let path_state =
        S.fold
          (fun addr ps -> Path_state.with_init_mem_at ps ~addr ~size:1)
          to_load path_state in
      Logger.debug ~level:1 "at %a:@ loading addresses @ %a"
        Path_state.pp_loc path_state
        (fun ppf ->
          S.iter (fun x -> Format.fprintf ppf "%a@ " Bitvector.pp_hex x))
        to_load;
      k path_state
  ;;

  let ctx_check_sat toload path_state ctx =
      let session = Context.session ctx in
      let result = Solver.Session.check_sat session in
      Context.debug_if_unknown ctx result;
      if result = Formula.SAT then
        toload := get_addresses_to_load session path_state;
      (result, ())
  ;;

  let check_existential ps =
    let st, m_opt =
      let ctx = Context.create ~universal:false () in
      let on_ctx ctx =
        match ctx_check_sat (ref no_address) ps ctx with
        | Formula.SAT, () ->
           Formula.SAT,
           Some (Solver.Session.get_model (Context.session ctx))
        | st, () -> st, None  in
      let result (st, _model) = Some st in
      match run_session ~ctx ps ~on_ctx ~result with
      | Ok (st, model) -> st, model
      | Error _ -> Formula.UNKNOWN, None in
    if st = Formula.SAT then begin
      Logger.warning
        "@[<v>Universal query at %a is UNSAT, existential query is SAT with model@ %a@]"
          Path_state.pp_loc ps
         (Print_utils.pp_opt Smt_model.pp) m_opt;
    end;
    Robust_status.{universal = Formula.UNSAT; existential = st}
  ;;

  let rec check_satistifiability ~explore path_state =
    let universal = Robust.get() && (not (explore && (RobustMode.get() = Validation))) && ((Path_state.merge_point path_state) = None) in
    if Path_state.symbolic_state path_state |> Sse_symbolic.State.unsat then
      begin
        Logger.debug ~level:1 "Path at %a is UNSAT by simplifications" Path_state.pp_loc path_state;
        (Robust_status.const Formula.UNSAT, path_state)
      end
    else
      begin
        let result, load =
          let toload = ref no_address in
          with_solver ~universal path_state (ctx_check_sat toload path_state)
          |> function
          | Ok (r, ()) ->
            r, !toload
          | Error e ->
            (match e with
             | E_failure -> ()
             | E_exn (e, bt) -> Logger.error "Solver could not be run successfully:@ %s@ %s" (Printexc.to_string e) bt
            );
            Formula.UNKNOWN, !toload
        in
        let rresult = if universal then
            Robust_status.{ universal = result; existential= Formula.UNKNOWN; }
          else
            Robust_status.{ existential= result; universal= Formula.UNKNOWN; }
        in
        match result with
        | Formula.SAT ->
          Logger.debug ~level:4 "SMT query at %a resulted in %a"
            Sse_types.vaddr_pp (Path_state.virtual_address path_state)
            Formula_pp.pp_status result;
          maybe_load_and_recurse ~k:(check_satistifiability ~explore) rresult path_state load
        | _ ->
          let log =
            if result = Formula.UNSAT then Logger.debug ~level:4
            else Logger.warning ~level:0 in
          log "SMT query at %a resulted in %a"
            Sse_types.vaddr_pp (Path_state.virtual_address path_state)
            Formula_pp.pp_status result;
          let rresult = if universal && (WarnOnLostControl.get()) then begin
              Logger.debug ~level:1
                "Failed attempt at robust query yielded %a: \
                 checking existential query for discrepancy"
                Formula_pp.pp_status result;
              if result = Formula.UNSAT then check_existential path_state else rresult
            end else rresult
          in
          rresult, path_state
      end
  ;;


  let get_model ~universal path_state =
    let on_ctx ctx =
      let session = Context.session ctx in
      let st = Solver.Session.check_sat session in
      st, match st with
      | Formula.SAT -> Solver.Session.get_model session
      | status ->
        begin
         Context.debug_if_unknown ctx status;
         Logger.fatal "Asking for a model on a %a formula"
           Formula_pp.pp_status status;
       end
    in
    match with_solver ~universal path_state on_ctx with
    | Error _ -> None
    | Ok (_, r) -> Some r
    | exception e ->
      let bt = Printexc.get_backtrace () in
      let s = Printexc.to_string e in
      (Logger.error "could not get model: %s %s" s bt; None)
  ;;

  let rec enumerate_values n expr path_state =
    Query_stats.add_enumeration ();
    let rec loop acc to_load n ctx =
      match n with
      | 0 -> acc, to_load
      | _ ->
        begin
          let session = Context.session ctx in
          begin match acc with
            | [] -> ()
            | x :: _ ->
              Formula.(mk_bv_distinct (mk_bv_cst x) expr)
              |> Formula.mk_assert
              |> Solver.Session.put_entry session
          end;
          match Solver.Session.check_sat session with
          | Formula.SAT ->
            let bv =
              Solver.Session.get_value session (Formula.mk_bv_term expr) in
            Logger.debug ~level:5 "Solver returned %a ; \
                                   %d solutions still to be found"
              Bitvector.pp_hex bv
              (n-1);
            let to_load' = get_addresses_to_load session path_state in
            loop (bv :: acc)
              (S.union to_load to_load') (n - 1) ctx
          | res ->
            begin
              Context.debug_if_unknown ctx res;
              Logger.debug ~level:4 "Solver returned %a"
                Formula_pp.pp_status res;
              acc, to_load
            end
        end
    in
    (* We need to avoid removal of the variables that are used in the
       enumeration. Since the enumerated term does not change --- only the
       distinct constant values do --- it is enough to compute the set of
       keep variables before any solver call.
     *)
    let keep = Formula_utils.bv_term_variables expr in
    (* universal = false because: we cannot stop on unsat with taint, and
     * anyway, we check later that this choice is really robust because we
     * taint (assert (= expr value)) *)
    let ctx = Context.create ~keep ~universal:false () in
    let result _ = None in
    let values, to_load =
      run_session ~ctx path_state ~on_ctx:(loop [] no_address n) ~result
      |> function
        | Error _ -> [], no_address
        | Ok (v) -> v
    in
    let values, path_state =
      let k = enumerate_values n expr in
      maybe_load_and_recurse ~k values path_state to_load
    in
    if List.length values = n then
      Logger.warning "[0x%a] Found as many solutions for@ %a@ as asked: %d.@ \
                      Possibly incomplete solution set."
        Sse_types.vaddr_pp (Path_state.virtual_address path_state)
        Formula_pp.pp_bv_term expr
        n;
    values, path_state
end

module Translate = struct
  open Dba
  open Sse_types


  let unary e = function
    | Unary_op.Not    -> Formula.mk_bv_not
    | Unary_op.UMinus -> Formula.mk_bv_neg
    | Unary_op.Sext n -> Formula.mk_bv_sign_extend (n - Dba.Expr.size_of e)
    | Unary_op.Uext n -> Formula.mk_bv_zero_extend (n - Dba.Expr.size_of e)
    | Unary_op.Restrict interval -> Formula.mk_bv_extract interval


  let as_bv bop e1 e2 =
    Formula.(mk_bv_ite (bop e1 e2) (mk_bv_one) (mk_bv_zero))

  let rotate_right_const n = Formula.mk_bv_rotate_right n
  let rotate_left_const n = Formula.mk_bv_rotate_left n

  let rotate shift_func rev_shift_func const_rot_func value shift =
    let open Formula in
    match shift.bv_term_desc with
    | BvCst x ->
      let op = Bitvector.value_of x |> Bigint.int_of_big_int |> const_rot_func in
      op value
    | _ ->
      let part1 = shift_func value shift
      and shift_size = Formula_utils.bv_size shift
      and value_size = Formula_utils.bv_size value |> Bigint.big_int_of_int in
      let value_size = Bitvector.create value_size shift_size |> mk_bv_cst in
      let offset = mk_bv_sub value_size shift in
      let part2 = rev_shift_func value offset in
      mk_bv_or part1 part2

  let rotate_right = rotate Formula.mk_bv_lshr Formula.mk_bv_shl rotate_right_const
  let rotate_left = rotate Formula.mk_bv_shl Formula.mk_bv_lshr rotate_left_const


  let binary op =
    let open Binary_op in
    match op with
    | Plus   -> Formula.mk_bv_add
    | Minus  -> Formula.mk_bv_sub
    | Mult   -> Formula.mk_bv_mul
    | DivU   -> Formula.mk_bv_udiv
    | DivS   -> Formula.mk_bv_sdiv
    | ModU   -> Formula.mk_bv_urem
    | ModS   -> Formula.mk_bv_srem
    | Eq     -> as_bv (Formula.mk_bv_equal)
    | Diff   -> as_bv (Formula.mk_bv_distinct)
    | LeqU   -> as_bv (Formula.mk_bv_ule)
    | LtU    -> as_bv (Formula.mk_bv_ult)
    | GeqU   -> as_bv (Formula.mk_bv_uge)
    | GtU    -> as_bv (Formula.mk_bv_ugt)
    | LeqS   -> as_bv (Formula.mk_bv_sle)
    | LtS    -> as_bv (Formula.mk_bv_slt)
    | GeqS   -> as_bv (Formula.mk_bv_sge)
    | GtS    -> as_bv (Formula.mk_bv_sgt)
    | Xor    -> Formula.mk_bv_xor
    | And    -> Formula.mk_bv_and
    | Or     -> Formula.mk_bv_or
    | Concat -> Formula.mk_bv_concat
    | LShift -> Formula.mk_bv_shl
    | RShiftU -> Formula.mk_bv_lshr
    | RShiftS -> Formula.mk_bv_ashr
    | LeftRotate -> rotate_left
    | RightRotate -> rotate_right


  let rec expr symbolic_state e =
    let smt_unary = unary and smt_binary = binary in
    let open Dba.Expr in
    match e with
    | Var {name; size; _} ->
      Sse_symbolic.State.get_bv name (Size.Bit.create size) symbolic_state
    | Cst (_, bv) -> (Formula.mk_bv_cst bv, symbolic_state)
    | Load (bytes, _endianness, e) ->
      let smt_e, st = expr symbolic_state e in
      let mem = Sse_symbolic.State.get_memory st in
      (Formula.mk_select bytes mem smt_e, st)
    | Binary (bop, lop, rop) as e ->
      Logger.debug ~level:6 "Translating binary %a" Dba_printer.Ascii.pp_bl_term e;
      let l_smt_e, st = expr symbolic_state lop in
      let r_smt_e, st' = expr st rop in
      (smt_binary bop l_smt_e r_smt_e, st')
    | Unary (uop, e) ->
      let v, st = expr symbolic_state e in
      (smt_unary e uop v, st)
    | Ite (c, then_e, else_e) ->
      let cond, st = expr symbolic_state c in
      let then_smt, st' = expr st then_e in
      let else_smt, st'' = expr st' else_e in
      let v = Formula.(mk_bv_ite
                 (mk_bv_equal cond (mk_bv_one)) then_smt else_smt)
      in (v, st'')

  let expr' st e =
    let e, sst = expr (Path_state.symbolic_state st) e in
    (e, Path_state.set_symbolic_state sst st)

  open Sse_symbolic

  let lvalue_with_rval_update symbolic_state logical_rval = function
    | LValue.Var { name; size = bitsize;  _} ->
      name, Formula.bv_sort bitsize, Formula.mk_bv_term logical_rval, symbolic_state
    | LValue.Restrict ({name; size = bitsize; _}, {Interval.lo; Interval.hi}) ->
      let size = Size.Bit.create bitsize in
      let t = Formula.bv_sort bitsize in
      let svar, st = State.get_bv name size symbolic_state in
      let concat_lo = lo - 1 and concat_hi = hi + 1 in
      let max_bit = bitsize - 1 in
      let rval =
        let open Formula in
        match concat_lo < 0, concat_hi > max_bit with
        | false, false ->
          mk_bv_concat
            (mk_bv_extract {Interval.lo=concat_hi; Interval.hi=max_bit} svar)
            (mk_bv_concat logical_rval
               (mk_bv_extract {Interval.lo=0; Interval.hi=concat_lo} svar))
        | true, false ->
          mk_bv_concat
            (mk_bv_extract {Interval.lo=concat_hi; Interval.hi=max_bit} svar)
            logical_rval
        | false, true ->
          mk_bv_concat
            logical_rval
            (mk_bv_extract {Interval.lo=0; Interval.hi=concat_lo} svar)
        | true, true -> logical_rval
      in name, t, Formula.mk_bv_term rval, st
    | LValue.Store (sz, _, e) ->
      let mem = State.get_memory symbolic_state in
      let value = logical_rval in
      let index, st = expr symbolic_state e in
      let n, s, v = State.memory_term (Formula.mk_store sz mem index value) in
      n, s, v, st


  let assign lval rval symstate =
    let logical_rval_base, st = expr symstate rval in
    let name, var_type, logical_rval, st' =
      lvalue_with_rval_update st logical_rval_base lval in
    Sse_symbolic.State.assign name var_type logical_rval st'

  let assignment lvalue rvalue path_state =
    let symstate = Path_state.symbolic_state path_state in
    let symbolic_state = assign lvalue rvalue symstate in
    Path_state.set_symbolic_state symbolic_state path_state

  let gen_unknown =
    let count = ref 0 in
    (fun ?naming_hint () ->
      match naming_hint with
      | None -> incr count; "bs_unknown" ^ string_of_int !count
      | Some name -> name
    )

  let nondet ?naming_hint ~controlled lvalue path_state =
    let symstate = Path_state.symbolic_state path_state in
    let size = LValue.size_of lvalue in
    let name = match naming_hint with
      | Some name -> name
      | None -> 
        let lvalue_name = Format.asprintf "%a" Dba_printer.Ascii.pp_lhs lvalue in
        let sanitised = String.map (fun c ->
            if ('a'<=c && c<='z') || ('A' <=c && c <='Z') || ('0'<=c && c<='9')
            then c
            else '_'
          ) lvalue_name in
        (gen_unknown ()) ^ "_for_" ^ sanitised
    in
    Logger.debug ~level:3
      "%s is non-determinism source for %a" name Dba_printer.EICAscii.pp_lhs lvalue;
    let symstate =
      Sse_symbolic.State.declare ~controlled name (Formula.BvSort size) symstate in
    let logical_rval_base, st =
      Sse_symbolic.State.get_bv name (Size.Bit.create size) symstate in
    let name, var_type, logical_rval, st' =
      lvalue_with_rval_update st logical_rval_base lvalue in
    let st' =
      Sse_symbolic.State.assign name var_type logical_rval st' in
    Path_state.set_symbolic_state st' path_state

  let mark_hypothesis ~assumption cond state =
    assert(Expr.size_of cond = 1);
    let c, st = expr (Path_state.symbolic_state state) cond in
    let tr_cond = Formula.(mk_bv_equal c mk_bv_one) in
    let st' = Sse_symbolic.State.constrain ~assumption tr_cond st in
         Path_state.set_symbolic_state st' state
  ;;

  let assume = mark_hypothesis ~assumption:true ;;

  let assertion = mark_hypothesis ~assumption:false ;;

end
