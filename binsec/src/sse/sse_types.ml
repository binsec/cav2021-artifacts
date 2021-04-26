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

open Format
open Sse_options


let vaddr_pp ppf vaddr =
  let x = Virtual_address.to_int vaddr in
  let img = Kernel_functions.get_img() in
  let symbols = Loader.Img.symbols img in
  Format.fprintf ppf "0x%08x" x;
  if Array.length symbols > 0 then
  let d y = if (x < y) then max_int else x - y in
  let (distance, symbol) = Array.fold_left (fun (distance, current_best) symbol ->
      let addr = Loader.Symbol.value symbol in
      let distance' = d addr in
      (* there are some strange symbols "$a", "$d" on elf armv7 *)
      if String.length (Loader.Symbol.name symbol) > 2 && distance' < distance then
        (distance', symbol)
      else
        (distance, current_best)
    ) (max_int, symbols.(0)) symbols in
  (* if we are too far from the target address, better print no indication than
   * a wrong one *)
  if distance < 0x1000 && distance < x/2 then
    Format.fprintf ppf "(%s+0x%x)" (Loader.Symbol.name symbol) distance

module Extra = struct
  type t = int
  let hash t = (t:>int)
  let equal = (=)
end


module C = struct
  include Instr_cfg.Make(Extra)
end

module Path = struct
  type t = Dba_types.Statement.t Sequence.t
  let empty = Sequence.empty
  let is_empty sq = sq = Sequence.empty
  let extend i p = Sequence.push_front i p

  let virtual_address stmt =
    let open Dba_types in
    Statement.location stmt |> Caddress.to_virtual_address

  let pp_as_address_trace ppf p =
    match Sequence.peek_front p with
    | None -> ()
    | Some stmt ->
      let last_vaddres = ref (virtual_address stmt) in
      pp_open_vbox ppf 0;
      Sequence.iter_forward
        (fun i ->
           let v = virtual_address i in
           if v <> !last_vaddres then
             fprintf ppf "@[<h>%a@]@ " vaddr_pp v;
           last_vaddres := v) p;
      pp_close_box ppf ()


  let pp_address_trace_to_file p =
    if Sse_options.AddressTraceFile.is_set () then
      let filename = Sse_options.AddressTraceFile.get () in
      Print_utils.pp_to_file ~filename pp_as_address_trace p
end


let decode vaddress =
  Logger.debug ~level:2 "Decoding %@ %a" vaddr_pp vaddress;
  Disasm_core.decode vaddress |> fst

module Path_state = struct

  type t  = {
    id : int ;   (* Unique identifier for the path *)
    depth : int; (* Current depth of traversal *)
    solver_calls : int;
    path : Path.t; (* Sequence of DBA statements for this path *)
    symbolic_state : Sse_symbolic.State.t; (* Current symbolic state *)
    instruction : Instruction.t; (* Current instruction *)
    block_index : int;           (* Current index into DBA block of current
                                    instruction *)

    (* How many times did we pass at this address ?*)
    address_counters : Sse_options.Address_counter.t Virtual_address.Map.t;
    cfg : C.t; (* The CFG that we are --- possibly --- building *)
    entrypoint: Virtual_address.t ; (* The entry point *)
    merge_point: Dba.address option; (** None if this path state should not be merged,
                                         and where it should be merged otherwise *)
  }

  let merge_point ps = ps.merge_point

  let link v ps =
    let vaddr_pred = Instruction.address ps.instruction in
    let vaddr_succ = C.V.addr v in
    C.add_edge_a ps.cfg vaddr_pred vaddr_succ

  let add_instruction ~inst ps =
    let vaddr = Instruction.address inst in
    let v = C.V.of_inst vaddr inst in
    C.add_vertex ps.cfg v;
    v

  let insert ~count ~addr ps =
    let inst = decode addr in
    let v = add_instruction ~inst ps in
    C.add_symb ps.cfg addr count;
    link v ps;
    inst

  let decode addr ps =
    let cfg = ps.cfg in
    match C.mem_vertex_a cfg addr with
    | None -> insert ~count:1 ~addr ps
    | Some v ->
       match C.V.inst v, C.V.symb v with
       (* I suspect it never happens *)
       | None, None -> insert ~count:1 ~addr  ps
       | None, Some _ | Some _, None ->
          let msg =
            Format.asprintf "Error durring sse in CFG construction %@ %a"
              vaddr_pp addr in
          failwith msg
       | Some i, Some c ->
          (* Link to its predecessor the returned instruction *)
          (C.add_symb cfg addr (c + 1);  link v ps; i;)


  let gen_id = ref (-1)

  let id st = st.id
  let cfg ps = ps.cfg
  let entrypoint ps = ps.entrypoint
  let symbolic_state st = st.symbolic_state
  let block_index st = st.block_index
  let inst ps = ps.instruction
  let counter vaddr st =
    match Virtual_address.Map.find vaddr st.address_counters with
    | c -> Some c
    | exception Not_found -> None

  let set_counter vaddr c st =
    { st with address_counters =
                Virtual_address.Map.add vaddr c st.address_counters }

  let paths_created () = !gen_id

  let solver_calls p = p.solver_calls
  let incr_solver_calls p = { p with solver_calls = p.solver_calls + 1; }
  let reset_solver_calls p = { p with solver_calls = 0; }

  let dba_instruction st =
    let block = st.instruction.Instruction.dba_block in
    Dhunk.inst block st.block_index |> Utils.unsafe_get_opt

  let set_instruction instruction st =
    { st with instruction }

  let set_block_index block_index st = { st with block_index }

  let set_symbolic_state symbolic_state st = { st with symbolic_state }

  let set_address_counters address_counters st = { st with address_counters }

  let add_assumption cond st =
    let open Sse_symbolic.State in
    let symbolic_state = constrain ~assumption:true cond st.symbolic_state in
    { st with symbolic_state }

  let add_assertion cond st =
    let open Sse_symbolic.State in
    let symbolic_state = constrain ~assumption:false cond st.symbolic_state in
    { st with symbolic_state }

  let assign_from_image ~addr ~size path_state =
    let symbolic_state =
      let value = Bitvector.value_of addr in
      let bvsize = Kernel_options.Machine.word_size () in
      let addr = Bitvector.create value bvsize in
      Sse_symbolic.State.assign_from_image path_state.symbolic_state ~addr ~size in
    { path_state with symbolic_state }

  let with_init_mem_at ~addr ~size path_state =
    let symbolic_state =
      let value = Bitvector.value_of addr in
      let bvsize = Kernel_options.Machine.word_size () in
      let addr = Bitvector.create value bvsize in
      Sse_symbolic.State.init_mem_at path_state.symbolic_state ~addr ~size in
    { path_state with symbolic_state }

  exception Found

  let address_belongs_to_init ~addr path_state =
    let add_int bv n =
      let size = Bitvector.size_of bv in
      let bv_n = Bitvector.of_int ~size n in
      Bitvector.add bv bv_n in
    try
      let open Sse_symbolic.State in
      Bitvector.Collection.Map.iter
        (fun kaddr vsize ->
           if
             Bitvector.compare addr kaddr >= 0 &&
             let end_addr = add_int kaddr vsize in
             Bitvector.compare addr end_addr < 0
           then raise Found
        ) (initializations path_state.symbolic_state);
      false
    with Found -> true

  let virtual_address st =
    let open Instruction in
    st.instruction.address

  let location st =
    let caddress =
      virtual_address st |> Dba_types.Caddress.of_virtual_address in
    Dba_types.Caddress.reid caddress st.block_index

  let current_statement st =
    dba_instruction st
    |> Dba_types.Statement.create (location st)

  let strip p = { p with symbolic_state = Sse_symbolic.State.strip p.symbolic_state; }

  let merge p1 p2 =
    let (=>) p q = (not p) || q in begin
      assert (p1.instruction = p2.instruction);
      assert (p1.block_index = p2.block_index);
      assert ((RobustMode.get() = Exploration) => (p1.merge_point = Some(location p1)));
      assert ((RobustMode.get() = Validation) => (p1.merge_point = None));
      assert (p1.entrypoint = p2.entrypoint);
    end;
    {
      id = p1.id;
      depth = min p1.depth p2.depth; (* so that we explore everything reachable until max depth *)
      solver_calls = max p1.solver_calls p2.solver_calls;
      path = (* FIXME *) p1.path;
      symbolic_state = Sse_symbolic.State.merge p1.symbolic_state p2.symbolic_state;
      instruction = p1.instruction;
      block_index = p1.block_index;
      address_counters = (* FIXME *) p1.address_counters;
      cfg = (* FIXME *) p1.cfg;
      merge_point = p1.merge_point;
      entrypoint = p1.entrypoint;
    }

  let goto address st =
    let vaddr = Dba_types.Caddress.to_virtual_address address in
    let instruction =
      if Virtual_address.compare vaddr (virtual_address st) <> 0
      then decode vaddr st
      else st.instruction in
    let st = set_instruction instruction st |> set_block_index address.Dba.id in
    let statement = current_statement st in
    let path = Path.extend statement st.path in
    let depth = st.depth + 1 in
    { st with path; depth; }

  let goto_vaddr address st =
    goto (Dba_types.Caddress.of_virtual_address address) st

  let set_block_index idx st =
    goto (Dba_types.Caddress.reid (location st) idx) st

  let pp_loc ppf st =
    let dba_instruction = dba_instruction st in
    let vaddress = virtual_address st in
    fprintf ppf "@[<hov>(%a, %d)@ :@ @[%a@]@]"
      vaddr_pp vaddress
      st.block_index
      Dba_printer.Ascii.pp_instruction dba_instruction

  let pp_path fmt ps =
    let path = ps.path in
    if not @@ Path.is_empty path then Format.fprintf fmt "%a" Path.pp_as_address_trace path

  let dump_path ps =
    let path = ps.path in
    if not @@ Path.is_empty path then Path.pp_address_trace_to_file path

  let is_depth_ok ps =
    let max_depth = Sse_options.MaxDepth.get () in
    ps.depth < max_depth
  ;;

  let set_merge_point merge_point st =
    match st.merge_point with
    | Some(a) ->
      Logger.fatal "trying to erase merge_point %a with %a in path state at %a"
        Dba_types.Caddress.pp_base a
        (Print_utils.pp_opt Dba_types.Caddress.pp_base) merge_point
        pp_loc st
    | None -> { st with merge_point }


  let formula ps = Sse_symbolic.State.formula @@ symbolic_state ps ;;

  let create
      ?(depth=0)
      ?(address_counters=Virtual_address.Map.empty)
      ?cfg
      ?(block_index=0) symbolic_state instruction =
    assert(
      block_index >= 0 &&
      block_index <=
      Dhunk.length instruction.Instruction.dba_block);
    incr gen_id;
    let cfg =
      match cfg with
      | None -> C.create (MaxDepth.get ())
      | Some cfg -> cfg in
    let entrypoint = Instruction.address instruction in
    let ps =
    { id = !gen_id; address_counters;
      depth;  path = Path.empty;
      block_index; symbolic_state; instruction;
      solver_calls = 0; (* At path creation we have never called a solver *)
      cfg; entrypoint;
      merge_point = None;
    } in
    ignore @@ add_instruction ~inst:instruction ps;
    ps


  let dump_cfg ~filename ps =
    C.dump ~filename ps.cfg

  let branch p =
    incr gen_id;
    { p with id = !gen_id }

end

(* Both the stack and the queue below are functional implementations of these
   data structures
*)

module type WORKLIST = sig
  type t
  val push : Path_state.t -> t -> t
  val pop  : t -> Path_state.t * t
  val singleton : Path_state.t -> t
  val length : t -> int
  val is_empty : t -> bool
  val empty : t
end

module W_stack:WORKLIST = Fstack.Make(Path_state)

module W_queue:WORKLIST = struct
  type t = Path_state.t Sequence.t

  let length = Sequence.length
  let is_empty q = Sequence.length q = 0
  let empty = Sequence.empty
  let push p q = Sequence.push_back p q
  let pop q =
    match Sequence.peek_front q with
    | None -> raise Not_found
    | Some v ->
      match Sequence.pop_front q with
      | None -> assert false
      | Some seq -> v, seq

  let singleton p = push p empty
end

module Random_heap: WORKLIST = struct
  (* This is actually a fairly classical heap.
     The priority added to the date is just generated at random.
  *)

  module T = struct
    type t = {
      priority : int;
      state : Path_state.t
    }

    let compare t1 t2 = Pervasives.compare t1.priority t2.priority

    let create ~priority ~state = {priority; state;}
  end

  module H = Worklist.Make(T)

  type t = H.t

  let gen_priority () = Utils.random_max_int ()

  let length = H.length
  let is_empty = H.is_empty
  let empty = H.empty

  let push p h =
    let priority = gen_priority () in
    H.add (T.create ~priority ~state:p) h

  let pop h =
    let e, h' = H.pop h in
    e.T.state, h'

  let singleton p = push p empty

end

module G(W:WORKLIST) = struct

  type d = {
    store : Directive.t Queue.t Virtual_address.Htbl.t;
  }

  module Bv_set = struct
    (* one could use Set.Make(Bitvector) but lists are simpler
     * and might even be faster
    *)
    type t = Bitvector.t list

    let union l1 l2 = l1 @ l2 |> List.sort_uniq Bitvector.compare

    let cardinal = List.length
  end

  module Etbl =
    Hashtbl.Make(
        struct
          type t = Dba.Expr.t ;;
          let hash = Hashtbl.hash ;;
          let equal = Dba.Expr.is_equal ;;
        end
      );;

  type merge_info = {
    in_worklist_count: int; (** count of sibling path states in the work queue *)
    result: Path_state.t option; (** the partial merge of path_states which reached
                                     the merge point *)
  }

  type t = {
    worklist : W.t; (** contains path_state to visit *)
    directives : d; (** targets, cuts etc. *)
    enumerations : Bv_set.t Etbl.t Virtual_address.Htbl.t;
    complete : bool ref; (** whether the search was complete or approximated because of an UNKNOWN for example *)
    to_merge : merge_info Dba_types.Caddress.Htbl.t; (** in progress opportunistic path merging *)
    merges : Path_state.t Virtual_address.Htbl.t; (** systematic path merging: goal -> merge of all states reaching it *)
  }

  let mark_as_incomplete t = t.complete := false
  let complete t = !(t.complete)

  (** to call when a path state to be merged reaches its merge point.
   * returns the fully merged path state, or None if other paths state
   * must be waited for *)
  let merge_ready_path_state e ps =
    let addr = match ps.Path_state.merge_point with
      | None -> Logger.fatal "merge path state %a not to be merged" Path_state.pp_loc ps
      | Some(x) -> x
    in
    let current = Dba_types.Caddress.Htbl.find e.to_merge addr in
    let result = Some(match current.result with
      | None -> ps
      | Some(old) -> Path_state.merge old ps)
    in
    if current.in_worklist_count = 0 then
      begin
        Dba_types.Caddress.Htbl.remove e.to_merge addr;
        match result with
        | None -> Logger.fatal "yield unmerged path state"
        | Some(r) -> Some(Path_state.{ r with merge_point = None })
      end
    else
      begin
        let next = { current with result } in
        Dba_types.Caddress.Htbl.replace e.to_merge addr next;
        None
      end

  (** marks that a path_state reached a goal and returns the merge of all 
   * path_states having reaches this goal so far *)
  let reach_and_merge e path_state =
    let addr = Path_state.virtual_address path_state in
    let next = try
        let current = Virtual_address.Htbl.find e.merges addr in
        Path_state.merge current path_state
      with
        Not_found -> path_state
    in
    Virtual_address.Htbl.replace e.merges addr next;
    next

  let drain_merged_paths e =
    Virtual_address.Htbl.fold (fun _addr p1 -> function
        | None -> Some p1
        | Some p2 -> Some (Path_state.merge p1 p2)) 
      e.merges None

  module Directives = struct
    let at address e =
      match Virtual_address.Htbl.find e.directives.store address with
      | q -> Some q
      | exception Not_found -> None

    let address d =
      let img = Kernel_functions.get_img () in
      match Loader_utils.Binary_loc.to_virtual_address
              ~img (Directive.loc d) with
      | Some a -> a
      | None -> assert false
    ;;

    (** counts goals to be reached (as opposed to annotations like cut) *)
    let count e =
      let d = e.directives.store in
      Virtual_address.Htbl.fold (fun _addr queue acc ->
          Queue.fold (fun acc g ->
              acc + 
              Directive.(match directive g with
                  | Choice _
                  | Cut
                  | Assume _ -> 0
                  | Reach _
                  | Enumerate _ -> 1
                )) acc queue) d 0


    let remove address e =
      Virtual_address.Htbl.remove e.directives.store address

    let update address g e =
      Virtual_address.Htbl.replace e.directives.store address g

    (** returns whether there are still goals to be reached
     * (as opposed to only [cut]s *)
    let has e =
      let ngoals = count e in
      ngoals <> 0

    let extend_directive store vaddr d =
      Logger.debug ~level:2 "Extend %a with directive %a"
        vaddr_pp vaddr Directive.pp d;
      match Virtual_address.Htbl.find store vaddr with
      | q -> Queue.add d q
      | exception Not_found ->
         let q = Queue.create () in
         Queue.add d q;
         Virtual_address.Htbl.add store vaddr q
    ;;

    (* Initialize goal table from cli specification *)
    let init () =
      let gs = Directives.get () in
      let len = List.length gs in
      (* Add one more slot to the hashtables if needed later on *)
      let h = Virtual_address.Htbl.create (len + 1) in
      let add_actions gs =
        List.iter
          (fun g ->
             Logger.debug ~level:2
               "Add action %a" Directive.pp g;
             extend_directive h (address g) g
            ) gs in
      add_actions gs;
      let open Basic_types in
      let add_action f int_set =
        Int.Set.iter
          (fun n ->
             let vaddr = Virtual_address.create n in
             let d = f vaddr in
             extend_directive h vaddr d
          ) int_set in
      add_action
        (fun vaddr ->
          let loc = Loader_utils.Binary_loc.address vaddr in
          Directive.reach loc) (GoalAddresses.get ());
      add_action (fun vaddr -> Directive.cut @@
                                 Loader_utils.Binary_loc.address vaddr)
        (AvoidAddresses.get ());
      { store = h }

    module Enumeration = struct
      type r =
        | No_record
        | No_values of Bv_set.t Etbl.t
        | Values of Bv_set.t * Bv_set.t Etbl.t

      let access va e g =
        match Virtual_address.Htbl.find g.enumerations va with
        | exception Not_found -> No_record
        | h ->
           match Etbl.find h e with
           | exception Not_found -> No_values h
           | bvs -> Values (bvs, h)
      ;;

      let record va e bvs g =
        let log =
          Logger.debug ~level:3
            "@[<h>Enumeration: recorded %d new values %@ %a@]"
        in
        match access va e g with
        | Values (vs, h) ->
           let n0 = Bv_set.cardinal vs in
           let v' = Bv_set.union bvs vs in
           let n1 = Bv_set.cardinal v' in
           log (n1 - n0) vaddr_pp va;
           Etbl.replace h e v'
        | No_values h ->
           Etbl.add h e bvs
        | No_record ->
           let etbl = Etbl.create 3 in
           Etbl.add etbl e bvs;
           Virtual_address.Htbl.add g.enumerations va etbl
      ;;


      let count va e g =
        match access va e g with
        | No_record
        | No_values _ -> 0
        | Values (vs, _) -> Bv_set.cardinal vs
      ;;

      let get va e g =
        match access va e g with
        | No_record | No_values _ -> []
        | Values (vs, _) -> vs

    end
  end

  module Path = struct
    exception Empty_worklist

    let choose e =
      let pick_one w =
        match W.pop w with
        | path_state, worklist ->
          begin
            Logger.debug "Selecting path #%d at %a (among %d)"
              (Path_state.id path_state) vaddr_pp (Path_state.virtual_address path_state) (W.length w) ;
            (match path_state.Path_state.merge_point with
            | None -> ()
            | Some(addr) ->
              begin
                let current = Dba_types.Caddress.Htbl.find e.to_merge addr in
                let next = { current with in_worklist_count = current.in_worklist_count - 1; } in
                Logger.info "now %i (-1) waiting for %a" next.in_worklist_count Dba_printer.Ascii.pp_code_address addr;
        Dba_types.Caddress.Htbl.replace e.to_merge addr next
              end);
              { e with worklist }, path_state
          end
        | exception Not_found ->
          begin
            if Dba_types.Caddress.Htbl.length e.to_merge <> 0 then
              Logger.result "Missing merges !"
            else if !(e.complete) then
              Logger.result "Goal unreachable."
            else
              Logger.warning "Empty path worklist but search was incomplete. Halting ...";
            raise Empty_worklist
          end
      in pick_one e.worklist
    ;;

    let add path_state e =
      let worklist = W.push path_state e.worklist in
      (match path_state.Path_state.merge_point with
       | None -> ()
       | Some(addr) ->
          let current =
            try
              Dba_types.Caddress.Htbl.find e.to_merge addr
            with Not_found ->
              { in_worklist_count = 0; result = None }
          in
          let next = { current with in_worklist_count = current.in_worklist_count + 1; } in
          Logger.info "now %i (+1) waiting for %a" next.in_worklist_count Dba_printer.Ascii.pp_code_address addr;
          Dba_types.Caddress.Htbl.replace e.to_merge addr next
      );
      { e with worklist }
    ;;

  end

  let from_address ~entrypoint =
    let level = 4 in
    let initial_instruction = decode entrypoint in
    Logger.debug ~level "Creating symbolic store ...";
    let symbolic_state = Sse_symbolic.State.create () in
    Logger.debug ~level "Creating initial path state ...";
    let initial_path_state =
      Path_state.create symbolic_state initial_instruction in
    Logger.debug ~level "Creating worklist ...";
    let worklist = W.singleton initial_path_state in
    Logger.debug ~level "@[<h>Initializing SSE goals ...@]";
    let directives = Directives.init () in
    let enumerations = Virtual_address.Htbl.create 7 in
    let complete = ref true in
    let to_merge = Dba_types.Caddress.Htbl.create 13 in
    let merges = Virtual_address.Htbl.create 13 in
    { worklist; directives; enumerations; complete; to_merge; merges }


  let wl_size path_state = W.length path_state.worklist

end

module type GLOBAL_ENV = sig
  type t

  val wl_size : t -> int

  module Directives : sig
    val at : Virtual_address.t -> t -> Directive.t Queue.t option
    val has : t -> bool
    val update : Virtual_address.t -> Directive.t Queue.t -> t -> unit
    val remove : Virtual_address.t -> t -> unit

    module Enumeration : sig
      val record: Virtual_address.t -> Dba.Expr.t -> Bitvector.t list -> t -> unit
      val count : Virtual_address.t -> Dba.Expr.t -> t -> int
      val get   : Virtual_address.t -> Dba.Expr.t -> t -> Bitvector.t list
    end

  end


  (** {3 Constructor }*)
  val from_address : entrypoint:Virtual_address.t -> t

  module Path : sig
    exception Empty_worklist

    val choose : t -> t * Path_state.t
    (** [choose_path e] pops a new path [p] from environment [e],
        and returns both the path and the environment without this path.

        @raise Empty_worklist *)

    val add : Path_state.t -> t -> t
    (** [add_path p e] register path [p] in the worlist
        of environment [e].
    *)
  end

  val mark_as_incomplete : t -> unit
  val complete : t -> bool

  (** to call when a path state to be merged reaches its merge point.
   * returns the fully merged path state, or None if other paths state
   * must be waited for *)
  val merge_ready_path_state : t -> Path_state.t -> Path_state.t option

  (** marks that a path_state reached a goal and returns the merge of all 
   * path_states having reaches this goal so far *)
  val reach_and_merge: t -> Path_state.t -> Path_state.t

  (** returns the merge of all paths input to [reach_and_merge]. 
   * Returns None if no such path was registered. *)
  val drain_merged_paths: t -> Path_state.t option

end

module Dfs_global:GLOBAL_ENV = G(W_stack)

module Bfs_global:GLOBAL_ENV = G(W_queue)

module Nurs_global:GLOBAL_ENV = G(Random_heap)
