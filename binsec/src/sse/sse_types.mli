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

module Extra : sig type t end

module C : Instr_cfg.S with type addr = Virtual_address.t
                        and type inst = Instruction.t
                        and type symb = Extra.t

(** Pretty print the virtual address with the name of the function
 * it belongs to. (Actually, the name of the closest symbol
 * with a lower address, plus a few fragile heuristics) *)
val vaddr_pp: Format.formatter -> Virtual_address.t -> unit

module Path_state : sig
  type t

  val create :
    ?depth:int ->
    ?address_counters:
      Sse_options.Address_counter.t Virtual_address.Map.t ->
    ?cfg:C.t ->
    ?block_index:int ->
    Sse_symbolic.State.t -> Instruction.t ->
    t

  val branch : t -> t

  (** {2 Accessors} *)

  val dba_instruction : t -> Dba.Instr.t
  val current_statement : t -> Dba_types.Statement.t
  val virtual_address : t -> Virtual_address.t
  val location : t -> Dba_types.Caddress.t
  val symbolic_state : t -> Sse_symbolic.State.t
  val block_index : t -> int
  val id : t -> int
  val solver_calls : t -> int
  val paths_created : unit -> int
  val is_depth_ok : t -> bool
  val cfg : t -> C.t
  val entrypoint : t -> Virtual_address.t
  val inst : t -> Instruction.t
  val merge_point : t -> Dba.address option

  val counter : Virtual_address.t -> t -> Sse_options.Address_counter.t option

  val formula : t -> Formula.formula

  (** {2 Modifiers} *)

  val set_counter :
    Virtual_address.t  -> Sse_options.Address_counter.t -> t -> t
  val set_merge_point : Dba.address option -> t -> t

  val set_block_index : int -> t -> t
  val set_symbolic_state : Sse_symbolic.State.t -> t -> t

  val incr_solver_calls : t -> t
  val reset_solver_calls : t -> t

  val set_address_counters :
    Sse_options.Address_counter.t Virtual_address.Map.t -> t -> t

  val goto_vaddr : Virtual_address.t -> t -> t
  val goto : Dba_types.Caddress.t -> t -> t

  val add_assumption : Formula.bl_term -> t -> t
  val add_assertion : Formula.bl_term -> t -> t

  val assign_from_image: addr:Bitvector.t -> size:int -> t -> t
  val with_init_mem_at: addr:Bitvector.t -> size:int -> t -> t
  val address_belongs_to_init: addr:Bitvector.t -> t -> bool

  val merge: t -> t -> t

  (** undeclare every variable except path constraint *)
  val strip: t -> t

  (** {2 Printers} *)

  val pp_loc : Format.formatter -> t -> unit
  val pp_path : Format.formatter -> t -> unit

  (** dumps trace to file specified with -sse-address-trace file*)
  val dump_path : t -> unit

  val dump_cfg : filename:string -> t -> unit
end

module type GLOBAL_ENV = sig
  type t

  (** {2 Accessors} *)

  val wl_size : t -> int
  (** [wl_size e] returns the size of the current worklist
      for environment [e].
   *)

  module Directives : sig
    val at : Virtual_address.t -> t -> Directive.t Queue.t option
    (** [at va e] returns the user-defined goals for this address.
        When no goals are defined [] is returned.
   *)

    val has : t -> bool
    (** [has e] is [true] if there are still some goals to deal with. *)

    val update : Virtual_address.t -> Directive.t Queue.t -> t -> unit
    (** [update va a e] replaces the action linked to [va] in [e] by [a].
     *)

    val remove : Virtual_address.t -> t -> unit

    module Enumeration : sig
      val record: Virtual_address.t -> Dba.Expr.t -> Bitvector.t list -> t -> unit
      val count : Virtual_address.t -> Dba.Expr.t -> t -> int
      val get   : Virtual_address.t -> Dba.Expr.t -> t -> Bitvector.t list
    end
  end


  (** {2 Constructors} *)

  val from_address : entrypoint:Virtual_address.t -> t

  (** {2 Modifiers} *)

  module Path : sig
    exception Empty_worklist

    val choose : t -> t * Path_state.t
    (** [choose_path e] pops a new path [p] from environment [e],
      and returns both the path and the environment without this path.

      @raise Empty_worklist once worklist has been emptied *)

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

module Dfs_global : GLOBAL_ENV

module Bfs_global : GLOBAL_ENV

module Nurs_global : GLOBAL_ENV
(** Non uniformed randomized search heuristics *)
