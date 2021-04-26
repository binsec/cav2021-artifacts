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

let decode vaddress = Disasm_core.decode vaddress |> fst

let _jt_to_addr addr = function
  | Dba.JInner idx -> Dba_types.Caddress.reid addr idx
  | Dba.JOuter a -> a

let instr_succ ~skip_calls instr =
  let open Dba.Instr in
  match instr with
  | Assign (_, _, id)
  | Assert (_, id)
  | NondetAssume (_, _, id)
  | Nondet (_, _, id)
  | Undef (_, id)
  | Free (_, id)
  | Malloc (_, _, id)
  | Print (_, id)
  | Assume (_, id) -> Some([Dba.JInner id])
  | Stop _ -> Some([])
  | DJump _ -> None (* unknown *)
  | SJump (_, Some(Dba.Call next)) when skip_calls -> Some([ Dba.JOuter next ])
  | SJump (jt, _) -> Some([jt])
  | If (_, jt, idx) -> Some([jt; Dba.JInner idx])


module G = Cfg.Make
    (struct
      type t = Dba_types.Caddress.t
      let compare = Dba_types.Caddress.compare
      let hash = Hashtbl.hash
      let equal = Dba_types.Caddress.equal
    end)
    (struct
      type t = Dba.Instr.t
      let hash = Hashtbl.hash
      let equal = (=)
    end)
    (struct
      type t = unit
      let hash _ = 0
      let equal _ _ = true
    end)

let rec populate_from ~skip_calls cfg vaddr depth =
  Sse_options.Logger.debug ~level:4 "populate %a (%d)" Virtual_address.pp vaddr depth;
  let root_addr = Dba_types.Caddress.of_virtual_address vaddr in
  if G.mem_vertex_a cfg root_addr = None then
    let block = decode vaddr in
    let i_to_a index = Dba_types.Caddress.reid root_addr index in
    Sse_options.Logger.debug ~level:4 "%a" Dhunk.pp block.Instruction.dba_block;
    Dhunk.iteri
      ~f:(fun index dba_instr ->
          let addr = i_to_a index in
          if depth - index >= 0 then begin
            G.add_inst cfg addr dba_instr;
            let targets =
              match instr_succ ~skip_calls dba_instr with None -> [] | Some x -> x in
            List.iter (function
                | Dba.JInner id ->
                  if depth - id >= 0 then
                    G.add_edge_a cfg addr (i_to_a id)
                | Dba.JOuter a ->
                  populate_from ~skip_calls
                    cfg
                    (Dba_types.Caddress.to_virtual_address a)
                    (depth - index - 1);
                  if depth - index - 1 >= 0 then begin
                    assert (G.mem_vertex_a cfg a <> None);
                    G.add_edge_a cfg addr a
                  end
              ) targets
          end
        )
      block.Instruction.dba_block


module Pp = Graph.Graphviz.Dot(struct
    include G
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let vertex_name v =
      let target = Format.asprintf "%a %a"
          Dba_printer.Ascii.pp_code_address (G.V.addr v)
          (Print_utils.pp_opt Dba_printer.Ascii.pp_instruction) (G.V.inst v)
      in
      "\"" ^ (String.escaped target) ^ "\""
    let get_subgraph _ = None
    let default_edge_attributes _ = []
    let edge_attributes _ = []
  end)

let pp_as_dot = Pp.fprint_graph
