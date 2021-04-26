(**************************************************************************)
(*                                                                        *)
(*  This file is part of Codex.                                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
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

(* "Conversions"; i.e. passing the same transfer function (currently:
   with same types for dimension identifiers) with minimal changes. *)

open Transfer_functions

module type Conversion = sig
  module From_Arity:Arity
  module To_Arity:Arity
  val ar0: 'r From_Arity.ar0 -> 'r To_Arity.ar0
  val ar1: ('a,'r) From_Arity.ar1 -> ('a,'r) To_Arity.ar1
  val ar2: ('a,'b,'r) From_Arity.ar2 -> ('a,'b,'r) To_Arity.ar2
  val ar3: ('a,'b,'c,'r) From_Arity.ar3 -> ('a,'b,'c,'r) To_Arity.ar3
  val variadic: ('a,'r) From_Arity.variadic -> ('a,'r) To_Arity.variadic
end

module Convert_Boolean_Forward
  (C:Conversion)
  (F:Boolean_Forward with module Arity := C.From_Arity):
  Boolean_Forward with module Arity := C.To_Arity
                   and type boolean = F.boolean
                                        

module Convert_Integer_Forward
  (C:Conversion)
  (F:Integer_Forward with module Arity := C.From_Arity):
  Integer_Forward with module Arity := C.To_Arity
                   and type boolean = F.boolean
                   and type integer = F.integer
                                      

module Convert_Binary_Forward
  (C:Conversion)
  (F:Binary_Forward with module Arity := C.From_Arity):
  Binary_Forward with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type binary = F.binary

module Convert_Memory_Forward
  (C:Conversion)
  (F:Memory_Forward with module Arity := C.From_Arity):
  Memory_Forward with module Arity := C.To_Arity
                  and type boolean = F.boolean
                  and type binary = F.binary
                  and type memory = F.memory



