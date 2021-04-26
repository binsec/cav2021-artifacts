(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(* Abstract domains and their operations.  *)
(* These can be viewed as signatures in the Tagless final approach
   [http://okmij.org/ftp/tagless-final/] *)


module type Arity = sig
  (* Symbols and their arities. 'r represents the result type. *)
  type ('r) ar0
  type ('a,'r) ar1
  type ('a,'b,'r) ar2
  type ('a,'b,'c,'r) ar3
  type ('a,'r) variadic
end


(* Note: in the following, we distinguish between backward and forward
   because there is no need to implement backward transfer functions
   for symbols with arity 0. *)

(****************************************************************)
(* Boolean transfer functions.  *)

module type Boolean_Backward = sig
  type boolean
  module Arity:Arity
  val not: (boolean,boolean) Arity.ar1
  val (&&): (boolean,boolean,boolean) Arity.ar2
  val (||): (boolean,boolean,boolean) Arity.ar2
  val assume: (boolean,boolean,boolean) Arity.ar2
end

module type Boolean_Forward = sig
  include Boolean_Backward
  val unknown: ?level:int -> boolean Arity.ar0 (* No leve = innermost, which is the most precise. *)
  val true_: boolean Arity.ar0
  val false_: boolean Arity.ar0
end

(* TODO: 

    - some operators are redundant; in particular false is just
     "not true". or could also be simulated using a double negation.
   
      Maybe we could require only "default" operators, or provide a
      mean to compute complex operators using the default ones.
*)

(****************************************************************)
(* Integer transfer functions.  *)

module type Integer_Backward = sig
  type integer
  type boolean

  module Arity:Arity

  (* Minimum versions. *)
  val itimes: Z.t -> (integer,integer) Arity.ar1
  val iadd: (integer,integer,integer) Arity.ar2
  val imul: (integer,integer,integer) Arity.ar2
  (* Note: this is truncated (C99-like) integer division. 
     MAYBE: rename to itdiv/itmod? *)
  val idiv: (integer,integer,integer) Arity.ar2
  val imod: (integer,integer,integer) Arity.ar2
  val ishl: (integer,integer,integer) Arity.ar2
  val ishr: (integer,integer,integer) Arity.ar2
  val iand: (integer,integer,integer) Arity.ar2
  val ior: (integer,integer,integer) Arity.ar2
  val ixor: (integer,integer,integer) Arity.ar2            
  val isub: (integer,integer,integer) Arity.ar2
  val ieq: (integer,integer,boolean) Arity.ar2
  val ile: (integer,integer,boolean) Arity.ar2
  val assume: (boolean,integer,integer) Arity.ar2
end

module type Integer_Forward_Min = sig
  include Integer_Backward
  val iconst: Z.t -> integer Arity.ar0
  val iunknown: unit -> integer Arity.ar0
end

module type Integer_Forward = sig
  include Integer_Forward_Min
  (* These can be defined from the others, but it may be more
     efficient to implement them directly (no need to build temporary
     values...). They are also often convenient to use directly. *)
  (* val ilt: (integer,integer,boolean) Arity.ar2 *)
  val zero: integer Arity.ar0
  val one: integer Arity.ar0
end

(****************************************************************)
(* Binary transfer functions.  *)

(* TODO: Biconst should take an "integer"; biconst means that it is
   a binary made from a certain integer (thus it is not a constant
   anymore; it should be named "binteger" instead). *)

(* Note: the size argument, when provided, refers to the size of the
   result. *)
module type Binary_Backward = sig
  type binary
  type boolean
  module Arity: Arity
  (* nsw means no signed overflow, nuw means no unwrapped overflow. No
     (signed) overflow means that when taking the signed integer value
     of the variable, then performing the operation remains in the
     signed range (and respectively for unsigned overflow). Transfer
     functions can take advantage of this fact to improve their
     precision. *)
  val biadd: size:int -> nsw:bool -> nuw:bool -> (binary,binary,binary) Arity.ar2
  val bisub: size:int -> nsw:bool -> nuw:bool -> (binary,binary,binary) Arity.ar2    
  val bimul: size:int -> nsw:bool -> nuw:bool -> (binary,binary,binary) Arity.ar2
  val bshl: size:int  -> nsw:bool -> nuw:bool -> (binary,binary,binary) Arity.ar2
  val bashr: size:int -> (binary,binary,binary) Arity.ar2
  val blshr: size:int -> (binary,binary,binary) Arity.ar2

  
  val beq:   size:int -> (binary,binary,boolean) Arity.ar2
  val bisle: size:int -> (binary,binary,boolean) Arity.ar2
  val biule: size:int -> (binary,binary,boolean) Arity.ar2

  (* First argument become most significant. *)
  val bconcat: size1:int -> size2:int -> (binary,binary,binary) Arity.ar2
  val bextract: size:int -> index:int -> oldsize:int -> (binary,binary) Arity.ar1
  val band: size:int -> (binary,binary,binary) Arity.ar2
  val bor: size:int -> (binary,binary,binary) Arity.ar2
  val bxor: size:int -> (binary,binary,binary) Arity.ar2

  (* TODO:  buext is just concatenation with zero. *)
  (* MAYBE: Provide old size here too? *)
  val buext: size:int -> oldsize:int -> (binary,binary) Arity.ar1
  val bsext: size:int -> oldsize:int -> (binary,binary) Arity.ar1
  val bisdiv: size:int -> (binary,binary,binary) Arity.ar2
  val bismod: size:int -> (binary,binary,binary) Arity.ar2
  val biudiv: size:int -> (binary,binary,binary) Arity.ar2
  val biumod: size:int -> (binary,binary,binary) Arity.ar2

  (* Returns the bitvector 0 for false, and 1 for true, for a given
     size. *)
  val bofbool: size:int -> (boolean,binary) Arity.ar1

  (* TODO: Should be a predicate on memory, and take both a binary and memory. *)
  val valid: size:int -> (binary,boolean)  Arity.ar1
  (* [max], if not None, limits further pointer arithmetics: one cannot go beyond max. *)
  (* MAYBE: separate this between bshift and a new operator [bnarrow]. *)

  (* Note: offset and max are in bytes, not in bits. *)
  val bshift: size:int -> offset:int -> max:int option -> (binary,binary) Arity.ar1 (* b + numeric offset. *)
  val bindex: size:int -> int -> (binary,binary,binary) Arity.ar2 (* b + k * exp. *)

  val assume: size:int -> (boolean,binary,binary) Arity.ar2
end

module type Binary_Forward = sig
  include Binary_Backward
  val biconst: size:int -> Z.t -> binary Arity.ar0
  val bunknown: size:int -> binary Arity.ar0
  val buninit: size:int -> binary Arity.ar0
end

(****************************************************************)
(* Memory transfer functions.  *)
  
module type Memory_Backward = sig
  type memory
  type binary
  type boolean
  module Arity: Arity
  val load: size:int -> (memory,binary,binary) Arity.ar2
  val store: size:int -> (memory,binary,binary,memory) Arity.ar3
  val assume: (boolean,memory,memory) Arity.ar2
  (* Fixed-size memcpy(store,from_,to_). *)
  val memcpy: size:int -> (memory,binary,binary,memory) Arity.ar3
  val malloc: id:Term_constructors.Malloc_id.t -> malloc_size:int ->
    (memory,binary * memory) Arity.ar1
  val free: (memory,binary,memory) Arity.ar2
  val unknown: level:int -> memory Arity.ar0
end

module type Memory_Forward = sig
  include Memory_Backward
end

(****************************************************************)
(* Tuple transfer functions.  *)

module type Tuple_Forward = sig

  (* Note: tuple operations are unsafe; we always provide the type of
     the tuple arguments (and results if different), which can be
     checked dynamically. *)
  type tuple
  (* MAYBE: Instead of Term_types.t, we could use an abstract "proof" that tells
     that something has the correct type. *)
  (* TODO: 
     - Define an abstract type 'a typ;
     - Replace the any type by Any: 'a typ * 'a -> any (used only by tuple_create);
     - Use the type, instead of any, wherever feasible.

 *)
  module Arity:Arity
  (* Note:  the type is needed by e.g. lift_integer_to_binary. *)
  (* val tuple_get: int -> Term_types.t -> (tuple,any) Arity.ar1       *)
  (* val tuple_create: Term_types.t Immutable_dynamic_array.t -> *)
  (*   (any Immutable_dynamic_array.t,tuple) Arity.ar1 *)
  val tuple_nondet: Term_types.t Immutable_dynamic_array.t -> (tuple,tuple) Arity.variadic
  val builtin: Builtin.t ->
    from_types:Term_types.t Immutable_dynamic_array.t ->
    to_types:Term_types.t Immutable_dynamic_array.t ->
    (tuple,tuple) Arity.ar1
      
  val var: tuple Arity.ar0

  (* Because of the binder, mu must be handled specially.  *)
  
end


(****************************************************************)
(* All. *)

module type All_Forward = sig
  type memory
  type binary
  type boolean
  type integer
  module Arity:Arity
  module Boolean:Boolean_Forward
    with module Arity := Arity
     and type boolean := boolean
  module Integer:Integer_Forward
    with module Arity := Arity
     and type boolean := boolean
     and type integer := integer
  module Binary:Binary_Forward
    with module Arity := Arity
     and type boolean := boolean
     and type binary := binary
  module Memory:Memory_Forward
    with module Arity := Arity
     and type boolean := boolean
     and type binary := binary
     and type memory := memory
end



