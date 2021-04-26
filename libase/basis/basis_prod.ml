(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Codex plugin of Frama-C.                     *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(* TODO: produce of lattices should go in prod_lattice.  Here, we have
   only the produce of transfer functions.  *)

open Basis_sig;;
open Lattice_sig;;

(* Default (non-reduced) product. We could apply a reduction function
   (e.g. propagate the bottoms) *)
module Prod_Binary_Forward
  (A:With_Binary_Forward with type boolean = Quadrivalent_basis.boolean)
  (B:With_Binary_Forward
   with type boolean = A.boolean)
  (* :With_Binary_Forward *) =
struct
  type boolean = A.boolean
  type binary = A.binary * B.binary
  module Binary_Forward = struct

    module ABF = A.Binary_Forward
    module BBF = B.Binary_Forward

    let assume ~size cond (a,b) =
      (ABF.assume ~size cond a, BBF.assume ~size cond b)
    let buext ~size ~oldsize (a,b) =
      (ABF.buext ~size ~oldsize a, BBF.buext ~size ~oldsize b)
    let bsext ~size ~oldsize (a,b) =
      (ABF.bsext ~size ~oldsize a, BBF.bsext ~size ~oldsize b)
    let bofbool ~size x =
      (ABF.bofbool ~size x, BBF.bofbool ~size x)      
    let bextract ~size ~index ~oldsize (a,b) =
      (ABF.bextract ~size ~index ~oldsize a, BBF.bextract ~size ~index ~oldsize b)

    let bop2 fa fb = fun ~size (a1,b1) (a2,b2) ->
      (fa ~size a1 a2, fb ~size b1 b2)

    let bop2_flags fa fb = fun ~size ~nsw ~nuw (a1,b1) (a2,b2) ->
      (fa ~size ~nsw ~nuw a1 a2, fb ~size ~nsw ~nuw b1 b2)
    
    
    let biadd = bop2_flags ABF.biadd BBF.biadd
    let bisub = bop2_flags ABF.bisub BBF.bisub
    let bimul = bop2_flags ABF.bimul BBF.bimul
    let bisdiv = bop2 ABF.bisdiv BBF.bisdiv
    let bismod = bop2 ABF.bismod BBF.bismod
    let biudiv = bop2 ABF.biudiv BBF.biudiv
    let biumod = bop2 ABF.biumod BBF.biumod
    let band = bop2 ABF.band BBF.band
    let bor = bop2 ABF.bor BBF.bor
    let bxor = bop2 ABF.bxor BBF.bxor
    let bshl = bop2_flags ABF.bshl BBF.bshl
    let bashr = bop2 ABF.bashr BBF.bashr
    let blshr = bop2 ABF.blshr BBF.blshr

    let bconcat ~size1 ~size2 (a1,b1) (a2,b2) =
      ABF.bconcat ~size1 ~size2 a1 a2, BBF.bconcat ~size1 ~size2 b1 b2

    let bpred fa fb = fun (a1,b1) (a2,b2) ->
      Quadrivalent_basis.Boolean_Lattice.inter (fa a1 a2) (fb b1 b2)

    let beq   ~size = bpred (ABF.beq   ~size) (BBF.beq   ~size)
    let bisle ~size = bpred (ABF.bisle ~size) (BBF.bisle ~size)
    let biule ~size = bpred (ABF.biule ~size) (BBF.biule ~size)

    let biconst ~size k = (ABF.biconst ~size k, BBF.biconst ~size k)
    let buninit ~size = (ABF.buninit ~size, BBF.buninit ~size)
    let bunknown ~size = (ABF.bunknown ~size, BBF.bunknown ~size)
      
    let valid ~size (a,b) =
      Quadrivalent_basis.Boolean_Lattice.inter (ABF.valid ~size a) (BBF.valid ~size b)
    ;;

    let bshift ~size ~offset ~max _ = assert false
    let bindex ~size _ = assert false
    
  end
end

module Prod_Binary_Backward
  (A:With_Binary_Backward with type boolean = Quadrivalent_basis.boolean)
  (B:With_Binary_Backward
   with type boolean = A.boolean) =
struct

  module ABB = A.Binary_Backward;;
  module BBB = B.Binary_Backward;;  
  
  let coalesce_nones olda oldb a b = match a,b with
    | None,None -> None
    | Some v, None -> Some(v, oldb)
    | None, Some v -> Some(olda, v)
    | Some va, Some vb -> Some(va,vb)

  
  let bpred2 op1 op2 (a1,a2) (b1,b2) res =
    let (newa1,newb1) = op1 a1 b1 res in
    let (newa2,newb2) = op2 a2 b2 res in
    (coalesce_nones a1 a2 newa1 newa2,
     coalesce_nones b1 b2 newb1 newb2)
  ;;

  let beq   ~size = bpred2 (ABB.beq   ~size) (BBB.beq   ~size)
  let biule ~size = bpred2 (ABB.biule ~size) (BBB.biule ~size)
  let bisle ~size = bpred2 (ABB.bisle ~size) (BBB.biule ~size)

  let bop2 op1 op2 (a1,a2) (b1,b2) (res1,res2) =
    let (newa1,newb1) = op1 a1 b1 res1 in
    let (newa2,newb2) = op2 a2 b2 res2 in
    (coalesce_nones a1 a2 newa1 newa2,
     coalesce_nones b1 b2 newb1 newb2)

  let bop2_flags ~size ~nsw ~nuw op1 op2 =
    bop2 (op1 ~size ~nsw ~nuw) (op2 ~size ~nsw ~nuw)        
  let bop2 ~size op1 op2 = bop2 (op1 ~size) (op2 ~size)


  let biadd = bop2_flags ABB.biadd BBB.biadd
  let bisub = bop2_flags ABB.bisub BBB.bisub    
  let bimul = bop2_flags ABB.bimul BBB.bimul
  let band = bop2 ABB.band BBB.band
  let bor = bop2 ABB.bor BBB.bor
  let bxor = bop2 ABB.bxor BBB.bxor
  let bisdiv = bop2 ABB.bisdiv BBB.bisdiv
  let bismod = bop2 ABB.bismod BBB.bismod    
  let biudiv = bop2 ABB.biudiv BBB.biudiv
  let biumod = bop2 ABB.biumod BBB.biumod
  let bashr = bop2 ABB.bashr BBB.bashr
  let blshr = bop2 ABB.blshr BBB.blshr
  let bshl = bop2_flags ABB.bshl BBB.bshl
    
    
  (* let bitimes ~size _ _ _ = None
   * let nondet ~size l result = List.map (fun _ -> None) l *)
  let assume ~size _cond _store _result = assert false
  let bsext ~size ~oldsize _ _ = assert false
  let buext ~size ~oldsize _ _ = assert false
  let bofbool ~size a res = assert false                               
  let bconcat ~size1 ~size2 a b _ = assert false
  let bextract ~size ~index ~oldsize _ _ = assert false
  let valid ~size _ _ = assert false

  let bshift ~size ~offset ~max _ = assert false
  let bindex ~size _ = assert false    
    
end

module Prod_Binary
  (A:Binary_Basis with type boolean = Quadrivalent_basis.boolean)
  (B:Binary_Basis
   with type boolean = A.boolean) =
struct
  let name = "Prod_Binary(" ^ A.name ^ "*" ^ B.name ^ ")";;
  
  module Boolean_Lattice = A.Boolean_Lattice
  module Binary_Lattice = struct
    (* include Prod_Lattice.Prod2_With_Inter_Bottom(A.Binary_Lattice)(B.Binary_Lattice) *)
    (* include Datatype_sig.Prod2(A.Binary_Lattice)(B.Binary_Lattice.Binary_Lattice);; *)
    let equal (a1,b1) (a2,b2) = A.Binary_Lattice.equal a1 a2 && B.Binary_Lattice.equal b1 b2
    let compare (a1,b1) (a2,b2) =
      let ra = A.Binary_Lattice.compare a1 a2 in
      if ra != 0 then ra
      else B.Binary_Lattice.compare b1 b2
    let sdbm x y = y + (x lsl 16) + (x lsl 6) - x;;          
    let hash (a,b) = sdbm (A.Binary_Lattice.hash a) (B.Binary_Lattice.hash b)
    let pretty ~size fmt (a,b) = Format.fprintf fmt "(%a,%a)" (A.Binary_Lattice.pretty ~size) a (B.Binary_Lattice.pretty ~size) b
    
    let includes (a1,a2) (b1,b2) = A.Binary_Lattice.includes a1 b1 && B.Binary_Lattice.includes a2 b2        
    let widen ~size ~previous:(a1,a2) (b1,b2) = (A.Binary_Lattice.widen ~size ~previous:a1 b1),(B.Binary_Lattice.widen ~size ~previous:a2 b2)
    let includes_or_widen ~size ~previous next =
      if includes previous next then (true,previous)
      else (false,widen ~size ~previous next)
    ;;
    let join ~size (a1,a2) (b1,b2) = (A.Binary_Lattice.join ~size a1 b1, B.Binary_Lattice.join ~size a2 b2)
    let bottom = A.Binary_Lattice.bottom,B.Binary_Lattice.bottom
    let is_bottom (a,b) = A.Binary_Lattice.is_bottom a || B.Binary_Lattice.is_bottom b
    let inter ~size (a1,a2) (b1,b2) = (A.Binary_Lattice.inter ~size a1 b1, B.Binary_Lattice.inter ~size a2 b2)
    let singleton ~size k = A.Binary_Lattice.singleton ~size k,
                            B.Binary_Lattice.singleton ~size k
    type t = A.Binary_Lattice.t * B.Binary_Lattice.t
  end
  module Boolean_Forward = A.Boolean_Forward
  module Boolean_Backward = A.Boolean_Backward
  type boolean = A.boolean
  type binary = Binary_Lattice.t

  module Binary_Forward = struct
    module AB = Prod_Binary_Forward(A)(B)
    include AB.Binary_Forward
  end
  module Binary_Backward = struct
    module AB = Prod_Binary_Backward(A)(B)
    include AB
  end
  let truth_value bool1 = bool1
  include Basis_sig.Dummy_Conversions

  (* TODO: Build an intermediary set. *)
  let binary_fold_crop ~size bin ~inf ~sup acc f = assert false

  let binary_is_empty ~size (a,b) = A.binary_is_empty ~size a || B.binary_is_empty ~size b
  
  let binary_to_ival ~signed ~size (a,b) =
    Framac_ival.Ival.narrow (A.binary_to_ival ~signed ~size a) (B.binary_to_ival ~signed ~size b)

  let binary_is_singleton ~size (a,b) =
    match A.binary_is_singleton ~size a with
    | None -> B.binary_is_singleton ~size b
    | x -> x

end
