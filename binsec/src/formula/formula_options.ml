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

include Cli.Make(
struct
  let shortname = "fml"
  let name = "Formulas"
end)

module OptimAll = Builder.False(
  struct
    let name = "optim-all"
    let doc = "Enable all the following optimizations"
  end
  )

module OptimCst = Builder.False(
  struct
    let name = "optim-cst"
    let doc = "Enable constant propagation"
  end
  )

module OptimAi = Builder.False(
  struct
    let name = "optim-ai"
    let doc = "Enable abstract interpretation (intervals, congruence) and AI read over write"
  end
  )

module OptimItv = Builder.False(
  struct
    let name = "optim-itv"
    let doc = "Enable intervals in read-over-write"
  end
  )

module OptimPrn = Builder.False(
  struct
    let name = "optim-prn"
    let doc = "Enable pruning and inlining"
  end
  )

module OptimRbs = Builder.False(
  struct
    let name = "optim-rbs"
    let doc = "Enable rebasing in read-over-write"
  end
  )

module OptimRow = Builder.False(
  struct
    let name = "optim-row"
    let doc = "Enable read-over-write"
  end
  )

module OptimRowUnfold = Builder.False(
  struct
    let name = "optim-row-unfold"
    let doc = "Enable read-over-write unfolding of irrelevant prefix of an array"
  end
  )

module OptimSsa = Builder.False(
  struct
    let name = "optim-ssa"
    let doc = "Enable A normal form"
  end
  )

module TaintRow = Builder.False(
  struct
    let name = "taint-row"
    let doc = "Enable read-over-write aware taint"
  end
  )

type universal_mode = Quantifier | Taint

module UniversalMode =
  Builder.Variant_choice_assoc(struct
      type t = universal_mode
      let name = "universal-mode"
      let doc = "How to do solve universally quantified formulas"
      let default = Quantifier
      let assoc_map = [
          "quantifier", Quantifier;
          "taint", Taint;
        ]
    end)

module UnquantifyMemory = Builder.False(
  struct
    let name = "unquantify-memory"
    let doc = "Rewrite the formula to keep memory out of the universal quantifier. Requires -fml-universal-mode quantifier"
  end
  )

module UnquantifyMemoryApprox = Builder.False(
  struct
    let name = "unquantify-memory-approx"
    let doc = "Over constrain the formula so that initial memory is not quantified. Requires -fml-universal-mode quantifier"
  end
  )

module InvertExistentials = Builder.False(
  struct
    let name = "invert-existentials"
    let doc = "synthethise values for existentially quantified variables from invertible equality constraints"
  end
  )

module InvertExistentialsIncomplete = Builder.False(
  struct
    let name = "invert-existentials-incomplete"
    let doc = "Only keep synthetised terms with -fml-invert-existentials; this is incomplete"
  end
  )

module OptimLst = Builder.Integer(
  struct
    let name = "optim-lst"
    let doc = "Switch to list-like memory representation in read-over-write"
    let default = 0
  end
  )


module Flatten_memory = Builder.False(
struct
  let name = "flat-mem"
  let doc = "Remove memory reads if indexes are constant"
end
)

type solver =
  | Boolector
  | Z3
  | CVC4
  | Yices

module Solver = struct
  include Builder.Variant_choice_assoc(
  struct
  type t = solver
  let assoc_map = [
      "z3", Z3;
      "cvc4", CVC4;
      "yices", Yices;
      "boolector", Boolector;
    ]
  let default = Z3

  let name = "solver"
  let doc = " Set solver to use"
end
)

let of_piqi = function
  | `boolector -> Boolector
  | `z3 -> Z3
  | `cvc4 -> CVC4
  | `yices -> Yices
  | _ -> get () (* The current value *)

let to_piqi = function
  | Boolector -> `boolector
  | Z3 -> `z3
  | CVC4 -> `cvc4
  | Yices -> `yices


 module Timeout = Builder.Integer(
struct
  let name = "solver-timeout"
  let doc = "Timeout for solver queries"
  let default = 5
end
)


 module Options = Builder.String_option(
  struct
     let name = "solver-options"
     let doc = "Use these options to launch the solver (ignore default options)"
  end)
end


module No_stitching =
  Builder.False(
  struct
    let name = "no-stitching"
    let doc = "Do not try to stitch together continuous stores/select"
  end
  )
