(**************************************************************************)
(*  This file is part of BINSEC.                                          *)
(*                                                                        *)
(*  Copyright (C) 2016-2020                                               *)
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

open Unix

module ChanTbl = Hashtbl.Make(struct
    type t = out_channel
    let equal = (=)
    let hash = Hashtbl.hash
  end)

(** fd new now points to old *)
let replace old new_ =
  dup2 ~cloexec:false old new_;
  try
    close old
  with Unix_error _ -> ()
;;

(* stdin -> pid, stdout, stderr *)
let open_processes = ChanTbl.create 7;;

let open_process_and_do args inchild =
  let inr, inw = pipe ~cloexec:true () in
  let outr, outw = try
      pipe ~cloexec:true ()
    with e ->
      close inr;
      close inw;
      raise e
  in
  let errr, errw = try
      pipe ~cloexec:true ()
    with e ->
      close inr;
      close inw;
      close outr;
      close outw;
      raise e
  in
  let inc = out_channel_of_descr inw in
  let outc = in_channel_of_descr outr in
  let errc = in_channel_of_descr errr in
  begin
    try
      match fork() with
      | 0 -> begin
        (* in child *)
        replace inr Unix.stdin;
        replace outw Unix.stdout;
        replace errw Unix.stderr;
        inchild ();
        execvp args.(0) args
      end
      | pid -> begin
        ChanTbl.add open_processes inc (pid, outc, errc)
      end
    with e ->
      close inr;
      close inw;
      close outr;
      close outw;
      close errr;
      close errw;
      raise e
  end;
  close inr;
  close outw;
  close errw;
  (inc, outc, errc)

let rec waitpid_non_intr pid =
  try waitpid [] pid
  with Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let please: 'a. ('a -> unit) -> 'a -> unit = fun f x ->
  try f x with Sys_error _ | Unix_error _ -> ()

let close_process (inc, outc, errc) =
  let pid, outc', errc' = ChanTbl.find open_processes inc in
  assert (outc = outc');
  assert (errc = errc');
  please close_out inc;
  please close_in outc;
  please close_in errc;
  ChanTbl.remove open_processes inc;
  waitpid_non_intr pid |> snd

let pr_set_pdeathsig =
    match Core.Linux_ext.pr_set_pdeathsig with
    | Core.Result.Ok f -> f
    | Core.Result.Error e -> Core.Error.raise e

let open_process args =
  let inchild () =
    pr_set_pdeathsig Core.Signal.kill;
    if getppid() = 1 then
      kill (getpid()) Sys.sigkill;
  in
  open_process_and_do args inchild


