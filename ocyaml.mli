(* OCaml bindings for libyaml                                          *)
(* (c)Copyright Aesthetic Integration, Ltd., 2016                      *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* Released under Apache 2.0 license as described in the file LICENSE. *)
(*                                                                     *)
(* Contributors:                                                       *)
(* Konstantin kanishchev (kostya@aestheticintegration.com)             *)
(*                                                                     *)

type yaml =
    Scalar of string
  | Collection of yaml list
  | Structure of (yaml * yaml) list

val load : string -> yaml
