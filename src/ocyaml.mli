(* OCaml bindings for libyaml                                          *)
(* (c)Copyright Aesthetic Integration, Ltd., 2016                      *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* Released under Apache 2.0 license as described in the file LICENSE. *)
(*                                                                     *)
(* Contributors:                                                       *)
(* Konstantin kanishchev (kostya@aestheticintegration.com)             *)
(* Matt Bray (matt@aestheticintegration.com)                           *)
(*                                                                     *)

type yaml =
  | Scalar of string
  | Collection of yaml list
  | Structure of (yaml * yaml) list

val of_file : string -> yaml
val of_string : string -> yaml

val equal : yaml -> yaml -> bool
