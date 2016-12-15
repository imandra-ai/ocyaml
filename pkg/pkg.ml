#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "ocb-stubblr.topkg"

open Topkg

let build =
  Pkg.build ~cmd:Ocb_stubblr_topkg.cmd ()

let opams =
  [ Pkg.opam_file ~lint_deps_excluding:(Some ["ocb-stubblr"]) "opam" ]


let () =
  Pkg.describe ~build ~opams "ocyaml" @@ fun c ->
  Ok [ Pkg.clib "src/libocyaml_stubs.clib"
     ; Pkg.mllib "src/ocyaml.mllib"
     ; Pkg.test ~dir:"src" "test/main"
     ]
