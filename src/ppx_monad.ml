(*---------------------------------------------------------------------------
  Copyright (c) 2012 Wojciech Meyer
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of Wojciech Meyer nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)

open Asttypes
open Ast_mapper
open Location
open Parsetree
open Longident

let fail_exit_code = ref 0

let mapper =
  object(this)

    inherit Ast_mapper.mapper as super

    val in_monad = false

    method! expr e =
      match e.pexp_desc with
      | Pexp_apply
          ( { pexp_desc = Pexp_ident {txt = Lident "perform"} },
            [_,body] )
        -> {< in_monad = true>} # expr body

      | Pexp_sequence
          ( { pexp_desc = Pexp_apply
              ( { pexp_desc = Pexp_ident { txt = Lident "<--" } },
                [_,lhs; _,rhs] ) },
            next)
          when in_monad ->

        begin match lhs.pexp_desc with
        | Pexp_ident { txt = Lident nm } ->
          E.apply_nolabs (E.lid "bind")
            [ this # expr rhs;
              E.function_ "" None
                [ P.var (Location.mkloc nm Location.none), this # expr next ] ]

        | _ ->
          Format.eprintf "%appx-monad: Expected variable binding.@." Location.print lhs.pexp_loc;
          exit !fail_exit_code
        end

      | Pexp_sequence (first, second) when in_monad ->
        E.apply_nolabs (E.lid "bind")
          [ this # expr first;
            E.function_ "" None
              [P.var (Location.mkloc "_" Location.none), this # expr second ] ]

      | Pexp_apply
          ( { pexp_loc;
              pexp_desc = Pexp_ident {txt = Lident "<--"} },
            [_,lhs;l,rhs] ) when in_monad ->
        Format.eprintf "%appx-monad: Monadic computation must be terminated with return.@." Location.print pexp_loc;
        exit !fail_exit_code

      | _ -> super # expr e

  end

(* let spec = Arg.align ["-fail-exit-code", Arg.Set_int fail_exit_code, " Set failing code for this ppx, useful for testing"] *)

let () =
  (* Arg.parse spec (fun _ -> ()) "ppx-monad: Monadic code in OCaml using ppx"; *)
  Ast_mapper.main mapper
