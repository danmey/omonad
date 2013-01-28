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

exception Pattern_translation_failure of Location.t

let map_opt f = function
    None -> None
  | Some e -> Some (f e)

let rec patt_of_expr : expression -> pattern = 
  fun {pexp_desc; pexp_loc} ->
    let desc = match pexp_desc with
      | Pexp_ident {txt = Lident txt; loc} -> Ppat_var { txt; loc }
      | Pexp_constant c -> Ppat_constant c
      | Pexp_tuple es -> Ppat_tuple (List.map patt_of_expr es)
      | Pexp_record (fields, basis) ->
        Ppat_record (List.map (fun (f, e) -> f, patt_of_expr e) fields, Open)
      | Pexp_construct (ident, e_opt, flag) ->
        Ppat_construct (ident, map_opt patt_of_expr e_opt, flag)
      | Pexp_variant (label, e_opt) ->
        Ppat_variant (label, map_opt patt_of_expr e_opt)
      | Pexp_array es -> Ppat_array (List.map patt_of_expr es)
      | Pexp_constraint (e, Some t, None) ->
        Ppat_constraint (patt_of_expr e, t)
      | Pexp_lazy e -> Ppat_lazy (patt_of_expr e)
      (* There's no expression syntax corresponding to _-, as-, or- or #-
         patterns. *)
      | _ -> raise (Pattern_translation_failure pexp_loc)
    in { ppat_desc = desc; ppat_loc = pexp_loc }

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

        let patt =
          try patt_of_expr lhs 
          with Pattern_translation_failure loc ->
            Format.eprintf "%appx-monad: Invalid pattern.@." Location.print loc;
            exit !fail_exit_code
        in
          E.apply_nolabs (E.lid "bind")
            [ this # expr rhs;
              E.function_ "" None
                [ patt, this # expr next ] ]

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
