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

module Exhaustive =
struct
  (*
    Sometimes we can detect that a pattern p is 'exhaustive' -- i.e. that
    'function p -> () | _ -> ()' will cause a redundancy warning.  For
    example, variables are exhaustive, as are tuples of exhaustive patterns.

    In other cases we can detect that a pattern p is 'inexhaustive' --
    i.e. that 'function p -> () | _ -> ()' will not cause a redundancy
    warning.  For example, integer constants are inexhaustive, as are array
    patterns.

    In a third family of cases we cannot detect syntactically whether a
    pattern p is exhaustive or inexhaustive because we do not have access to
    enough information.  For example, whether a pattern constisting of a
    constructor C is exhaustive or inexhaustive depends on whether it is the
    only constructor for the datatype, and whether a polymorphic variant
    pattern with a type ascription (`C:t) is exhaustive depends on the
    definition of the type t.

    [Technical note: we base the definition of 'exhaustive' on redundancy
     warnings rather than exhaustiveness warnings to give the right behaviour
     for polymorphic variants.  Note that

        function `A -> ()

     does not cause an exhaustiveness warning (and so `A could be considered
     exhaustive), but

       function `A -> () | _ -> ()

     does not cause a redundancy warning (and so `A could be considered
     inexhaustive).  However, the input to the first function has type [< `A]
     rather than the more liberal type [> `A].  We want the more liberal type,
     which allows us to accept a  larger class of programs.]
  *)

  type t = Exhaustive | Inexhaustive | PossiblyExhaustive

  let (&&) l r = match l, r with
    | Exhaustive, e | e, Exhaustive -> e
    | Inexhaustive, e | e, Inexhaustive -> Inexhaustive
    | PossiblyExhaustive, PossiblyExhaustive -> PossiblyExhaustive

  let all p = List.fold_left (fun e x -> e && p x) PossiblyExhaustive

  let rec is_exhaustive : pattern -> t =
    fun {ppat_desc} -> match ppat_desc with
      | Ppat_any
      | Ppat_var _
      | Ppat_unpack _ -> Exhaustive
      | Ppat_tuple ps -> all is_exhaustive ps
      | Ppat_alias (p, _)
      | Ppat_lazy p -> is_exhaustive p
      (* We can't tell whether (`A:t) is exhaustive without resolving t *)
      | Ppat_constraint (p, _) -> PossiblyExhaustive
      | Ppat_array _
      (* 'Constant' means integer, string or character constant. *)
      | Ppat_constant _ -> Inexhaustive
      | Ppat_type _
      | Ppat_variant _
      | Ppat_construct _ -> PossiblyExhaustive
      (* 'Or'-patterns are too much work for the moment *)
      | Ppat_or _ -> PossiblyExhaustive
      | Ppat_record (fields, _) -> all (fun (_,p) -> is_exhaustive p) fields
end

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
          let fail_code = E.apply_nolabs (E.lid "fail")
            [E.constant (Const_string ("Pattern-match failure in perform-block"))]
          in
          Exhaustive.(match is_exhaustive patt with
            | Exhaustive ->
            (* We've determined that pattern-matching against p cannot fail.
               The desugaring is
               
               p <-- e1; e2    ~>    bind e1 (fun p -> e2)
            *)
              E.apply_nolabs (E.lid "bind")
                [ this # expr rhs;
                  E.function_ "" None [ patt, this # expr next ] ]
            | Inexhaustive ->
            (* We've determined that pattern-matching against p can fail.
               The desugaring is
               
               p <-- e1; e2    ~>    bind e1 (function p -> e2
               | _ -> fail "...")
            *)
              E.apply_nolabs (E.lid "bind")
                [ this # expr rhs;
                  E.function_ "" None
                    [ patt, this # expr next;
                      P.var (Location.mkloc "_" Location.none),
                      fail_code]]
            | PossiblyExhaustive ->
            (* We cannot determine whether pattern-matching against p can fail.
               The desugaring is
               
               p <-- e1; e2    ~>    bind e1 (function p when true -> e2
               | _ -> fail "...")

               The 'when true' guard avoids the redundancy warning that would
               otherwise be generated if the OCaml implementation discovered
               that matching against p could not fail.
            *)
              E.apply_nolabs (E.lid "bind")
                [ this # expr rhs;
                  E.function_ "" None
                    [ patt, (E.when_
                               ~loc:Location.none
                               (E.construct ~loc:Location.none 
                                  (Location.mkloc (Longident.parse "true") Location.none)
                                  None false)
                               (this # expr next));
                      P.var (Location.mkloc "_" Location.none),
                      fail_code]]
          )

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
