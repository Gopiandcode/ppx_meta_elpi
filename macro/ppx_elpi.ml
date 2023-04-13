[@@@warning "-33"]
open Ppxlib


type elpi_type_decl =
  | Opaque of expression        (* payload of an expression of type 'a Elpi.API.OpaqueData.declaration *)
  | Algebraic                   (* a plain algebraic data type with no constructors *)


let expand_str : loc:location ->
  path:label ->
  rec_flag * type_declaration list ->
  expression option ->
  expression option -> expression option -> Ppxlib.structure =
  fun ~loc ~path (rec_, tydecls) decl mapper ctx ->
  assert false
[@@warning "-27"]


let expand_sig: loc:location ->
  path:label ->
  rec_flag * type_declaration list ->
  module_expr option -> Ppxlib.signature =
  fun ~loc:_ ~path:_ (_, _) _ ->
  assert false

let str_type_decl_generator =
  let arguments = Deriving.Args.(empty +> arg "declaration" __ +> arg "mapper" __ +> arg "context" __) in
  Deriving.Generator.make arguments expand_str

let sig_type_decl_generator =
  let arguments = Deriving.Args.(empty +> arg "index" (pexp_pack __) ) in
  Deriving.Generator.make arguments expand_sig

let deriving_elpi =
  Deriving.add ~str_type_decl:str_type_decl_generator ~sig_type_decl:sig_type_decl_generator "elpi"

(* let conversion_of_expansion : loc:location -> path:label -> core_type -> expression = *)
(*   fun ~loc:_ ~path:_ _cty -> assert false *)

(* let conversion_extension = *)
(*   Extension.declare "elpi" *)
(*     Extension.Context.expression *)
(*     Ast_pattern.(ptyp __) *)
(*     conversion_of_expansion *)

(* let () = *)
(*   Driver.register_transformation ~extensions:[conversion_extension] *)
(*     "elpi.conversion" *)

