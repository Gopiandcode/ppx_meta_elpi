[@@@warning "-33"]
open Ppxlib
module AB = Ast_builder.Default

let error_expr ~loc fmt =
  Format.kasprintf (fun s ->
      Ast_builder.Default.pexp_extension ~loc @@
      Location.error_extensionf ~loc "%s" s
    ) fmt
  


type data =
  | Cons1 of int
  | Cons2 of bool
  | Cons3 of data
[@@deriving elpi, show]

let pp_data : Format.formatter -> data -> unit = fun _fmt _data -> ()

let embed_data : data Elpi.API.Conversion.t =
  let open Elpi.API.AlgebraicData in
  declare {
    ty=Elpi.API.Conversion.TyName "";
    doc="Hello";
    pp = pp_data;
    constructors=[
      K ("Cons1", "", A (Elpi.API.BuiltInData.int, N),
         B (fun d -> Cons1 d),
         M (fun ~ok ~ko -> function Cons1 d -> ok d | _ -> ko ())
        );
    ]
  } |> Elpi.API.ContextualConversion.(!<)

let label_to_elpi_param name = "elpi__param_" ^ name

let pexp_str ~loc str = AB.pexp_constant ~loc (Pconst_string (str,loc,None))

let get_attr_doc : attribute list -> string option =
  let unwrap_payload ~loc payload =
    match[@warning "-9"] payload with
    | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant (Pconst_string (str, _, _))}, _)}] -> str
    | _ -> Location.raise_errorf ~loc "unsupported doc string format" in
  fun attributes ->
    List.find_map (function
        | { attr_name={txt="Ocaml.doc";_}; attr_payload; attr_loc=loc } ->
          Some (unwrap_payload ~loc attr_payload) 
        | _ -> None
      ) attributes

let rec lident_to_elpi_conversion : longident -> longident =
  fun lid ->
  let build_embed_name = function "t" -> "embed" | name -> "embed_" ^ name in
  match lid with
  | Lident name ->
    Lident (build_embed_name name)
  | Ldot (prefix, name) ->
    Ldot (prefix, build_embed_name name)
  | Lapply (prefix,r) ->
    Lapply (prefix, lident_to_elpi_conversion r)

(** [resolve_elpi_conversion cty] converts a core_type [cty] to a value that
    should contain it's encoding.

    e.g.

    {[
      int -> Elpi.API.BuiltInData.int
      int list -> Elpi.API.BuiltInData.list Elpi.API.BuiltInData.int
      my_type -> embed_my_type
    ]}
*)
let rec resolve_elpi_conversion : core_type -> expression =
  fun ty ->
  let loc = ty.ptyp_loc in
  match ty with
  | [%type: int] | [%type: Int.t] -> [%expr Elpi.API.BuiltInData.int ]
  | [%type: float] | [%type: Float.t] -> [%expr Elpi.API.BuiltInData.float ]
  | [%type: string] | [%type: String.t] -> [%expr Elpi.API.BuiltInData.string ]
  | [%type: [%t? t] list] | [%type: [%t? t] List.t] ->
    [%expr Elpi.API.BuiltInData.list [%e resolve_elpi_conversion t] ]
  | {ptyp_desc=Ptyp_constr (ty_name, args); _} ->
    let embed_fn_name = lident_to_elpi_conversion ty_name.txt in
    let embed_fn = (AB.pexp_ident ~loc {ty_name with txt=embed_fn_name}) in
    let args = List.map resolve_elpi_conversion args in
    begin match args with
      | [] -> embed_fn
      | args ->
        AB.pexp_apply ~loc embed_fn
          (List.map (fun arg -> Nolabel, arg) args)
    end
  | {ptyp_desc=Ptyp_tuple _; _} ->
    Location.raise_errorf ~loc "support for tuples not implemented"
  | {ptyp_desc=Ptyp_alias (_cty,_name); _} ->
    Location.raise_errorf ~loc "support for aliases not implemented"
  | {ptyp_desc=Ptyp_poly (_cty,_name); _} ->
    Location.raise_errorf ~loc "support for polymorphic quantification not implemented"
  | _ ->
    error_expr ~loc "unsupported type %a" Pprintast.core_type ty

(** [build_elpi_arg_repr ~loc ctys] converts a list of types to a value of type {Elpi.API.AlgebraicData.constructor_arguments}

    {[
      [int; string] ==> A (Elpi.API.BuiltInData.int, A (Elpi.API.BuiltInData.string, N))
    ]}
*)
let build_elpi_arg_repr ~loc : core_type list -> expression =
  fun ctys ->
  List.fold_right (fun cty rest ->
      let loc = cty.ptyp_loc in
      let cty_conversion = resolve_elpi_conversion cty in
      [%expr Elpi.API.AlgebraicData.A ([%e cty_conversion], [%e rest])]
    ) ctys [%expr Elpi.API.AlgebraicData.N ]

let embed_constructor: constructor_declaration -> _ = fun cdecl ->
  let loc = cdecl.pcd_loc in
  let constructor_name = pexp_str ~loc cdecl.pcd_name.txt in
  let constructor_doc =
    get_attr_doc cdecl.pcd_attributes
    |> Option.value ~default:""
    |> pexp_str ~loc in
  let () =
    match cdecl.pcd_vars with
    | [] -> ()
    | _ -> Location.raise_errorf ~loc "quantified variables not supported" in

  let constructor_arg_repr =
    (match cdecl.pcd_args with
    | Pcstr_tuple tys -> tys
    | Pcstr_record fields ->
      (List.map (fun ldcl -> ldcl.pld_type) fields))
    |> build_elpi_arg_repr ~loc in

  let constructor_builder =
    build_elpi_builder cdecl in

  let constructor_builder =
    build_elpi_matcher cdecl in

  [%expr
    Elpi.API.AlgebraicData.K (
      [%e constructor_name ],
      [%e constructor_doc],
      [%e constructor_arg_repr ],
      [%e constructor_builder ],
      [%e constructor_matcher ]
    ) ]


let embed_tydecl : type_declaration -> unit = fun tydecl ->
  let loc = tydecl.ptype_loc in
  let name = tydecl.ptype_name.txt in
  let params = tydecl.ptype_params |> List.map (function
      | ({ ptyp_desc=ity; ptyp_loc=loc; _ }, (var, inj)) ->
        let ity = match ity with Ptyp_var label -> label | _ -> Location.raise_errorf ~loc "invalid type param to ADT" in
        let () = match var, inj with (NoVariance, NoInjectivity) -> () | _ -> Location.raise_errorf ~loc "variance and injectivity on type parameters not supported"  in
        label_to_elpi_param ity
    ) in
  let cstr = match tydecl.ptype_kind with
    | Ptype_variant cstrs -> Location.raise_errorf ~loc "todo"
    | Ptype_record fields -> Location.raise_errorf ~loc "todo"
    | Ptype_abstract -> Location.raise_errorf ~loc "todo"
    | _ -> Location.raise_errorf ~loc "unsupported type" in
  ()

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

