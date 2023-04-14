[@@@warning "-33"]
open Ppxlib
module AB = Ast_builder.Default


module Utils = struct
  let error_expr ~loc fmt =
    Format.kasprintf (fun s ->
      Ast_builder.Default.pexp_extension ~loc @@
      Location.error_extensionf ~loc "%s" s
    ) fmt

  let error_str ~loc fmt =
    Format.kasprintf (fun s ->
      Ast_builder.Default.pstr_extension ~loc @@
      Location.error_extensionf ~loc "%s" s
    ) fmt

  let error_typ ~loc fmt =
    Format.kasprintf (fun s ->
      Ast_builder.Default.ptyp_extension ~loc @@
      Location.error_extensionf ~loc "%s" s
    ) fmt

  let pexp_str ~loc str =
    AB.pexp_constant ~loc (Pconst_string (str,loc,None))

  let pexp_ident ~loc txt =
    AB.pexp_ident ~loc {txt;loc}

  let pexp_ident_s ~loc txt =
    AB.pexp_ident ~loc {txt=Lident txt;loc}


  let ppat_var_s ~loc txt =
    AB.ppat_var ~loc {txt;loc}

  (** [lookup_doc_attr attrs] retrieves the corresponding doc annotation, if it exists in [attrs]. *)
  let lookup_doc_attr : attribute list -> string option =
    let unwrap_payload ~loc payload =
      match[@warning "-9"] payload with
      | PStr [ {pstr_desc=Pstr_eval ({pexp_desc=Pexp_constant (Pconst_string (str, _, _))}, _)}] -> str
      | _ -> Location.raise_errorf ~loc "unsupported doc string format" in
    fun attributes ->
      List.find_map (function
        | { attr_name={txt="ocaml.doc";_}; attr_payload; attr_loc=loc } ->
          Some (unwrap_payload ~loc attr_payload) 
        | _ -> None
      ) attributes

end

module ENaming = struct

  let label_to_param name = "elpi__param_" ^ name

  let rec tyname_to_elpi_conversion_name : longident -> longident =
    fun lid ->
    let build_embed_name = function "t" -> "embed" | name -> "embed_" ^ name in
    match lid with
    | Lident name ->
      Lident (build_embed_name name)
    | Ldot (prefix, name) ->
      Ldot (prefix, build_embed_name name)
    | Lapply (prefix,r) ->
      Lapply (prefix, tyname_to_elpi_conversion_name r)

end
