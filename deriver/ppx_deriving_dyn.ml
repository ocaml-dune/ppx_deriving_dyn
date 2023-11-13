open Ppxlib

module Error = struct
  let unsupported_mutually_rec_type_decl ~loc =
    Location.error_extensionf
      ~loc
      "ppx_deriving_dyn: Mutually recursive type declarations are not supported."
  ;;

  let unsupported_type_param ~loc =
    Location.error_extensionf ~loc "ppx_deriving_dyn: unsupported type parameter"
  ;;

  let unsupported_longident ~loc =
    Location.error_extensionf ~loc "ppx_deriving_dyn: unsupported longident"
  ;;

  let unsupported_type ~loc =
    Location.error_extensionf
      ~loc
      "ppx_deriving_dyn: cannot derive to_dyn for this type"
  ;;

  let unsupported_gadt ~loc =
    Location.error_extensionf ~loc "ppx_deriving_dyn: cannot derive to_dyn for GADTs"
  ;;

  let unsupported_rinherit ~loc =
    Location.error_extensionf
      ~loc
      "ppx_deriving_dyn: cannot derive to_dyn for inherited polymorphic variant types."
  ;;

  let unsupported_conjunctive_tag_arg ~loc =
    Location.error_extensionf
      ~loc
      "ppx_deriving_dyn: cannot derive to_dyn for polymorphic variant tag with \
       conjunctive type argument."
  ;;
end

let to_dyn_name = function
  | "t" -> "to_dyn"
  | s -> s ^ "_to_dyn"
;;

module Impl = struct
  let to_dyn_ident ~loc { txt = lident; loc = err_loc } =
    match lident with
    | Lident ("unit" as s)
    | Lident ("char" as s)
    | Lident ("string" as s)
    | Lident ("int" as s)
    | Lident ("int32" as s)
    | Lident ("int64" as s)
    | Lident ("nativeint" as s)
    | Lident ("float" as s)
    | Lident ("bool" as s)
    | Lident ("list" as s)
    | Lident ("array" as s)
    | Lident ("option" as s) ->
      Ast_builder.Default.pexp_ident ~loc { txt = Ldot (Lident "Dyn", s); loc }
    | Lident s -> Ast_builder.Default.evar ~loc (to_dyn_name s)
    | Ldot (path, s) ->
      Ast_builder.Default.pexp_ident ~loc { txt = Ldot (path, to_dyn_name s); loc }
    | Lapply _ ->
      Ast_builder.Default.pexp_extension
        ~loc:err_loc
        (Error.unsupported_longident ~loc:err_loc)
  ;;

  let destruct_tuple core_types =
    List.mapi (fun i t -> Printf.sprintf "x%i" i, t) core_types
  ;;

  let tuple_pattern ~loc destructed =
    let vars =
      List.map (fun (name, _) -> Ast_builder.Default.pvar ~loc name) destructed
    in
    Ast_builder.Default.ppat_tuple ~loc vars
  ;;

  let fun_or_applied ~loc ~f final_arg =
    match final_arg with
    | None -> f
    | Some expr -> Ast_builder.Default.pexp_apply ~loc f [ Nolabel, expr ]
  ;;

  let rec core_type_to_dyn ~loc ~core_type expr_opt =
    match core_type with
    | { ptyp_desc = Ptyp_constr (lident_loc, type_params); _ } ->
      let pexp_ident = to_dyn_ident ~loc lident_loc in
      let param_args = type_params_to_arg_list ~loc type_params in
      let args =
        match expr_opt with
        | None -> param_args
        | Some expr -> param_args @ [ Nolabel, expr ]
      in
      (match args with
       | [] -> pexp_ident
       | _ -> Ast_builder.Default.pexp_apply ~loc pexp_ident args)
    | { ptyp_desc = Ptyp_var s; _ } ->
      let pexp_ident = Ast_builder.Default.evar ~loc (to_dyn_name s) in
      fun_or_applied ~loc ~f:pexp_ident expr_opt
    | { ptyp_desc = Ptyp_tuple (([ _; _ ] | [ _; _; _ ]) as core_types); _ } ->
      let size = List.length core_types in
      let f = if size = 2 then [%expr Dyn.pair] else [%expr Dyn.triple] in
      let to_dyn_args =
        List.map
          (fun core_type -> Nolabel, core_type_to_dyn ~loc ~core_type None)
          core_types
      in
      let args =
        match expr_opt with
        | None -> to_dyn_args
        | Some expr -> to_dyn_args @ [ Nolabel, expr ]
      in
      Ast_builder.Default.pexp_apply ~loc f args
    | { ptyp_desc = Ptyp_tuple core_types; _ } ->
      let destructed = destruct_tuple core_types in
      let pat = tuple_pattern ~loc destructed in
      let body = [%expr Dyn.Tuple [%e tuple_to_dyn_list ~loc destructed]] in
      let to_dyn = [%expr fun [%p pat] -> [%e body]] in
      fun_or_applied ~loc ~f:to_dyn expr_opt
    | { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
      let to_dyn =
        Ast_builder.Default.pexp_function ~loc (List.map (row_field_case ~loc) row_fields)
      in
      fun_or_applied ~loc ~f:to_dyn expr_opt
    | { ptyp_loc = loc; _ } ->
      Ast_builder.Default.pexp_extension ~loc (Error.unsupported_type ~loc)

  and type_params_to_arg_list ~loc type_params =
    List.map (fun core_type -> Nolabel, core_type_to_dyn ~loc ~core_type None) type_params

  and tuple_to_dyn_list ~loc destructed_tuple =
    let to_dyn_list =
      List.map
        (fun (arg_name, core_type) ->
          let arg = Ast_builder.Default.evar ~loc arg_name in
          core_type_to_dyn ~loc ~core_type (Some arg))
        destructed_tuple
    in
    Ast_builder.Default.elist ~loc to_dyn_list

  and row_field_case ~loc { prf_desc; prf_loc; _ } =
    let pc_lhs, pc_rhs =
      match prf_desc with
      | Rtag ({ txt; _ }, _, []) ->
        let pc_lhs = Ast_builder.Default.ppat_variant ~loc txt None in
        let pc_rhs = variant_no_arg ~loc ~variant_name:txt in
        pc_lhs, pc_rhs
      | Rtag ({ txt; _ }, _, [ { ptyp_desc = Ptyp_tuple core_types; _ } ]) ->
        let destructed = destruct_tuple core_types in
        let pat = tuple_pattern ~loc destructed in
        let pc_lhs = Ast_builder.Default.ppat_variant ~loc txt (Some pat) in
        let pc_rhs = variant_x_args ~loc ~variant_name:txt destructed in
        pc_lhs, pc_rhs
      | Rtag ({ txt; _ }, _, [ core_type ]) ->
        let arg_name = String.uncapitalize_ascii txt in
        let pat = Ast_builder.Default.pvar ~loc arg_name in
        let pc_lhs = Ast_builder.Default.ppat_variant ~loc txt (Some pat) in
        let pc_rhs = variant_one_arg ~loc ~arg_name ~variant_name:txt core_type in
        pc_lhs, pc_rhs
      | Rtag (_, _, _) ->
        let pc_lhs =
          Ast_builder.Default.ppat_extension
            ~loc:prf_loc
            (Error.unsupported_conjunctive_tag_arg ~loc:prf_loc)
        in
        pc_lhs, [%expr Dyn.opaque ()]
      | Rinherit _ ->
        let pc_lhs =
          Ast_builder.Default.ppat_extension
            ~loc:prf_loc
            (Error.unsupported_rinherit ~loc:prf_loc)
        in
        pc_lhs, [%expr Dyn.opaque ()]
    in
    { pc_lhs; pc_rhs; pc_guard = None }

  and variant_no_arg ~loc ~variant_name =
    let string_lit = Ast_builder.Default.estring ~loc variant_name in
    [%expr Dyn.variant [%e string_lit] []]

  and variant_one_arg ~loc ~variant_name ~arg_name core_type =
    let string_lit = Ast_builder.Default.estring ~loc variant_name in
    let arg = Ast_builder.Default.evar ~loc arg_name in
    [%expr
      Dyn.variant [%e string_lit] [ [%e core_type_to_dyn ~loc ~core_type (Some arg)] ]]

  and variant_x_args ~loc ~variant_name destructed_tuple =
    let string_lit = Ast_builder.Default.estring ~loc variant_name in
    [%expr Dyn.variant [%e string_lit] [%e tuple_to_dyn_list ~loc destructed_tuple]]
  ;;

  let destruct_record labels =
    List.map (fun { pld_name = { txt; _ }; pld_type; _ } -> txt, pld_type) labels
  ;;

  let record_pattern ~loc destructed_record =
    let fields =
      List.map
        (fun (field_name, _) ->
          { txt = Lident field_name; loc }, Ast_builder.Default.pvar ~loc field_name)
        destructed_record
    in
    Ast_builder.Default.ppat_record ~loc fields Closed
  ;;

  let record_field_to_dyn ~loc (txt, pld_type) =
    let string_lit = Ast_builder.Default.estring ~loc txt in
    let field_var = Ast_builder.Default.evar ~loc txt in
    let expr = core_type_to_dyn ~loc ~core_type:pld_type (Some field_var) in
    Ast_builder.Default.pexp_tuple ~loc [ string_lit; expr ]
  ;;

  let record_to_dyn ~loc destructed_record =
    let fields = List.map (record_field_to_dyn ~loc) destructed_record in
    [%expr Dyn.record [%e Ast_builder.Default.elist ~loc fields]]
  ;;

  let constructor_case ~loc { pcd_name = { txt; _ }; pcd_args; pcd_res; pcd_loc; _ } =
    let longident_loc = { txt = Lident txt; loc } in
    let pc_lhs, pc_rhs =
      match pcd_res, pcd_args with
      | Some _, _ ->
        let pat =
          Ast_builder.Default.ppat_extension
            ~loc:pcd_loc
            (Error.unsupported_gadt ~loc:pcd_loc)
        in
        pat, [%expr Dyn.opaque ()]
      | None, Pcstr_record labels ->
        let string_lit = Ast_builder.Default.estring ~loc txt in
        let destructed = destruct_record labels in
        let record_pat = record_pattern ~loc destructed in
        let record_expr = record_to_dyn ~loc destructed in
        let pc_lhs =
          Ast_builder.Default.ppat_construct ~loc longident_loc (Some record_pat)
        in
        let pc_rhs = [%expr Dyn.variant [%e string_lit] [ [%e record_expr] ]] in
        pc_lhs, pc_rhs
      | None, Pcstr_tuple [] ->
        let pc_lhs = Ast_builder.Default.ppat_construct ~loc longident_loc None in
        let pc_rhs = variant_no_arg ~loc ~variant_name:txt in
        pc_lhs, pc_rhs
      | None, Pcstr_tuple [ core_type ] ->
        let arg_name = String.uncapitalize_ascii txt in
        let pat = Ast_builder.Default.pvar ~loc arg_name in
        let pc_lhs = Ast_builder.Default.ppat_construct ~loc longident_loc (Some pat) in
        let pc_rhs = variant_one_arg ~loc ~variant_name:txt ~arg_name core_type in
        pc_lhs, pc_rhs
      | None, Pcstr_tuple core_types ->
        let destructed = destruct_tuple core_types in
        let pat = tuple_pattern ~loc destructed in
        let pc_lhs = Ast_builder.Default.ppat_construct ~loc longident_loc (Some pat) in
        let pc_rhs = variant_x_args ~loc ~variant_name:txt destructed in
        pc_lhs, pc_rhs
    in
    { pc_lhs; pc_rhs; pc_guard = None }
  ;;

  let rec with_type_param_args ~loc ~ptype_params expr =
    match ptype_params with
    | [] -> expr
    | ({ ptyp_desc = Ptyp_var s; _ }, _) :: tl ->
      let pat = Ast_builder.Default.(ppat_var ~loc { txt = to_dyn_name s; loc }) in
      Ast_builder.Default.pexp_fun
        ~loc
        Nolabel
        None
        pat
        (with_type_param_args ~loc ~ptype_params:tl expr)
    | ({ ptyp_loc = loc; _ }, _) :: _ ->
      Ast_builder.Default.pexp_extension ~loc (Error.unsupported_type_param ~loc)
  ;;

  let to_dyn_fun ~loc ~rec_flag:_ ~type_declaration =
    match type_declaration with
    | { ptype_kind = Ptype_abstract; ptype_manifest = Some core_type; ptype_params; _ } ->
      let main_arg_name = type_declaration.ptype_name.txt in
      let main_arg_pat = Ast_builder.Default.pvar ~loc main_arg_name in
      let main_arg_expr = Ast_builder.Default.evar ~loc main_arg_name in
      let to_dyn_fun =
        Ast_builder.Default.pexp_fun
          ~loc
          Nolabel
          None
          main_arg_pat
          (core_type_to_dyn ~loc ~core_type (Some main_arg_expr))
      in
      with_type_param_args ~loc ~ptype_params to_dyn_fun
    | { ptype_kind = Ptype_record labels; ptype_params; _ } ->
      let destructed = destruct_record labels in
      let main_arg_pat = record_pattern ~loc destructed in
      let to_dyn_fun =
        Ast_builder.Default.pexp_fun
          ~loc
          Nolabel
          None
          main_arg_pat
          (record_to_dyn ~loc destructed)
      in
      with_type_param_args ~loc ~ptype_params to_dyn_fun
    | { ptype_kind = Ptype_variant constructors; ptype_params; _ } ->
      let to_dyn_fun =
        Ast_builder.Default.pexp_function
          ~loc
          (List.map (constructor_case ~loc) constructors)
      in
      with_type_param_args ~loc ~ptype_params to_dyn_fun
    | { ptype_loc; _ } ->
      Ast_builder.Default.pexp_extension
        ~loc:ptype_loc
        (Error.unsupported_type ~loc:ptype_loc)
  ;;

  let generate ~ctxt (rec_flag, type_declarations) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match type_declarations with
    | [] -> assert false
    | _ :: _ :: _ ->
      [ Ast_builder.Default.pstr_extension
          ~loc
          (Error.unsupported_mutually_rec_type_decl ~loc)
          []
      ]
    | [ type_declaration ] ->
      let fun_name = to_dyn_name type_declaration.ptype_name.txt in
      let pat = Ast_builder.Default.(ppat_var ~loc { txt = fun_name; loc }) in
      let expr = to_dyn_fun ~loc ~rec_flag ~type_declaration in
      let value_binding = Ast_builder.Default.value_binding ~loc ~pat ~expr in
      [ Ast_builder.Default.pstr_value ~loc Nonrecursive [ value_binding ] ]
  ;;

  let generator = Deriving.Generator.V2.make_noarg generate
end

module Intf = struct
  let dyn_builder_lident ~loc = { txt = Ldot (Lident "Dyn", "builder"); loc }

  let type_dyn_builder ~loc type_ =
    Ast_builder.Default.ptyp_constr ~loc (dyn_builder_lident ~loc) [ type_ ]
  ;;

  let dyn_builder ~loc ~type_params type_name =
    let type_lident = { txt = Lident type_name; loc } in
    let arg = Ast_builder.Default.ptyp_constr ~loc type_lident type_params in
    type_dyn_builder ~loc arg
  ;;

  let rec with_type_param_args ~loc ~type_params type_ =
    match type_params with
    | [] -> type_
    | ({ ptyp_desc = Ptyp_var _; _ } as type_var) :: tl ->
      let arg_type = type_dyn_builder ~loc type_var in
      Ast_builder.Default.ptyp_arrow
        ~loc
        Nolabel
        arg_type
        (with_type_param_args ~loc ~type_params:tl type_)
    | { ptyp_loc = loc; _ } :: _ ->
      Ast_builder.Default.ptyp_extension ~loc (Error.unsupported_type_param ~loc)
  ;;

  let to_dyn_type ~loc { ptype_name = { txt; _ }; ptype_params; _ } =
    let type_params = List.map (fun (ct, _) -> ct) ptype_params in
    let type_ = dyn_builder ~loc ~type_params txt in
    with_type_param_args ~loc ~type_params type_
  ;;

  let generate ~ctxt (_rec_flag, type_declarations) =
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
    match type_declarations with
    | [] -> assert false
    | _ :: _ :: _ ->
      [ Ast_builder.Default.psig_extension
          ~loc
          (Error.unsupported_mutually_rec_type_decl ~loc)
          []
      ]
    | [ type_declaration ] ->
      let fun_name = to_dyn_name type_declaration.ptype_name.txt in
      let name = { txt = fun_name; loc } in
      let type_ = to_dyn_type ~loc type_declaration in
      let descr = Ast_builder.Default.value_description ~loc ~name ~type_ ~prim:[] in
      [ Ast_builder.Default.psig_value ~loc descr ]
  ;;

  let generator = Deriving.Generator.V2.make_noarg generate
end

let deriver =
  Deriving.add "to_dyn" ~str_type_decl:Impl.generator ~sig_type_decl:Intf.generator
;;
