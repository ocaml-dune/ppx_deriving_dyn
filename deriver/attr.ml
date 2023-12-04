open Ppxlib

let get_attr ~wrap_err_ext attr ast =
  match Attribute.get_res attr ast with
  | Ok opt -> opt
  | Error (err, _) ->
    let loc = Location.Error.get_location err in
    Some (wrap_err_ext ~loc (Location.Error.to_extension err))
;;

module To_dyn = struct
  let name = "ppx_deriving_dyn.to_dyn"

  let payload_pattern =
    let open Ast_pattern in
    let ident_expr = as__ (pexp_ident drop) in
    let apply_expr = as__ (pexp_apply drop drop) in
    let fun_expr = as__ (pexp_fun drop drop drop drop) in
    let function_expr = as__ (pexp_function drop) in
    single_expr_payload (ident_expr ||| apply_expr ||| fun_expr ||| function_expr)
  ;;

  let get_attr attr ast =
    get_attr ~wrap_err_ext:Ast_builder.Default.pexp_extension attr ast
  ;;

  let core_type_attr =
    Attribute.declare name Attribute.Context.core_type payload_pattern (fun expr -> expr)
  ;;

  let from_core_type core_type = get_attr core_type_attr core_type

  let label_decl_attr =
    Attribute.declare
      name
      Attribute.Context.label_declaration
      payload_pattern
      (fun expr -> expr)
  ;;

  let from_label_declaration label_declaration =
    get_attr label_decl_attr label_declaration
  ;;
end
