open Ppxlib

let error_extensionf ~loc fmt =
  Location.error_extensionf ~loc ("ppx_deriving_dyn: " ^^ fmt)
;;

let unsupported_mutually_rec_type_decl ~loc =
  error_extensionf ~loc "Mutually recursive type declarations are not supported."
;;

let unsupported_type_param ~loc = error_extensionf ~loc "unsupported type parameter"
let unsupported_longident ~loc = error_extensionf ~loc "unsupported longident"
let unsupported_type ~loc = error_extensionf ~loc "cannot derive to_dyn for this type"
let unsupported_gadt ~loc = error_extensionf ~loc "cannot derive to_dyn for GADTs"

let unsupported_rinherit ~loc =
  error_extensionf ~loc "cannot derive to_dyn for inherited polymorphic variant types."
;;

let unsupported_conjunctive_tag_arg ~loc =
  error_extensionf
    ~loc
    "cannot derive to_dyn for polymorphic variant tag with conjunctive type argument."
;;

let cannot_ignore_all_elements ~loc =
  error_extensionf ~loc "you cannot mark all elements of a tuple or record with [@ignore]"
;;
