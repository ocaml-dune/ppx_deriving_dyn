open Ppxlib

module To_dyn : sig
  (** Attribute to overwrite the default [to_dyn] converter derived for a given
      type.
      The converter to use instead is embedded in the payload as a function,
      potentially anonymous. *)

  (** Return the user provided converter for the given type, if any. *)
  val from_core_type : core_type -> expression option

  (** Return the user provided converter for the given record label, if any. *)
  val from_label_declaration : label_declaration -> expression option
end
