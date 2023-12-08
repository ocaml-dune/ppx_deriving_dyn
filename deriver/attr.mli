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

module Ignore : sig
  (** Attribute to exclude record fields or tuple elements from the [Dyn.t]
      value produced by the derived converter. *)

  (** Return whether a core_type is to be ignored *)
  val core_type : core_type -> (bool, extension * Location.t) result

  (** Return whether a record label is to be ignored *)
  val label_declaration : label_declaration -> (bool, extension * Location.t) result
end
