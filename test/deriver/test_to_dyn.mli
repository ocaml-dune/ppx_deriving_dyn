module X : sig
  type t

  val to_dyn : t Dyn.builder

  type u

  val u_to_dyn : u Dyn.builder
end

type from_module
[@@deriving dyn]

type specialized_param
val specialized_param_to_dyn : specialized_param Dyn.builder

type 'a simple_record
[@@deriving dyn]

type ('a, 'b) simple_variant
[@@deriving dyn]

type long_tuple
val long_tuple_to_dyn : long_tuple Dyn.builder

type polymorphic_variant
val polymorphic_variant_to_dyn : polymorphic_variant Dyn.builder

module Base_types : sig
  type t
  [@@deriving dyn]
  type t1
  val t1_to_dyn : t1 Dyn.builder
  type t2
  val t2_to_dyn : t2 Dyn.builder
  type t3
  val t3_to_dyn : t3 Dyn.builder
  type t4
  val t4_to_dyn : t4 Dyn.builder
  type t5
  val t5_to_dyn : t5 Dyn.builder
  type t6
  val t6_to_dyn : t6 Dyn.builder
  type t7
  val t7_to_dyn : t7 Dyn.builder
  type t8
  val t8_to_dyn : t8 Dyn.builder
  type 'a t9
  val t9_to_dyn : 'a Dyn.builder -> 'a t9 Dyn.builder
  type 'a t10
  val t10_to_dyn : 'a Dyn.builder -> 'a t10 Dyn.builder
  type 'a t11
  val t11_to_dyn : 'a Dyn.builder -> 'a t11 Dyn.builder
end
