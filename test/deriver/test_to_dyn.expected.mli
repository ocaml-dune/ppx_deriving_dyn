module X :
sig type t val to_dyn : t Dyn.builder type u val u_to_dyn : u Dyn.builder end
type from_module[@@deriving dyn]
include sig val from_module_to_dyn : from_module Dyn.builder end[@@ocaml.doc
                                                                  "@inline"]
[@@merlin.hide ]
type specialized_param
val specialized_param_to_dyn : specialized_param Dyn.builder
type 'a simple_record[@@deriving dyn]
include
  sig
    val simple_record_to_dyn : 'a Dyn.builder -> 'a simple_record Dyn.builder
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) simple_variant[@@deriving dyn]
include
  sig
    val simple_variant_to_dyn :
      'a Dyn.builder -> 'b Dyn.builder -> ('a, 'b) simple_variant Dyn.builder
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type long_tuple
val long_tuple_to_dyn : long_tuple Dyn.builder
type polymorphic_variant
val polymorphic_variant_to_dyn : polymorphic_variant Dyn.builder
type mrec_1
and mrec_2
and mrec_3[@@deriving dyn]
include
  sig
    val mrec_1_to_dyn : mrec_1 Dyn.builder
    val mrec_2_to_dyn : mrec_2 Dyn.builder
    val mrec_3_to_dyn : mrec_3 Dyn.builder
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
module Base_types :
sig
  type t[@@deriving dyn]
  include sig val to_dyn : t Dyn.builder end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                    ]
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
