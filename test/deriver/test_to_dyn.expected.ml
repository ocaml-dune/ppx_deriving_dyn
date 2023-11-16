module X =
  struct
    type t = int
    let to_dyn = Dyn.int
    type u = string
    let u_to_dyn = Dyn.string
  end
type from_module = X.t[@@deriving dyn]
include struct let from_module_to_dyn from_module = X.to_dyn from_module end
[@@ocaml.doc "@inline"][@@merlin.hide ]
type specialized_param = int list[@@deriving dyn]
include
  struct
    let specialized_param_to_dyn specialized_param =
      Dyn.list Dyn.int specialized_param
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type 'a simple_record =
  {
  int_field: int ;
  string_field: string ;
  a_option_field: 'a option }[@@deriving dyn]
include
  struct
    let simple_record_to_dyn a_to_dyn
      { int_field; string_field; a_option_field } =
      Dyn.record
        [("int_field", (Dyn.int int_field));
        ("string_field", (Dyn.string string_field));
        ("a_option_field", (Dyn.option a_to_dyn a_option_field))]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type ('a, 'b) simple_variant =
  | First 
  | Second of 'a 
  | Third of int * 'b 
  | Fourth of {
  left: int ;
  right: string } [@@deriving dyn][@@ocaml.warning "-37"]
include
  struct
    let simple_variant_to_dyn a_to_dyn b_to_dyn =
      function
      | First -> Dyn.variant "First" []
      | Second second -> Dyn.variant "Second" [a_to_dyn second]
      | Third (x0, x1) -> Dyn.variant "Third" [Dyn.int x0; b_to_dyn x1]
      | Fourth { left; right } ->
          Dyn.variant "Fourth"
            [Dyn.record
               [("left", (Dyn.int left)); ("right", (Dyn.string right))]]
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type long_tuple = (int * string * bool * float)[@@deriving dyn]
include
  struct
    let long_tuple_to_dyn long_tuple =
      (fun (x0, x1, x2, x3) ->
         Dyn.Tuple [Dyn.int x0; Dyn.string x1; Dyn.bool x2; Dyn.float x3])
        long_tuple
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
type polymorphic_variant = [ `A  | `B of int  | `C of (int * string) ]
[@@deriving dyn]
include
  struct
    let polymorphic_variant_to_dyn polymorphic_variant =
      (function
       | `A -> Dyn.variant "A" []
       | `B b -> Dyn.variant "B" [Dyn.int b]
       | `C (x0, x1) -> Dyn.variant "C" [Dyn.int x0; Dyn.string x1])
        polymorphic_variant
  end[@@ocaml.doc "@inline"][@@merlin.hide ]
module Base_types =
  struct
    type t = int[@@deriving dyn]
    include struct let to_dyn t = Dyn.int t end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type t1 = unit[@@deriving dyn]
    include struct let t1_to_dyn t1 = Dyn.unit t1 end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type t2 = char[@@deriving dyn]
    include struct let t2_to_dyn t2 = Dyn.char t2 end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type t3 = string[@@deriving dyn]
    include struct let t3_to_dyn t3 = Dyn.string t3 end[@@ocaml.doc
                                                         "@inline"][@@merlin.hide
                                                                    ]
    type t4 = int32[@@deriving dyn]
    include struct let t4_to_dyn t4 = Dyn.int32 t4 end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type t5 = int64[@@deriving dyn]
    include struct let t5_to_dyn t5 = Dyn.int64 t5 end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type t6 = nativeint[@@deriving dyn]
    include struct let t6_to_dyn t6 = Dyn.nativeint t6 end[@@ocaml.doc
                                                            "@inline"]
    [@@merlin.hide ]
    type t7 = float[@@deriving dyn]
    include struct let t7_to_dyn t7 = Dyn.float t7 end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type t8 = bool[@@deriving dyn]
    include struct let t8_to_dyn t8 = Dyn.bool t8 end[@@ocaml.doc "@inline"]
    [@@merlin.hide ]
    type 'a t9 = 'a list[@@deriving dyn]
    include struct let t9_to_dyn a_to_dyn t9 = Dyn.list a_to_dyn t9 end
    [@@ocaml.doc "@inline"][@@merlin.hide ]
    type 'a t10 = 'a array[@@deriving dyn]
    include struct let t10_to_dyn a_to_dyn t10 = Dyn.array a_to_dyn t10 end
    [@@ocaml.doc "@inline"][@@merlin.hide ]
    type 'a t11 = 'a option[@@deriving dyn]
    include struct let t11_to_dyn a_to_dyn t11 = Dyn.option a_to_dyn t11 end
    [@@ocaml.doc "@inline"][@@merlin.hide ]
    type t12 = (int * string)[@@deriving dyn]
    include struct let t12_to_dyn t12 = Dyn.pair Dyn.int Dyn.string t12 end
    [@@ocaml.doc "@inline"][@@merlin.hide ]
    type t13 = (int * string * bool)[@@deriving dyn]
    include
      struct
        let t13_to_dyn t13 = Dyn.triple Dyn.int Dyn.string Dyn.bool t13
      end[@@ocaml.doc "@inline"][@@merlin.hide ]
  end
