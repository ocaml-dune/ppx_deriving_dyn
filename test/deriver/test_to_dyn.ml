module X = struct
  type t = int

  let to_dyn = Dyn.int

  type u = string

  let u_to_dyn = Dyn.string
end

type from_module = X.t
[@@deriving dyn]

type specialized_param = int list
[@@deriving dyn]

type 'a simple_record =
  { int_field : int
  ; string_field : string
  ; a_option_field : 'a option
  }
[@@deriving dyn]

type ('a, 'b) simple_variant =
  | First
  | Second of 'a
  | Third of int * 'b
  | Fourth of {left : int; right : string}
[@@deriving dyn][@@ocaml.warning "-37"]

type long_tuple = int * string * bool * float
[@@deriving dyn]

type polymorphic_variant =
  [ `A
  | `B of int
  | `C of int * string ]
[@@deriving dyn]

module Base_types = struct
  type t = int
  [@@deriving dyn]
  type t1 = unit
  [@@deriving dyn]
  type t2 = char
  [@@deriving dyn]
  type t3 = string
  [@@deriving dyn]
  type t4 = int32
  [@@deriving dyn]
  type t5 = int64
  [@@deriving dyn]
  type t6 = nativeint
  [@@deriving dyn]
  type t7 = float
  [@@deriving dyn]
  type t8 = bool
  [@@deriving dyn]
  type 'a t9 = 'a list
  [@@deriving dyn]
  type 'a t10 = 'a array
  [@@deriving dyn]
  type 'a t11 = 'a option
  [@@deriving dyn]
  type t12 = int * string
  [@@deriving dyn]
  type t13 = int * string * bool
  [@@deriving dyn]
end
