module X = struct
  type t = int

  let to_dyn = Dyn.int

  type u = string

  let u_to_dyn = Dyn.string
end

type from_module = X.t
[@@deriving to_dyn]

type specialized_param = int list
[@@deriving to_dyn]

type 'a simple_record =
  { int_field : int
  ; string_field : string
  ; a_option_field : 'a option
  }
[@@deriving to_dyn]

type ('a, 'b) simple_variant =
  | First
  | Second of 'a
  | Third of int * 'b
  | Fourth of {left : int; right : string}
[@@deriving to_dyn][@@ocaml.warning "-37"]

type long_tuple = int * string * bool * float
[@@deriving to_dyn]

type polymorphic_variant =
  [ `A
  | `B of int
  | `C of int * string ]
[@@deriving to_dyn]

module Base_types = struct
  type t = int
  [@@deriving to_dyn]
  type t1 = unit
  [@@deriving to_dyn]
  type t2 = char
  [@@deriving to_dyn]
  type t3 = string
  [@@deriving to_dyn]
  type t4 = int32
  [@@deriving to_dyn]
  type t5 = int64
  [@@deriving to_dyn]
  type t6 = nativeint
  [@@deriving to_dyn]
  type t7 = float
  [@@deriving to_dyn]
  type t8 = bool
  [@@deriving to_dyn]
  type 'a t9 = 'a list
  [@@deriving to_dyn]
  type 'a t10 = 'a array
  [@@deriving to_dyn]
  type 'a t11 = 'a option
  [@@deriving to_dyn]
  type t12 = int * string
  [@@deriving to_dyn]
  type t13 = int * string * bool
  [@@deriving to_dyn]
end
