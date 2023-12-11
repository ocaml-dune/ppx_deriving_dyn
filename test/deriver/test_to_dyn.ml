module X = struct
  type t = int

  let to_dyn = Dyn.int

  type u = string

  let u_to_dyn = Dyn.string
end

type from_module = X.t [@@deriving dyn]
type specialized_param = int list [@@deriving dyn]

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
  | Fourth of
      { left : int
      ; right : string
      }
[@@deriving dyn] [@@ocaml.warning "-37"]

type long_tuple = int * string * bool * float [@@deriving dyn]

type polymorphic_variant =
  [ `A
  | `B of int
  | `C of int * string
  ]
[@@deriving dyn]

type recursive =
  | Leaf
  | Node of recursive * recursive
[@@deriving dyn] [@@ocaml.warning "-37"]

type mrec_1 =
  | A of int
  | B of mrec_2
  | C of mrec_3
[@@ocaml.warning "-37"]

and mrec_2 =
  | D of mrec_1
  | E of string
  | F of mrec_3
[@@ocaml.warning "-37"]

and mrec_3 =
  | G of mrec_1
  | H of mrec_2
  | I of bool
[@@deriving dyn] [@@ocaml.warning "-37"]

module Base_types = struct
  type t = int [@@deriving dyn]
  type t1 = unit [@@deriving dyn]
  type t2 = char [@@deriving dyn]
  type t3 = string [@@deriving dyn]
  type t4 = int32 [@@deriving dyn]
  type t5 = int64 [@@deriving dyn]
  type t6 = nativeint [@@deriving dyn]
  type t7 = float [@@deriving dyn]
  type t8 = bool [@@deriving dyn]
  type 'a t9 = 'a list [@@deriving dyn]
  type 'a t10 = 'a array [@@deriving dyn]
  type 'a t11 = 'a option [@@deriving dyn]
  type ('a, 'b) t12 = ('a, 'b) result [@@deriving dyn]
end

module To_dyn_attr = struct
  type t = (int[@ppx_deriving_dyn.to_dyn Dyn.opaque]) [@@deriving dyn]

  type t1 = int * ((int * string)[@to_dyn fun (_, s) -> Dyn.string s]) * bool
  [@@deriving dyn]

  type t2 =
    { a : int
    ; b : string option
         [@to_dyn
           function
           | Some s -> Dyn.string s
           | None -> Dyn.string "null"]
    }
  [@@deriving dyn]

  type t3 =
    | A
    | B of
        { a : int
        ; b : bool [@to_dyn fun x -> Dyn.string (string_of_bool x)]
        }
  [@@deriving dyn] [@@ocaml.warning "-37"]
end

module Ignore_attr = struct
  type t = int * (string[@ignore]) * float [@@deriving dyn]
  type t1 = string * (int[@ignore]) [@@deriving dyn]

  type t2 =
    { field_a : int
    ; field_b : string [@to_dyn.ignore]
    ; field_c : bool
    }
  [@@deriving dyn]

  type t3 =
    | A of (int[@ppx_deriving_dyn.to_dyn.ignore])
    | B of int * (string[@ignore])
    | All_ignored of (int[@ignore]) * (string[@ignore])
    | Record_arg of
        { field_a : int
        ; field_b : string [@ignore]
        ; field_c : bool
        }
  [@@deriving dyn] [@@ocaml.warning "-37"]
end
