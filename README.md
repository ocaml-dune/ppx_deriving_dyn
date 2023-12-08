# `ppx_deriving_dyn`

PPX deriver for dune's `Dyn.t` converters

## Overview

`ppx_deriving_dyn` derives basic `Dyn.t` converters from type definitions.

Attach the `[@@deriving dyn]` attribute to your type definitions to generate
the corresponding `to_dyn` function.

It can be used to derive both the function and its signature depending on the
context.

For example you can use it in your `.ml` files like this:
```ocaml
type t = int
[@@deriving dyn]
```

to generate the following:
```ocaml
let to_dyn = Dyn.int
```

or in your `.mli` files:
```ocaml
type t
[@@deriving dyn]
```

to generate the following:
```ocaml
val to_dyn : t Dyn.builder
```

## Specification

`ppx_deriving_dyn` follows the following rules:
- The converter is named `to_dyn` for types named `t` and `<type name>_to_dyn`
  for any other type name.
- It uses the builders provided by `Dyn` for the following base types:
    + `unit`
    + `char`
    + `string`
    + `int`
    + `int32`
    + `int64`
    + `nativeint`
    + `float`
    + `bool`
    + `list`
    + `array`
    + `option`
- Tuple types are converted using an inline
   `(fun (x0, x1, x2, x3) -> Dyn.Tuple [...])`
- It uses `Dyn.record` for record types
- It uses `Dyn.variant` for variant and polymorphic variant types. Inline record
  arguments are converted using `Dyn.record` as well.
- An `'a Dyn.builder` extra argument is expected for each type parameter,
  following the declaration order, e.g. for a `type ('a, 'b) t` it will generate
  `val to_dyn : 'a Dyn.builder -> 'b Dyn.builder -> ('a, 'b) t Dyn.builder` 

## Advanced Usage

### Configuration attributes

`ppx_deriving_dyn` allows you to override the default behaviour using the
following attributes.

#### `[@to_dyn ...]`

It can be used as `[@to_dyn ...]` or `[@ppx_deriving_dyn.to_dyn ...]` and allows
you to define how a specific part of a type should be converted to a `Dyn.t`
value by attaching this attribute to it and passing the function to use in the
payload.

Here are some examples on how to use it:
```ocaml
type t = float [@to_dyn special_float_to_dyn]
[@@deriving dyn]
```

```ocaml
type t = int * string * (bool [@to_dyn fun b -> Dyn.string (string_of_bool b)])
[@@deriving dyn]
```

```ocaml
type t =
  { a : int
  ; b : string option
       [@to_dyn
         function
         | Some s -> Dyn.string s
         | None -> Dyn.string "null"]
  }
[@@deriving dyn]
```

The attribute can be attached to a core type or record field and accepts
function identifier, partial function applications and anonymous functions as
payload.

#### `[@ignore]`

It can be used as `[@ignore]`, `[@to_dyn.ignore]` or
`[@ppx_deriving_dyn.to_dyn.ignore]` and allows you to exclude part of a tuple,
record or variant arguments from the output of derived `to_dyn` function.

For example, with the following type definition:
```ocaml
type t = int * (string[@ignore]) * bool
[@@deriving dyn]
```

The generated dyn converter will ignore the string element of the tuple and
return a `Dyn.t` value describing an `int * bool` pair. To clarify this a bit,
the generated code will look roughly like this:
```ocaml
let to_dyn (x0, _, x2) = Dyn.Tuple [Dyn.int x0; Dyn.bool x2]
```

It can also be used to similarly ignore fields of a record:
```ocaml
type t =
  { field_a : int
  ; field_b : string [@ignore]
  ; field_c : bool 
  }
[@@deriving dyn]
```

which will produce the following dyn converter:
```ocaml
let to_dyn {field_a; field_b = _; field_c} =
  Dyn.record
    [("field_a", (Dyn.int field_a)); ("field_c", (Dyn.bool field_c))]
```

Finally, you can use it on sum type constructor or polymorphic variant's
arguments:
```ocaml
type t =
  | A of int * (string [@ignore])
[@@deriving dyn]
```

which will produce the following dyn converter:
```ocaml
let to_dyn = function
  | A (x0, _) -> Dyn.variant "A" [Dyn.int x0]
```

Note that you cannot ignore all elements of a tuple or all fields of a record
but you can ignore all arguments of a constructor, in which case the `to_dyn`
function will treat it as if it had no argument, e.g.:
```ocaml
type t =
  | A of (int[@ignore])
[@@deriving dyn]
```

will derive:
```ocaml
let to_dyn = function
  | A _ -> Dyn.variant "A" []
```

It is also worth noting that if you ignore all elements of a tuple but one,
the dyn converter will treat it as it was just that type and not as a tuple
anymore, e.g.:
```ocaml
type t = int * (string [@ignore])
[@@deriving dyn]
```

will derive:
```ocaml
let to_dyn (x0, _) = Dyn.int x0
```
