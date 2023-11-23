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
- It uses `Dyn.pair` and `Dyn.triple` for tuples of the corresponding size
- Larger tuple types are converted using an inline
   `(fun (x0, x1, x2, x3) -> Dyn.tuple [...])`
- It uses `Dyn.record` for record types
- It uses `Dyn.variant` for variant and polymorphic variant types. Inline record
  arguments are converted using `Dyn.record` as well.
- An `'a Dyn.builder` extra argument is expected for each type parameter,
  following the declaration order, e.g. for a `type ('a, 'b) t` it will generate
  `val to_dyn : 'a Dyn.builder -> 'b Dyn.builder -> ('a, 'b) t Dyn.builder` 
