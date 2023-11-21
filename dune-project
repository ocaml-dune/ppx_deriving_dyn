(lang dune 3.10)

(generate_opam_files true)

(name ppx_deriving_dyn)

(source
 (github ocaml-dune/ppx_deriving_dyn))

(license MIT)

(authors "Nathan Rebours <nathan.p.rebours@gmail.com>")

(maintainers "Nathan Rebours <nathan.p.rebours@gmail.com>")

(package
 (name ppx_deriving_dyn)
 (synopsis "PPX deriver for dune's Dyn.t converters")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.30.0))
  (dyn :with-test)))
