(lang dune 3.14)
(generate_opam_files)

(source (github dariohett/ocaml-poker))
(authors "Dario Hett")

(package
 (name ocaml_poker)
 (synopsis "Basic Texas Holdem methods.")
 (description "Basic Texas Holdem methods.")
 (depends ocaml dune core)
 (tags
  (poker)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-projec
