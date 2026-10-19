(lang dune 3.16)
(name queue_positioning)
(generate_opam_files false)
(package
 (name queue_positioning)
 (synopsis "Deterministic market replay, queue-position inference and execution simulation")
 (depends
  ocaml
  dune
  yojson
  (qcheck-core :with-test)))
