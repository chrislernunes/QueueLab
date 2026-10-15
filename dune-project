(lang dune 3.16)
(name queuelab)
(generate_opam_files false)
(package
 (name queuelab)
 (synopsis "Deterministic market replay, queue-position inference and execution simulation")
 (depends
  ocaml
  dune
  yojson
  (qcheck-core :with-test)))
