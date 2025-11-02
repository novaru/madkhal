(lang dune 3.11)

(name madkhal)

(version 1.0.0)

(generate_opam_files true)

(source
 (github novaru/madkhal))

(authors "Febriyan Ahza <fbryn@proton.me>")

(maintainers "Febriyan Ahza <fbryn@proton.me>")

(license MIT)

(package
 (name madkhal)
 (synopsis "OCaml API Gateway with validation and transformation")
 (description
  "Production-ready API gateway leveraging OCaml's type system for safe request validation and routing")
 (depends
  (ocaml
   (>= 5.0.0))
  (dune
   (>= 3.11))
  (dream
   (>= 1.0.0~alpha5))
  (yojson
   (>= 2.0.0))
  (lwt
   (>= 5.6.0))
  (cohttp-lwt-unix
   (>= 5.0.0))
  (uri
   (>= 4.2.0))
  (logs
   (>= 0.7.0))
  (fmt
   (>= 0.9.0))
  (uuidm
   (>= 0.9.8))
  (re
   (>= 1.10.0))
  (alcotest
   (and
    :with-test
    (>= 1.6.0)))
  (qcheck
   (and
    :with-test
    (>= 0.20)))
  (qcheck-alcotest
   (and
    :with-test
    (>= 0.20)))))
