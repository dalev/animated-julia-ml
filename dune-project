(lang dune 3.1)

(name animated_julia)

(generate_opam_files true)

(source
 (github dalev/animated-julia-ml))

(authors "Dale Vaillancourt")

(maintainers "Dale Vaillancourt")

(license LICENSE)

(documentation https://github.com/dalev/animated-julia-ml)

(package
 (name animated_julia)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  (ppx_sexp_message
   (>= v0.17.0))
  (ppx_base
   (>= v0.17.0))
  (eio_main
   (>= 1.3))
  (eio
   (>= 1.3))
  (domainslib
   (>= 0.5.2))
  (base_bigstring
   (>= v0.17.0))
  ocaml
  base
  cmdliner
  fmt
  tsdl))