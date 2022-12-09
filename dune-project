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
  ppx_sexp_message
  ocaml
  dune
  base
  cmdliner
  ctypes
  integers
  fmt
  lwt
  time_now
  tsdl
  conf-sdl2
  conf-libffi)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
