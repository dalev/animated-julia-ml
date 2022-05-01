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
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
