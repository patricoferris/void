(lang dune 3.15)

(name void)

(generate_opam_files true)

(source
 (github patricoferris/void))

(authors "Patrick Ferris")

(maintainers "Patrick Ferris")

(license MIT)

(documentation https://ocaml.org/p/void)

(package
 (name void)
 (synopsis "Void Processes in Eio_linux")
 (description "A longer description")
 (depends 
   ocaml 
   dune
   eio_posix)
 (tags
  (linux process spawn)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
