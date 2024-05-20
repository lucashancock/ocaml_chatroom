(lang dune 3.15)

(name ocaml_chatroom)

(generate_opam_files true)

(source
 (github lucashancock/ocaml_chatroom))

(authors "Lucas Hancock")

(maintainers "Lucas Hancock")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ocaml_chatroom)
 (synopsis "Simple Ocaml chatroom with server and client")
 (description "Ocaml chatroom using lwt")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
