(lang dune 2.5)

(name brisk-tyxml)

(license BSD3)

(source
 (github ulrikstrid/brisk-tyxml))

(homepage https://ulrikstrid.github.io/brisk-tyxml)

(documentation https://ulrikstrid.github.io/brisk-tyxml)

(authors "Ulrik Strid")

(maintainers "ulrik.strid@outlook.com")

(generate_opam_files true)

(package
 (version 0.0.1)
 (name brisk-tyxml)
 (synopsis "Brisk plus Tyxml living happily together")
 (description "A typesafe react alternative")
 (depends
  (ocaml
   (>= 4.09.0))
  (dune
   (>= 2.5))
  (reason
   (>= 4.0.0))
  brisk-reconciler
  tyxml))
