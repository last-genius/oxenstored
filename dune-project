(lang dune 3.15)

(formatting
 (enabled_for ocaml))

(cram enable)

(implicit_transitive_deps false)

(generate_opam_files true)

(using dune_site 0.1)

(name "oxenstored")

(source
 (github xapi-project/oxenstored))

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(authors "xen-api@lists.xen.org")

(maintainers "Xapi project maintainers")

(homepage "https://xapi-project.github.io/")

(package
 (name oxenstored)
 (synopsis "Oxenstored")
 (depends
  dune-site
  (alcotest :with-test))
 (sites
  (lib plugins)))
