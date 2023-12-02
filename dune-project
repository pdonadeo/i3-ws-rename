(lang dune 3.12)

(name ws-rename)

(version 15)

(generate_opam_files true)

(authors "Paolo Donadeo")
(bug_reports "https://github.com/pdonadeo/i3-ws-rename/issues")
(homepage "https://github.com/pdonadeo/i3-ws-rename")
(license ISC)
(maintainers "paolo@donadeo.net")
(source (uri git+https://github.com/pdonadeo/i3-ws-rename))

(package
 (name ws-rename)
 (synopsis "Renames i3/Sway workspaces using Icon Fonts")
 (description "Renames i3/Sway workspaces using Icon Fonts")
 (homepage https://gitlab.4sigma.it/pdonadeo/mare-magnum-injector)
 (depends
  (dune (= 3.12.1))
  (cryptokit (= 1.18))
  (lwt_ppx (= 2.1.0))
  (ppx_deriving_yojson (= 3.6.1))
  (batteries (= 3.7.1))
  (ctypes-foreign (= 0.21.1))
  (color (= 0.2.0))
  (re2 (= v0.15.0))
  (stdint (= 0.7.2))

  (utop (and :dev (= 2.13.1)))
  (merlin (and :dev (= 4.12-414)))
  (ocp-indent (and :dev (= 1.8.1)))
  (ocp-index (and :dev (= 1.3.5)))
  (ocamlformat (and :dev (= 0.26.1)))
  (odoc (and :dev (= 2.2.0)))
  (odig (and :dev (= 0.0.9)))
  (ocaml-lsp-server (and :dev (= 1.16.2)))
 )
)
