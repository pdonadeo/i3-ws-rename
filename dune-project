(lang dune 3.20)

(name ws-rename)

(version 18)

(generate_opam_files true)

(authors "Paolo Donadeo")

(bug_reports "https://github.com/pdonadeo/i3-ws-rename/issues")

(homepage "https://github.com/pdonadeo/i3-ws-rename")

(license ISC)

(maintainers "paolo@donadeo.net")

(source
 (uri git+https://github.com/pdonadeo/i3-ws-rename))

(package
 (name ws-rename)
 (synopsis "Renames i3/Sway workspaces using Icon Fonts")
 (description "Renames i3/Sway workspaces using Icon Fonts")
 (homepage https://gitlab.4sigma.it/pdonadeo/mare-magnum-injector)
 (depends
  (dune (= 3.20.2))
  (cryptokit (= 1.21.1))
  (lwt_ppx (= 5.9.2))
  (ppx_deriving_yojson (= 3.10.0))
  (batteries (= 3.10.0))
  (ctypes-foreign (= 0.24.0))
  (color (= 0.3.0))
  (re2 (= v0.17.0))
  (stdint (= 0.7.2))
  (utop (and :dev (= 2.16.0)))
  (merlin (and :dev (= 5.6-504)))
  (ocp-indent (and :dev (= 1.9.0)))
  (ocp-index (and :dev (= 1.4.0)))
  (ocamlformat (and :dev (= 0.28.1)))
  (odoc (and :dev (= 3.1.0)))
  (odig (and :dev (= 0.1.0)))
  (ocaml-lsp-server (and :dev (= 1.25.0))))
)
