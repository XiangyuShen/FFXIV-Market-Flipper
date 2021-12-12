(lang dune 2.9)

 (generate_opam_files true)
  (authors "Xiangyu Shen" "Austin Yu")
  (maintainers "xshen2006@yahoo.com")
  (package
   (name FFXIV_market_flipper) ; put in your own project name here
   (synopsis "An OCaml application for FFXIV")
   (description "Application to find the best goods to flip for profit on the FFXIV marketboard.")
   (depends
    (ounit2 (> 2.0)) ; for each opam dependency list the version
    (core (> 0.14.1)) ; `opam list core` will display which version of core you have
    (yojson (> 1.7.0))
    (cohttp-async (> 4.0.0))
    (cohttp-lwt-unix (> 4.0.0))
    (lwt-ssl (> 1.1.13))
    (tls (> 0.15.1))
 ))