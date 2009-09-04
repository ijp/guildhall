;;@ A package manager for R6RS implementations.
(package (dorodango)
  (depends (spells))
  (libraries (libs -> "dorodango"))
  (programs (("scripts" "doro.sps") -> "doro.sps")))

;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
