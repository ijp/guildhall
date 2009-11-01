;;@ A package manager for R6RS implementations.
(package (dorodango)
  (depends (srfi)
           (spells)
           (industria)
           (ocelotl))
  (libraries (libs -> "dorodango"))
  (programs (("scripts" "doro.sps") -> "doro")))

;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
