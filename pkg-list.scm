;;@ A package manager for R6RS implementations.
(package (dorodango (0))
  (depends (srfi)
           (spells)
           (parscheme)
           (industria)
           (ocelotl))
  (libraries (sls -> "dorodango")
             ("private" -> ("dorodango" "private")))
  (programs (("scripts" "doro.sps") -> "doro")))

;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
