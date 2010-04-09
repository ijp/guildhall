;;@ A package manager for R6RS implementations.
(package (dorodango (0))
  (depends (srfi)
           (spells)
           (parscheme)
           (industria)
           (ocelotl))
  (libraries (sls -> "dorodango")
             ("private" -> ("dorodango" "private")))
  (programs (("scripts" "doro.sps") -> "doro"))
  (installation-hook ()
    (import (except (rnrs) file-exists? delete-file)
            (srfi :8 receive)
            (spells fmt)
            (spells pathname)
            (spells filesys)
            (dorodango ui cmdline)
            (dorodango ui cmdline help))
    (lambda (agent)
      (receive (pathname port) (create-temp-file)
        (call-with-port port
          (lambda (port)
            (fmt port (dsp-man-page (command-list)))))
        (agent 'install-file 'man "doro.1" (->namestring pathname))
        (delete-file pathname)))))

;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
