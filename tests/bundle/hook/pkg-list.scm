(package (hook (0))
  (installation-hook ()
    (import (rnrs))
    (lambda (agent)
      (call-with-output-file ",test.tmp"
        (lambda (port)
          (write '(library (test)
                    (export)
                    (import (rnrs base)))
                 port)))
      (agent 'install-file 'libraries "test.scm" ",test.tmp")
      (delete-file ",test.tmp"))))

(package (hook-crash (0))
  (depends (hook))
  (installation-hook ()
    (import (rnrs))
    (lambda (agent)
      (error 'installation-hook "testing error handling"))))

;; Local Variables:
;; scheme-indent-styles: (pkg-list)
;; End:
