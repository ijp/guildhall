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
      (agent 'install-file 'libraries "test.sls" ",test.tmp")
      (delete-file ",test.tmp"))))

;; Local Variables:
;; scheme-indent-styles: ((package 1) (installation-hook 1))
;; End:
