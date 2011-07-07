#!r6rs

;;@ Miscellaneous procedures providing access to various bits of
;; information regarding the host running the scheme implementation.
(library (spells sysutils)
  (export lookup-environment-variable
          current-process-environment
          extend-process-environment
          find-exec-path
          host-info)
  (import (rnrs base)
          (rnrs lists)
          (srfi :98 os-environment-variables)
          (spells sysutils compat))

  (define lookup-environment-variable get-environment-variable)
  (define current-process-environment get-environment-variables)
  
  (define (extend-process-environment env)
    (let ((current-env (remp (lambda (x) (assoc (car x) env))
                             (current-process-environment))))
      (append env current-env)))

  )
