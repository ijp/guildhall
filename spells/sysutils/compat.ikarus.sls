;;@ Miscellaneous procedures providing access to various bits of
;; information regarding the host running the scheme implementation.
(library (spells sysutils compat)
  (export find-exec-path
          host-info)
  (import (rnrs base)
          (srfi :8 receive)
          (spells filesys)
          (spells string-utils)
          (only (srfi :13 strings) string-index)
          (prefix (only (ikarus)
                        getenv
                        host-info)
                  ik:))

  (define (find-exec-path prog)
    (let ((paths (string-split (ik:getenv "PATH") #\:)))
      (find-file prog paths file-executable?)))

  (define (host-info)
    (let* ((hi (ik:host-info))
           (first-dash (string-index hi #\-))
           (second-dash (and first-dash (string-index hi #\- (+ first-dash 1)))))
      (cond ((and first-dash second-dash)
             (values (substring hi 0 first-dash)
                     (substring hi (+ first-dash 1) second-dash)
                     (substring hi (+ second-dash 1) (string-length hi))))
            (first-dash
             (values (substring hi 0 first-dash)
                     "unknown"
                     (substring hi (+ first-dash 1) (string-length hi))))
            (else
             (values "unknown" "unknown" hi)))))

)
