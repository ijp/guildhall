#!r6rs
;;; awk.sls --- AWK loop macro

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; An awk loop, after the design of David Albertz and Olin Shivers.

;;; Code:


;;@ SCSH's awk macro.
(library (spells awk)
  (export awk)
  (import (rnrs)
          (only (srfi :1) append-map)
          (srfi :8 receive)
          (spells tracing)
          (for (spells awk helpers) run expand))

;;@defspec awk next-record (record field ...) state-vars . clauses
;;@defspecx awk next-record (record field ...) counter state-vars . clauses
;;
;; See
;; @uref{http://www.scsh.net/docu/html/man-Z-H-9.html#node_sec_8.2,
;; the SCSH manual}.
;;
;;@end defspec
(define-syntax awk
  (lambda (stx)
    (syntax-case stx ()
      ((_ next-record
          (record field ...)
          counter
          ((state-var init-expr) ...)
          clause ...)
       (identifier? #'counter)
       (let*-values (((clauses) (map parse-clause #'(clause ...)))
                     ((svars) #'(state-var ...))
                     ((clauses rx-bindings)
                      (optimize-clauses clauses)))
         (with-syntax (((after-body ...) (get-after-body clauses svars))
                       ((range-var ...) (get-range-vars clauses))
                       ((rx-binding ...) rx-bindings))
           #`(let ((reader (lambda () next-record))
                   rx-binding ...)
               (let ^loop-var ((counter 0)
                               (state-var init-expr) ...
                               (range-var #f) ...)
                 (receive (record field ...) (reader)
                   (cond ((eof-object? record)
                          after-body ...)
                         (else
                          #,@(expand-loop-body #'record
                                               #'counter
                                               #'(range-var ...)
                                               svars
                                               clauses)))))))))

      ;; Left out counter...
      ((_ next-record
          (record field ...)
          ((state-var init-expr) ...)
          clause ...)
       #'(awk next-record
              (record field ...)
              counter
              ((state-var init-expr) ...)
              clause ...)))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
