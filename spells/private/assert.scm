;;; assert.scm --- Assertations and debugging aids.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>, based on code taken
;; from SSAX, written by Oleg Kiselyov and placed in the public
;; domain.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Assert the truth of an expression (or of a sequence of expressions).
;;
;; syntax: @code{assert @var{?expr} @var{?expr} ... [report: @var{?r-exp} @var{?r-exp}
;; ...]}
;;
;; If @code{(and @var{?expr} @var{?expr} ...)} evaluates to anything but
;; @code{#f}, the result is the value of that expression. Otherwise, an
;; error is reported.
;;
;; The error message will show the failed expressions, as well as the
;; values of selected variables (or expressions, in general). The user may
;; explicitly specify the expressions whose values are to be printed upon
;; assertion failure -- as @var{?r-exp} that follow the identifier
;; @code{report:}.
;;
;; Typically, @var{?r-exp} is either a variable or a string constant. If
;; the user specified no @var{?r-exp}, the values of variables that are
;; referenced in @var{?expr} will be printed upon the assertion failure.
;;
(define-syntax assert
  (syntax-rules (report:)
    ((assert "doit" (expr ...) (r-exp ...))
     (cond
      ((and expr ...) => (lambda (x) x))
      (else
       (error 'assert "assertion failure" (list '(and expr ...) r-exp ...)))))
    ((assert "collect" (expr ...))
     (assert "doit" (expr ...) ()))
    ((assert "collect" (expr ...) report: r-exp ...)
     (assert "doit" (expr ...) (r-exp ...)))
    ((assert "collect" (expr ...) expr1 stuff ...)
     (assert "collect" (expr ... expr1) stuff ...))
    ((assert stuff ...)
     (assert "collect" () stuff ...))))

;;@ Invoke @code{display} all elements of @1, except for procedures,
;; which are called with no arguments instead of being
;; @code{display}ed.
(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))


;;@ Invoke @code{(display x (current-error-port))} all elements @var{x}
;; of @1, except for procedures, which are called with
;; @code{(current-error-port)} as single argument instead of being
;; @code{display}ed.
(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                (x (current-error-port))
                (display x (current-error-port))))
            args))

;; arch-tag: 4e369d8c-f537-4a45-94eb-7815b53bc510
