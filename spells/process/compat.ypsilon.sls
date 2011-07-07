;;; compat.ypsilon.sls --- process compat library for Ypsilon

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells process compat)
  (export process?
          process-id
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process

          get-process-id
          
          run-shell-command)
  (import (rnrs base)
          (rnrs io ports)
          (rnrs arithmetic bitwise)
          (srfi :8 receive)
          (srfi :9 records)
          (spells pathname)
          (prefix (only (core primitives)
                        process-spawn
                        process
                        process-wait
                        system)
                  yp:)
          (only (core destructuring) destructuring-bind))

  (define-record-type process
    (make-process pid input output errors)
    process?
    (pid process-id)
    (input process-input)
    (output process-output)
    (errors process-errors))

  (define (x->strlist who lst)
    (map (lambda (s)
           (cond ((string? s)   s)
                 ((pathname? s) (->namestring s))
                 (else
                  (error who "cannot coerce to string list" lst))))
         lst))

  (define (spawn-process env stdin stdout stderr prog . args)
    (destructuring-bind (pid p-in p-out p-err)
                        (apply yp:process-spawn #f env stdin stdout stderr
                               (x->strlist 'spawn-process (cons prog args)))
      (make-process pid p-in p-out p-err)))

  (define (status->values status)
    (if (>= status 0)
        (values status #f)
        (values #f (- status))))

  (define (wait-for-process process)
    (status->values (yp:process-wait (process-id process) #f)))

  (define (run-shell-command cmd)
    (status->values (yp:system cmd)))

  (define (get-process-id)
    ;; FIXME: Ypsilon doesn't wrap getpid() yet.
    12356789)

)
