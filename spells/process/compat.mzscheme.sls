;;; compat.mzscheme.sls --- OS process, mzscheme compat.

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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
          (only (scheme base)
                subprocess subprocess-wait subprocess-status)
          (only (scheme system)
                system/exit-code)
          (only (mzlib os) getpid)
          (only (r6rs private ports)
                r6rs-port->port)
          (srfi :8 receive)
          (srfi :9 records)
          (spells pathname))

  (define-record-type process
    (make-process id input output errors)
    process?
    (id process-id)
    (input process-input)
    (output process-output)
    (errors process-errors))

  (define (x->strlist lst)
    (map (lambda (s)
           (cond ((string? s)   s)
                 ((pathname? s) (->namestring s))
                 (else
                  (error 'x->strlist "cannot coerce to string list" lst s))))
         lst))

  (define (maybe-port->mz-port port)
    (and port (r6rs-port->port port)))
  
  (define (spawn-process env stdin stdout stderr prog . args)
    (receive (process stdout-port stdin-port stderr-port)
             (apply subprocess
                    (maybe-port->mz-port stdout)
                    (maybe-port->mz-port stdin)
                    (maybe-port->mz-port stderr)
                    (x->strlist (cons prog args)))
      (make-process process stdin-port stdout-port stderr-port)))

  (define (status->values status)
    (if (<= 0 status)
        (values status #f)
        (values status 'unknown)))
  
  (define (wait-for-process process)
    (subprocess-wait (process-id process))
    (status->values (subprocess-status (process-id process))))

  (define (run-shell-command cmd)
    (status->values (system/exit-code cmd)))

  (define (get-process-id)
    (getpid))

  )
