;;; hook-runner.sps --- Hook mechanism backend

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(import (rnrs)
        (rnrs eval))

(define (hook-runner in-port out-port error-port)
  (define (lose-unexpected input)
    (display "unexpected input: " error-port)
    (write input error-port)
    (display "\n" error-port))
  (define (put datum)
    (write datum out-port)
    (display "\n" out-port)
    (flush-output-port out-port))
  (define (get)
    (let ((datum (read in-port)))
      (if (eof-object? datum)
          (raise-protocol-error "unexpected EOF")
          datum)))
  (guard (c ((i/o-write-error? c) ;probably SIGPIPE
             #t))
    (let loop ()
      (define (run-hook input)
        (unless (and (list? input)
                     (eq? 'run-hook (car input))
                     (= 5 (length input)))
          (raise-protocol-error "expected `run-hook' form" input))
        (apply execute-hook (cdr input)))
      (define (execute-hook kind options env hook)
        (let ((agent (construct-agent kind options get put))
              (hook (eval-hook env hook)))
          (hook agent)
          (put '(hook-done))))
      (let ((input (read in-port)))
        (unless (eof-object? input)
          (run-hook input)
          (loop))))))

(define (eval-hook env hook)
  (eval hook (apply environment env)))

(define (construct-agent kind options get put)
  (case kind
    ((installation)
     (make-installation-agent options get put))
    (else
     (raise-protocol-error "unknown agent kind" kind))))

(define (make-installation-agent options get put)
  (lambda (message . args)
    (case message
      ((install-file) ;category dest-filename source-filename
       (unless (and (= 3 (length args))
                    (symbol? (car args))
                    (string? (cadr args))
                    (string? (caddr args)))
         (raise-hook-bug "invalid arguments to `install-file'" args))
       (put (cons message args))
       (let ((reply (get)))
         (case reply
           ((#t) #t)
           (else (raise-protocol-error "unexpected reply to `install-file'"))))))))


;;; Conditions

(define (raise-protocol-error message . irritants)
  (apply error 'raise-protocol-error message irritants))

(define (raise-hook-bug message . irritants)
  (apply error 'raise-hook-bug message irritants))


;;;; Entry point

(let ()
  (define (transcoded port)
    (transcoded-port port (make-transcoder (utf-8-codec))))
  (hook-runner (transcoded (standard-input-port))
               (transcoded (standard-output-port))
               (current-error-port)))
