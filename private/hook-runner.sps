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
    (newline out-port)
    (flush-output-port out-port))
  (define (get/maybe-eof)
    (let ((line (get-line in-port)))
      (if (eof-object? line)
          line
          (read (open-string-input-port line)))))
  (define (get)
    (let ((datum (get/maybe-eof)))
      (if (eof-object? datum)
          (raise-protocol-error "unexpected EOF")
          datum)))
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
    (let ((input (get/maybe-eof)))
      (unless (eof-object? input)
        (guard (c ((serious-condition? c)
                   (put `(exception ,(condition->sexp c)))))
          (run-hook input))
        (loop)))))

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
           (else (raise-protocol-error "unexpected reply to `install-file'")))))
      ((package-name unpacked-source)
       (unless (null? args)
         (raise-hook-bug "unexpected arguments to message" message args))
       (put (list message))
       (get)))))


;;; Conditions

(define (raise-protocol-error message . irritants)
  (apply error 'raise-protocol-error message irritants))

(define (raise-hook-bug message . irritants)
  (apply error 'raise-hook-bug message irritants))

(define (condition->sexp c)
  (map simple-condition->sexp (simple-conditions c)))

(define (simple-condition->sexp c)
  (let ((c-rtd (record-rtd c)))
    (let loop ((rtd c-rtd) (fields '()))
      (cond (rtd
             (loop (record-type-parent rtd)
                   (append-reverse (record-fields rtd c) fields)))
            (else
             (cons (record-type-name c-rtd)
                   (reverse fields)))))))

(define (record-fields rtd record)
  (let* ((field-names (record-type-field-names rtd))
         (n-fields (vector-length field-names)))
    (let loop ((i (- n-fields 1)) (result '()))
      (if (< i 0)
          result
          (let ((field-value ((record-accessor rtd i) record)))
            (cond ((has-external-representation? field-value)
                   (loop (- i 1)
                         (cons (cons (vector-ref field-names i) field-value)
                               result)))
                  (else
                   (loop (- i 1) result))))))))

(define (has-external-representation? object)
  (or (number? object)
      (string? object)
      (symbol? object)
      (null? object)
      (cond ((pair? object)
             (and (has-external-representation? (car object))
                  (has-external-representation? (cdr object))))
            ((vector? object)
             (let loop ((i (- (vector-length object) 1)))
               (cond ((< i 0)
                      #t)
                     ((has-external-representation? (vector-ref object i))
                      (loop (- i 1)))
                     (else
                      #f))))
            (else
             #f))))


;;; Utilities

(define (append-reverse list1 list2)
  (let loop ((result list2) (l1 list1))
    (if (null? l1)
        result
        (loop (cons (car l1) result)
              (cdr l1)))))


;;; Entry point

(let ()
  (define (transcoded port)
    (transcoded-port port (make-transcoder (utf-8-codec))))
  (hook-runner (transcoded (standard-input-port))
               (transcoded (standard-output-port))
               (current-error-port)))
