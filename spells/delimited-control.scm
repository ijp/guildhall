;;; delimited-control.sls --- Delimited-control operators

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; Code taken from http://okmij.org/ftp/Scheme/delim-control-n.scm;
;; - Changed to use R6RS records instead of mutable pairs
;; - Use macrology to define all 4 operator pairs

;;; Code:
#!r6rs

;;@ Delimited control operators.
(library (spells delimited-control)
  (export abort
          prompt control shift reset
          prompt0 control0 shift0 reset0)
  (import (rnrs base)
          (rnrs records syntactic))

; This is one single global mutable cell
(define holes '())

(define (hole-push! hole) (set! holes (cons hole holes)))
(define (hole-pop!) (let ((hole (car holes))) (set! holes (cdr holes)) hole))

(define-record-type (cell cell-new cell?)
  (fields (immutable v cell-ref)
          (immutable mark cell-marked?)))

; Essentially this is the ``return from the function''
(define (abort-top! v) ((cell-ref (hole-pop!)) v))

(define (unwind-till-marked! who keep?)
  (if (null? holes)
      (assertion-violation who "No prompt set"))
  (let ((hole (hole-pop!)))
    (if (cell-marked? hole)		; if marked, it's prompt's hole
      (begin (hole-push!		; put it back
	       (if keep?
                   hole
                   (cell-new (cell-ref hole) #f))) ; make the hole non-delimiting
	'())	
      (cons hole (unwind-till-marked! who keep?)))))

(define (prompt* thunk)
  (call-with-current-continuation
    (lambda (outer-k)
      (hole-push! (cell-new outer-k #t)) ; it's prompt's hole
      (abort-top! (thunk)))))
      
(define (make-control* who shift? keep?)
  (lambda (f)
    (call-with-current-continuation
      (lambda (k-control)
        (let* ((holes-prefix (reverse (unwind-till-marked! who keep?)))
               (invoke-subcont 
                (lambda (v)
                  (call-with-current-continuation
                    (lambda (k-return)
                      (hole-push! (cell-new k-return shift?))
                      (for-each hole-push! holes-prefix)
                      (k-control v))))))
          (abort-top! (f invoke-subcont)))))))

(define-syntax define-operators
  (syntax-rules ()
    ((_ (control* control prompt) shift? keep?)
     (begin
       (define control* (make-control* 'control shift? keep?))
     
       ;; Some syntactic sugar
       (define-syntax prompt
         (syntax-rules ()
           ((prompt e) (prompt* (lambda () e)))))

       (define-syntax control
         (syntax-rules ()
           ((control k e) (control* (lambda (k) e)))))))))

(define-operators (shift* shift reset) #t #t)
(define-operators (shift0* shift0 reset0) #t #f)

(define-operators (control* control prompt) #f #t)
(define-operators (control0* control0 prompt0) #f #f)

(define (abort v) (control* (lambda (k) v)))

)
