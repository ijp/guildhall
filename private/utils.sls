;;; utils.sls --- Utilities for dorodango

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (dorodango private utils)
  (export in-hashtable
          symbol<?
          
          wt-tree/update
          
          xvector-remove-first!
          xvector-insert!
          xvector-remove
          in-xvector
          fmt-join/xvector

          apush
          
          warn
          define-guarantor
          
          dsp-pathname
          logger:dorodango
          make-fmt-log)
  (import (rnrs)
          (srfi :8 receive)
          (ocelotl wt-tree)
          (spells foof-loop)
          (spells alist)
          (spells xvector)
          (spells fmt)
          (spells pathname)
          (spells logging))

(define-syntax define-guarantor
  (syntax-rules ()
    ((define-guarantor guarantor predicate type-name)
     (define (guarantor obj who)
       (if (predicate obj)
           obj
           (assertion-violation who
                                (string-append "invalid argument type (expected "
                                               type-name ")")
                                obj))))))

(define (warn who message . irritants)
  (raise-continuable
    (condition (make-warning)
               (make-who-condition who)
               (make-message-condition message)
               (make-irritants-condition irritants))))

(define (apush k v vals)
  (cond ((assq k vals)
         => (lambda (entry)
              (acons k (cons v (cdr entry)) (remq entry vals))))
        (else
         (acons k (list v) vals))))

(define make-fmt-log
  (case-lambda
    ((logger)
     (let ((log (make-log logger)))
       (lambda (level . formats)
         (log level (lambda (port)
                      (apply fmt port formats))))))
    ((logger level)
     (let ((log (make-log logger level)))
       (lambda formats
         (log (lambda (port) (apply fmt port formats))))))))

(define (dsp-pathname pathname)
  (lambda (st)
    ((dsp (->namestring pathname)) st)))

(define-syntax in-hashtable
  (syntax-rules ()
    ((_ (key-var datum-var) (hashtable-expr) cont . env)
     (cont
      (((keys datums size)                       ;Outer bindings
        (let ((hashtable hashtable-expr))
          (receive (keys datums)
                   (hashtable-entries hashtable)
            (values keys datums (vector-length keys))))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((key-var datum-var)
        (values (vector-ref keys index)
                (vector-ref datums index))))     ;Body bindings
      ()                                         ;Final bindings
      . env))))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))


;;; wt-tree utilities

(define not-found (list 'not-found))

(define (wt-tree/update tree key updater default)
  (let ((datum (wt-tree/lookup tree key not-found)))
    (wt-tree/add tree key (updater (if (eq? datum not-found)
                                       default
                                       datum)))))


;;; xvector utilities

(define (xvector-insert! vec index value)
  (loop continue ((for i (up-from (+ index 1) (to (xvector-length vec))))
                  (with tmp (xvector-ref vec index)))
    => (begin
         (xvector-set! vec index value)
         (xvector-push! vec tmp))
    (let ((new-tmp (xvector-ref vec i)))
      (xvector-set! vec i tmp)
      (continue (=> tmp new-tmp)))))

;; Removes (at most) a single item from xvector; does not keep
;; relative order
(define (xvector-remove-first! vec value =?)
  (loop continue ((for i (up-from 0 (to (xvector-length vec)))))
    (cond ((=? value (xvector-ref vec i))
           (let ((last-element (xvector-pop! vec)))
             (unless (= i (xvector-length vec)) ;shall we remove the last?
               (xvector-set! vec i last-element))))
          (else
           (continue)))))

;; Removes all matching values, and returns a new xvector. Keeps
;; relative order.
(define (xvector-remove vec value =?)
  (let ((result (make-xvector)))
    (loop ((for i (up-from 0 (to (xvector-length vec)))))
      => result
      (let ((elt (xvector-ref vec i)))
        (unless (=? elt value)
          (xvector-push! result elt))))))

(define-syntax in-xvector
  (syntax-rules ()
    ((_ (element-var) (xvector-expr) cont . env)
     (cont
      (((xvector size)                       ;Outer bindings
        (let ((xvector xvector-expr))
          (values xvector (xvector-length xvector)))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((element-var)                            ;Body bindings
        (xvector-ref xvector index)))
      ()                                         ;Final bindings
      . env))))

(define (fmt-join/xvector formatter vec sep)
  (lambda (st)
    (let ((len (xvector-length vec)))
      (loop ((for i (up-from 0 (to len)))
             (with st st
                   ((cat (formatter (xvector-ref vec i))
                         (if (< i (- len 1)) sep fmt-null))
                    st)))
        => st))))



;; This doesn't really belong here
(define logger:dorodango (make-logger root-logger 'dorodango))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
