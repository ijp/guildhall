;; table.scm -- Hash tables

;; Copyright (C) 2005, 2008, 2009, 2011 by Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells table compat)
  (export make-table table? table-ref table-set! table-walk)
  (import (rnrs base)
          (rnrs control)
          (rnrs hashtables))
  
  ;;@ Create a hash table. The optional argument @var{type} can be
  ;; either @code{'eq}, @code{'eqv} or @code{'equal}.
  (define make-table
    (case-lambda
      (()
       (make-hashtable))
      ((type)
       (case type
         ((eq) (make-eq-hashtable))
         ((eqv) (make-eqv-hashtable))
         ((equal) (make-hashtable equal-hash equal?))
         (else (error 'make-table "invalid hash table type"))))))

  ;;@ Table type predicate. Hash tables are a disjoint type.
  (define table? hashtable?)

  ;;@args table key [ failure-thunk ]
  ;; Lookup @2 in @1. If no value is found for key @2, return the value
  ;; obtained by invoking @3 with no arguments, or #f if the optional
  ;; argument @3 is not specified.
  (define table-ref
    (let ((fail (list 'fail)))
      (case-lambda
        ((table key)
         (hashtable-ref table key #f))
        ((table key failure-thunk)
         (let ((result (hashtable-ref table key fail)))
           (if (eq? result fail)
               (failure-thunk)
               result))))))

  ;;@ Set the value correspoinding to @2 in @1 to @3.
  (define (table-set! table key value)
    (if value
        (hashtable-set! table key value)
        (hashtable-delete! table key)))

  ;;@ Call @2 with the key and value of every entry in @1 as arguments.
  (define (table-walk table proc)
    (let ((keys (hashtable-keys table)))
      (do ((i 0 (+ i 1)))
          ((< i (vector-length keys)))
        (let ((key (vector-ref keys)))
          (proc key (hashtable-ref table key #f))))))

)

