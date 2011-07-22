;;; misc.scm --- Misc stuff.

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Stuff that doesn't fit somewhere else.
(library (spells misc)
  (export identity
          compose
          unspecific
          sleep-seconds
          sort-list
          and-map
          or-map
          and=>
          topological-sort
          scheme-implementation)
  (import (rnrs base)
          (rnrs lists)
          (rnrs io simple)
          (rnrs sorting)
          (rnrs hashtables)
          (only (guile) identity compose and-map or-map and=>
                usleep))

(define (sleep-seconds t)
  (usleep (+ (* (exact (truncate t)) #e1e+6)
             (mod (exact (round (* t #e1e+6))) #e1e+6))))
  
(define (scheme-implementation) 'guile)

;;@ Efficiently sort the list @1 using the comparison function
;; @2. Stability is not required.
(define (sort-list lst cmpf)
  (list-sort cmpf lst))

;;@ Returns the `unspecific' value, as normally returned by e.g. code
;; @code{(if #f #f)}.
(define (unspecific)
  (if #f #f))



;; `topological-sort' based on code written by Peter Danenberg,
;; (re-)licensed with his permission to new-style BSD.

;;@ Topologically sort @1, according to the equality function @2,
;;  which defaults to @code{eqv?}.
(define (topological-sort graph . maybe-eql)
  (let* ((eql? (if (pair? maybe-eql) (car maybe-eql) eqv?))
         (make-table (cond ((eq? eql? eq?)
                            make-eq-hashtable)
                           ((eq? eql? eqv?)
                            make-eqv-hashtable)
                           ((eq? eql? string=?)
                            (lambda ()
                              (make-hashtable string-hash eql?)))
                           (else
                            (lambda ()
                              (make-hashtable equal-hash eql?))))))
    (let ((discovered (make-table))
          (finalized (make-table))
          (predecessors (make-table))
          (vertices (map car graph))
          (sorted '()))
      (define (discovered? vertex)
        (hashtable-ref discovered vertex #f))
      (define (visit-if-undiscovered vertex)
        (if (not (discovered? vertex))
            (visit vertex)))
      (define (set-predecessor! child parent)
        (hashtable-set! predecessors child parent))
      (define (set-discovered! vertex)
        (hashtable-set! discovered vertex #t))
      (define (set-finalized! vertex)
        (hashtable-set! finalized vertex #t))
      (define (undiscovered-children parent)
        (filter (lambda (n)
                  (not (discovered? n)))
                (children parent)))
      (define (children parent)
        (let ((parent-children (assoc parent graph)))
          (if parent-children
              (cdr parent-children)
              '())))
      (define (visit parent)
        (set-discovered! parent)
        (let ((children (children parent)))
          (for-each (lambda (child)
                      (if (not (discovered? child))
                          (begin (set-predecessor! child parent)
                                 (visit child))))
                    children))
        (set! sorted (cons parent sorted))
        (set-finalized! parent))
      (for-each visit-if-undiscovered vertices)
      sorted)))
)
