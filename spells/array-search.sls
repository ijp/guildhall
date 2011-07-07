;;; array-search.sls --- Search algorithms for array-like data

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells array-search)
  (export
    make-array-binary-search
    make-array-equal-range
    
    vector-binary-search
    vector-equal-range
    )
  (import (rnrs)
          (srfi :67 compare-procedures))

(define (make-array-binary-search array-ref array-length)
  (define binary-search
    (case-lambda
      ((array value cmp k)
       (let loop ((start 0)
                  (end (array-length array))
                  (j #f))
         (let ((i (div (+ start end) 2)))
           (if (or (= start end) (and j (= i j)))
               #f
               (let* ((elt (array-ref array i))
                      (result (cmp elt value)))
                 (if3 result
                      (loop i end i)
                      (k i elt)
                      (loop start i i)))))))
      ((array value cmp)
       (binary-search array value cmp (lambda (i elt) i)))))
  binary-search)

(define (make-array-equal-range array-ref array-length)
  (define equal-range
    (case-lambda
      ((array value cmp k)
       (define (lower-bound start end)
         (let loop ((start start)
                    (end end))
           (let ((i (div (+ start end) 2)))
             (if (= start end)
                 start
                 (if<? (cmp (array-ref array i) value)
                       (loop (+ i 1) end)
                       (loop start i))))))
       (define (upper-bound start end)
         (let loop ((start start)
                    (end end))
           (let ((i (div (+ start end) 2)))
             (if (= start end)
                 start
                 (if>? (cmp (array-ref array i) value)
                       (loop start i)
                       (loop (+ i 1) end))))))
       (let loop ((left 0)
                  (right (array-length array)))
         (let ((i (div (+ left right) 2)))
           (if (= left right)
               (k left right)
               (if3 (cmp (array-ref array i) value)
                    (loop (+ i 1) right)
                    (k (lower-bound left i)
                       (upper-bound i right))
                    (loop left i))))))
      ((array value cmp)
       (equal-range array value cmp values))))
  equal-range)

(define vector-binary-search
  (make-array-binary-search vector-ref vector-length))

(define vector-equal-range
  (make-array-equal-range vector-ref vector-length))

)
