;;; expression.sls --- Dynamically updateable expression DAG

;; Copyright (C) 2009, 2010 Andreas Rottmann
;; Copyright (C) 2009 Daniel Burrows

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

(library (guildhall solver expression)
  (export expression/value
          expression/add-parent!
          expression/remove-parent!
          expression/dsp

          expression/child
          expression/set-child!
          
          expression/add-child!
          expression/remove-child!

          make-expression-wrapper
          
          make-var-expression
          expression/set-value!
          
          make-and-expression
          make-or-expression)
  (import (rnrs)
          (only (srfi :1) append-reverse count)
          (ice-9 weak-vector)
          (guildhall spells operations)
          (guildhall ext foof-loop)
          (guildhall ext fmt))

(define (make-weak-cell obj)
  ;; Guile seems to have issues with `weak-vector', so we do it this
  ;; way
  (let ((result (make-weak-vector 1)))
    (vector-set! result 0 obj)
    result))

(define (weak-cell-ref weak-cell)
  (vector-ref weak-cell 0))


;;; Operations valid on all expressions

(define-operation (expression/init! expr))
(define-operation (expression/add-parent! expr parent))
(define-operation (expression/remove-parent! expr))

(define-operation (expression/signal-value-changed expr old-value new-value))

(define-operation (expression/value expr))
(define-operation (expression/dsp expr))

(define (make-expression)
  (let ((parents '()))
    (object #f
      ((expression/signal-value-changed expr old-value new-value)
       (loop ((for parent-cell (in-list parents))
              (let parent (weak-cell-ref parent-cell))
              (for remaining-parents (listing parent-cell (if parent))))
         => (set! parents remaining-parents)
         (when parent
           (expression/child-modified parent expr old-value new-value))))
      ((expression/add-parent! expr parent)
       (set! parents (cons (make-weak-cell parent) parents)))
      ((expression/remove-parent! expr parent)
       (set! parents (remp (lambda (parent-cell)
                             (let ((p (weak-cell-ref parent-cell)))
                               (or (not p) (eq? p parent))))
                           parents))))))

;;; Variable expression

(define-operation (expression/set-value! expr value))

(define (make-var-expression value =?)
  (join (object #f
          ((expression/value expr)
           value)
          ((expression/set-value! expr new-value)
           (unless (=? value new-value)
             (let ((old-value value))
               (set! value new-value)
               (expression/signal-value-changed expr old-value new-value))))
          ((expression/dsp expr)
           (dsp `(var ,value))))
        (make-expression)))

;;; Container operations (both N-Ary, and boxes)

(define-operation (expression/child-modified expr child old-value new-value))

;;; Expression boxes

(define-operation (expression/child expr))
(define-operation (expression/set-child! expr))

(define (make-expression-box child)
  (join (object #f
          ((expression/init! expr)
           (when child
             (expression/add-parent! child expr)))
          ((expression/child expr)
           child)
          ((expression/set-child! expr new-child)
           (when child
             (expression/remove-parent! child expr))
           (set! child new-child)
           (when new-child
             (expression/add-parent! new-child expr)))
          ((expression/value expr)
           (and child (expression/value child)))
          ((expression/dsp expr)
           (if child
               (expression/dsp child)
               fmt-null)))
        (make-expression)))

(define-operation (expression/changed expr new-value)
  (values))

(define (make-expression-wrapper child)
  (join (object #f
          ((expression/child-modified expr child old-value new-value)
           (expression/changed expr new-value)))
        (make-expression-box child)))

;;; N-ary container operations

(define-operation (expression/add-child! expr child))
(define-operation (expression/remove-child! expr child))
(define-operation (expression/n-children expr))

(define (make-container-expression name children)
  (let ((n-children (length children)))
    (join (object #f
            ((expression/init! expr)
             (for-each (lambda (child)
                         (expression/add-parent! child expr))
                       children))
            ((expression/add-child! expr child)
             (set! children (cons child children))
             (set! n-children (+ n-children 1))
             (expression/add-parent! expr expr))
            ((expression/remove-child! expr child)
             (loop continue ((for c rest (in-list children))
                             (with seen '() (cons c seen)))
               (cond ((eq? c child)
                      (set! children (append-reverse seen rest))
                      (set! n-children (- n-children 1)))
                     (else
                      (continue))))
             (expression/remove-parent! child expr))
            ((expression/n-children expr)
             n-children)
            ((expression/dsp expr)
             (cat "(" name " " (fmt-join expression/dsp children " ") ")")))
          (make-expression))))

;;; Counting boolean container expression

(define-operation (expression/n-true expr))

(define (make-counting-bool-expression name children)
  (let ((n-true (count (lambda (child) (expression/value child)) children))
        (container (make-container-expression name children)))
    (define (maybe-signal expr thunk)
      (let ((old-value (expression/value expr)))
        (thunk)
        (let ((new-value (expression/value expr)))
          (unless (eqv? old-value new-value)
            (expression/signal-value-changed expr old-value new-value)))))
    (join (object #f
            ((expression/n-true expr)
             n-true)
            ((expression/child-modified expr child old-value new-value)
             (unless (eqv? old-value new-value)
               (maybe-signal expr
                             (lambda ()
                               (set! n-true (+ n-true (if new-value 1 -1)))))))
            ((expression/add-child! expr child)
             (if (expression/value child)
                 (maybe-signal expr
                               (lambda ()
                                 (expression/add-child! container child)
                                 (set! n-true (+ 1 n-true))))
                 (expression/add-child! container child)))
            ((expression/remove-child! expr child)
             (if (expression/value child)
                 (maybe-signal expr
                               (lambda ()
                                 (expression/remove-child! container child)
                                 (set! n-true (- n-true 1))))
                 (expression/remove-child! container child))))
          container)))

;;; Boolean combinators

(define (make-and-expression sub-exprs)
  (let ((expr (join (object #f
                      ((expression/value expr)
                       (= (expression/n-true expr) (expression/n-children expr))))
                    (make-counting-bool-expression 'and sub-exprs))))
    (expression/init! expr)
    expr))

(define (make-or-expression sub-exprs)
  (let ((expr (join (object #f
                      ((expression/value expr)
                       (> (expression/n-true expr) 0)))
                    (make-counting-bool-expression 'or sub-exprs))))
    (expression/init! expr)
    expr))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (object 1))
;; End:
