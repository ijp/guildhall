#!r6rs
;;; zipper.sls --- Huet's "Zipper" data structure

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;@
;; A zipper is a cursor into a tree, allowing to manipulate the tree
;; in a functional manner.
;;
;; This implementation is nearly identical to the OCaml one given in
;; the paper ``Functional Pearl -- The Zipper'' by Gerard Huet. It
;; operates on a tree built from lists; the leafs may be of any type.
;;
;; There is a more general derivation of the zipper using delimited
;; continuations (see
;; @uref{http://okmij.org/ftp/Scheme/zipper-in-scheme.txt}), which does
;; not restrict the datatype to lists, but this one is likely to be
;; more performant, at least on implementations that don't provide
;; native support for delimited continuations.
;;
(library (spells zipper-tree)
  (export make-zipper
          zipper?
          zipper-node
          zip-top?
          zip-leftmost?
          zip-rightmost?
          
          zip-left
          zip-right
          zip-up
          zip-down
          zip-top
          zip-finish

          zip-change
          zip-insert-right
          zip-insert-left
          zip-insert-down
          zip-delete)
  (import (rnrs)
          (only (spells record-types) define-functional-fields))

(define-record-type path
  (fields left up right))

(define-functional-fields path left up right)

(define top (make-path '() #f '()))

(define-record-type (zipper %make-zipper zipper?)
  (fields node path))

;;;@subheading Construction and deconstruction

;;@ Construct a zipper based on @var{tree}.
(define (make-zipper tree)
  (%make-zipper tree top))

;;@defun zipper? object
;; Return @code{#t} if @var{object} is a zipper, @code{#f} otherwise.
;;@end defun

;;@defun zipper-node zipper
;; Return the current node of @var{zipper}.
;;@end defun

(define (make-loser who irritant)
  (lambda (msg)
    (assertion-violation who msg irritant)))

;;;@subheading Predicates

;;@ Return @code{#t} if @var{z} is at the top of its tree, @code{#f}
;; otherwise.
(define (zip-top? z)
  (eq? (zipper-path z) top))

;;@ Return @code{#t} if @var{z} is at leftmost position, @code{#f}
;; otherwise.
(define (zip-leftmost? z)
  (null? (path-left (zipper-path z))))

;;@ Return @code{#t} if @var{z} is at rightmost position, @code{#f}
;; otherwise.
(define (zip-rightmost? z)
  (null? (path-right (zipper-path z))))

;;;@subheading Movement

;;@ Return a zipper moved to the left of @var{z}.
(define (zip-left z)
  (define lose (make-loser 'zip-left z))
  (let ((p (zipper-path z)))
    (cond ((eq? top p)           (lose "at top"))
          ((null? (path-left p)) #f)
          (else
           (%make-zipper (car (path-left p))
                         (make-path (cdr (path-left p))
                                    (path-up p)
                                    (cons (zipper-node z)
                                          (path-right p))))))))

;;@ Return a zipper moved to the right of @var{z}.
(define (zip-right z)
  (define lose (make-loser 'zip-right z))
  (let ((p (zipper-path z)))
    (cond ((eq? p top)            (lose "at top"))
          ((null? (path-right p)) #f)
          (else
           (%make-zipper (car (path-right p))
                         (make-path (cons (zipper-node z)
                                          (path-left p))
                                    (path-up p)
                                    (cdr (path-right p))))))))

;;@ Return a zipper moved up from @var{z}.
(define (zip-up z)
  (define lose (make-loser 'zip-up z))
  (let ((p (zipper-path z)))
    (cond ((eq? p top) #f)
          (else
           (%make-zipper
            (append (reverse (path-left p))
                    (cons (zipper-node z) (path-right p)))
            (path-up p))))))

;;@ Return a zipper located on the top of @var{z}'s tree.
(define (zip-top z)
  (let loop ((z z))
    (cond ((zip-up z) => loop)
          (else          z))))

;;@ Return a zipper moved down to the leftmost child of @var{z}.
(define (zip-down z)
  (define lose (make-loser 'zip-down z))
  (let ((node (zipper-node z)))
    (cond ((pair? node)
           (%make-zipper
            (car node)
            (make-path '() (zipper-path z) (cdr node))))
          (else
           (lose "down on leaf")))))

;;@ Return the tree root of @var{z}.
(define (zip-finish z)
  (zipper-node (zip-top z)))

;;;@subheading Modification

;;@ Return a zipper with @var{z}'s current node exchanged with
;; @var{node}.
(define (zip-change z node)
  (%make-zipper node (zipper-path z)))

;;@ Return a zipper with @var{node} inserted to the right of @var{z}.
(define (zip-insert-right z node)
  (define lose (make-loser 'zip-insert-right z))
  (let ((p (zipper-path z)))
    (cond ((eq? p top)
           (lose "at top"))
          (else
           (%make-zipper
            (zipper-node z)
            (path-modify-right p (lambda (nodes) (cons node nodes))))))))


;;@ Return a zipper with @var{node} inserted to the left of @var{z}.
(define (zip-insert-left z node)
  (define lose (make-loser 'zip-insert-left z))
  (let ((p (zipper-path z)))
    (cond ((eq? p top)
           (lose "at top"))
          (else
           (%make-zipper
            (zipper-node z)
            (path-modify-left p (lambda (nodes) (cons node nodes))))))))

;;@ Return a zipper referring to @var{node}, which is inserted to as
;; leftmost child of @var{z}.
(define (zip-insert-down z node)
  (define lose (make-loser 'zip-insert-down z))
  (let ((children (zipper-node z)))
    (cond ((pair? children)
           (%make-zipper node (make-path '() (zipper-path z) children)))
          (else
           (lose "down on leaf")))))

;;@ Delete the current node of @var{z}.  Returns a zipper that is
;; either moved to the right, left, or upwards from @var{z}, in this
;; order of precedence, depending on whether such a move is possible.
(define (zip-delete z)
  (define lose (make-loser 'zip-delete z))
  (let ((p (zipper-path z)))
    (cond ((eq? p top)
           (lose "at top"))
          ((pair? (path-right p))
           (%make-zipper (car (path-right p))
                         (path-modify-right p cdr)))
          ((pair? (path-left p))
           (%make-zipper (car (path-left p))
                         (path-modify-left p cdr)))
          (else
           (%make-zipper '() (path-up p))))))

)
