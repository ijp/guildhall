;;; inventory.scm --- Tree data structure modeling a hierarchical namespace

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (guildhall inventory)
  (export make-inventory
          inventory?
          inventory-leaf?
          inventory-container?
          inventory-empty?
          inventory-name
          inventory-data

          inventory-open
          inventory-enter
          inventory-leave
          inventory-leave-n
          inventory-next
          inventory-previous
          inventory-top

          in-inventory
          
          inventory-cursor
          inventory-cursor?
          inventory-cursor-node
          inventory-cursor-name
          inventory-cursor-data
          inventory-cursor-path
          inventory-cursor-next
          
          inventory-insert
          inventory-delete
          inventory-relabel
          
          inventory-ref
          inventory-ref-data
          inventory-update

          inventory->tree
          tree->inventory
          
          merge-inventories)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (guildhall ext foof-loop)
          (guildhall spells zipper-tree))


;;; Constructors and deconstructors

(define-record-type item
  (fields name data))

(define (make-inventory name data)
  (make-zipper (list (make-item name data))))

(define (make-node name container?  data)
  (let ((item (make-item name data)))
    (if container? (list item) item)))

(define (rename-node node name)
  (if (pair? node)
      (cons (make-item name (item-data (car node))) (cdr node))
      (make-item name (item-data node))))

(define (inventory? thing)
  (and (zipper? thing)
       (let ((node (zipper-node thing)))
         (or (item? node)
             (and (pair? node)
                  (item? (car node)))))))

(define (inventory-container? inventory)
  (pair? (zipper-node inventory)))

(define (inventory-empty? inventory)
  (null? (cdr (zipper-node inventory))))

(define (inventory-leaf? inventory)
  (not (inventory-container? inventory)))

(define (inventory-item inventory)
  (if (inventory-leaf? inventory)
      (zipper-node inventory)
      (car (zipper-node inventory))))

(define (inventory-name inventory)
  (item-name (inventory-item inventory)))

(define (inventory-data inventory)
  (item-data (inventory-item inventory)))

;;; Navigation

(define (inventory-open inventory)
  (zip-down inventory))

(define (inventory-enter inventory)
  (zip-right (zip-down inventory)))

(define (inventory-leave inventory)
  (zip-up inventory))

(define (inventory-leave-n inventory n)
  (if (= n 0)
      inventory
      (inventory-leave-n (zip-up inventory) (- n 1))))

(define (inventory-next inventory)
  (zip-right inventory))

(define (inventory-previous inventory)
  (zip-left inventory))

(define (inventory-top inventory)
  (zip-top inventory))

;;@ Foof-loop iterator for inventories
(define-syntax in-inventory
  (syntax-rules (result)
    ((_ (item-var) (inventory-expr) cont . env)
     (cont
      (((inventory) inventory-expr))     ;Outer bindings
      ((item-var (inventory-enter inventory)
                 (inventory-next item-var))) ;Loop variables
      ()                                 ;Entry bindings
      ((not item-var))                   ;Termination conditions
      ()                                 ;Body bindings
      ()                                 ;Final bindings
      . env))
    ((_ (item-var) (inventory-expr (result result-var)) cont . env)
     (cont
      (((inventory contents)
        (let ((inventory inventory-expr))
          (values inventory (inventory-enter inventory))))) ;Outer bindings
      ((item-var contents next)
       (cursor contents (or next item-var)))         ;Loop variables
      ()                    ;Entry bindings
      ((not item-var))                   ;Termination conditions
      (((next) (inventory-next item-var))) ;Body bindings
      (((result-var) (if cursor
                         (inventory-leave cursor)
                         inventory))) ;Final bindings
      . env))))

;;; Cursors

;; An `inventory-cursor' iterates over the leaf nodes of an inventory
;; in depth-first order, and keeps track of the "path" back to the
;; root of the traversal.

(define-record-type (:inventory-cursor make-inventory-cursor inventory-cursor?)
  (fields
   (immutable node inventory-cursor-node)
   (immutable path inventory-cursor-path)))

(define (inventory-cursor-name cursor)
  (inventory-name (inventory-cursor-node cursor)))

(define (inventory-cursor-data cursor)
  (inventory-data (inventory-cursor-node cursor)))

(define (inventory-cursor inventory)
  (and-let* (((inventory-container? inventory))
             (node (inventory-enter inventory)))
    (maybe-container-next-cursor node '())))

(define (maybe-container-next-cursor node path)
  (cond ((inventory-leaf? node)
         (make-inventory-cursor node path))
        ((inventory-enter node)
         => (lambda (child)
              (maybe-container-next-cursor child
                                           (cons (inventory-name node) path))))
        (else
         (next-cursor node path))))

(define (next-cursor node path)
  (cond ((inventory-next node)
         => (lambda (next-node)
              (maybe-container-next-cursor next-node path)))
        ((null? path)
         #f)
        (else
         (next-cursor (inventory-leave node) (cdr path)))))

(define (inventory-cursor-next cursor)
  (next-cursor (inventory-cursor-node cursor)
               (inventory-cursor-path cursor)))

(define (inventory-ref/aux inventory path not-found)
  (loop continue ((for path-elt path-rest (in-list path))
                  (with cursor inventory))
    => cursor
    (cond ((and (inventory-container? cursor)
                (loop next-child ((for child (in-inventory cursor)))
                  => #f
                  (if (string=? path-elt (inventory-name child))
                      child
                      (next-child))))
           => (lambda (child)
                (continue (=> cursor child))))
          (else
           (not-found cursor path-rest)))))

(define (inventory-ref inventory path)
  (inventory-ref/aux inventory path (lambda (node path) #f)))

(define (inventory-ref-data inventory path default)
  (cond ((inventory-ref inventory path) => inventory-data)
        (else default)))

;;; Manipulation

(define (inventory-insert-down inventory node)
  (zip-right (zip-insert-right (zip-down inventory) node)))

(define inventory-insert
  (case-lambda
    ((inventory other)
     (zip-insert-right inventory (zipper-node other)))
    ((inventory name container? data)
     (zip-insert-right inventory (make-node name container? data)))))

(define (inventory-relabel inventory name data)
  (zip-up (zip-change (zip-down inventory) (make-item name data))))

(define (inventory-update/aux inventory path other-node)
  (define (lose msg . irritants)
    (apply assertion-violation 'inventory-update msg irritants))
  (define (adder node path-rest)
    (if (inventory-leaf? node)
        (lose "unexpected leaf node (expected container)"
              inventory path)
        (if (null? (cdr path-rest))
            (inventory-insert-down node (rename-node other-node (car path-rest)))
            (inventory-update/aux
             (inventory-insert-down node (make-node (car path-rest) #t #f))
             (cdr path-rest)
             other-node))))
  (inventory-ref/aux inventory path adder))

(define inventory-update
  (case-lambda
    ((inventory path other)
     (inventory-update/aux inventory path (zipper-node other)))
    ((inventory path container? data)
     (inventory-update/aux inventory path (make-node #f container? data)))))

(define (inventory-delete inventory)
  (if (zip-leftmost? inventory)
      (assertion-violation 'inventory-delete
                           "cannot delete container info"
                           inventory)
      (let ((cursor (zip-delete inventory)))
        (values (zip-leftmost? cursor) cursor))))

;;; Conversion from/to trees

(define (tree->inventory tree data)
  (let ((inventory (make-inventory (car tree) data)))
    (loop continue ((for item (in-list (reverse (cdr tree))))
                    (with cursor (inventory-open inventory)))
      => (inventory-leave cursor)
      (continue
       (=> cursor (if (pair? item)
                      (inventory-insert cursor (tree->inventory item data))
                      (inventory-insert cursor item #f data)))))))

(define (inventory->tree inventory)
  (loop ((for cursor (in-inventory inventory))
         (for result
              (listing-reverse
               (cond ((inventory-leaf? cursor)
                      (inventory-name cursor))
                     ((inventory-empty? cursor)
                      #f)
                     (else
                      (inventory->tree cursor)))
               => values)))
    => (cons (inventory-name inventory) result)))



(define (merge-inventories a-inventory b-inventory conflict)
  (loop continue ((with to a-inventory)
                  (for from (in-inventory b-inventory)))
    => to
    (let ((leaf? (inventory-leaf? from))
          (cursor (inventory-ref to (list (inventory-name from)))))
      (continue
       (=> to
           (if leaf?
               (if cursor
                   (conflict cursor from)
                   (inventory-leave
                    (inventory-insert (inventory-open to) from)))
               (cond ((and cursor (inventory-leaf? cursor))
                      (conflict cursor from))
                     (cursor
                      (inventory-leave
                       (merge-inventories cursor from conflict)))
                     (else
                      (inventory-leave
                       (inventory-insert (inventory-open to) from))))))))))
)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
