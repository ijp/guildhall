;;; wt-tree.scm --- R6RS port of MIT/GNU Scheme's weight-balanced trees

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>
;;
;; Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
;;     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;     2006, 2007, 2008 Massachusetts Institute of Technology
;;
;; Copyright (c) 1993-1994 Stephen Adams
;;
;; This program was written by Stephen Adams, based on the following
;; reference:

;;   Stephen Adams, Implemeting Sets Efficiently in a Functional
;;      Language, CSTR 92-10, Department of Electronics and Computer
;;      Science, University of Southampton, 1992

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
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

(library (sigil ext wt-tree)
  (export make-wt-tree-type
          number-wt-type
          string-wt-type
          make-wt-tree
          singleton-wt-tree
          alist->wt-tree
          
          wt-tree?
          wt-tree/empty?
          wt-tree/size
          wt-tree/add
          wt-tree/add!
          wt-tree/member?
          wt-tree/lookup
          wt-tree/delete
          wt-tree/delete!

          ;; "Advanced operations"
          wt-tree/split<
          wt-tree/split>
          wt-tree/union
          wt-tree/intersection
          wt-tree/difference
          wt-tree/subset?
          wt-tree/set-equal?
          wt-tree/fold
          wt-tree/for-each
          wt-tree/union-merge

          ;; Indexing operations
          wt-tree/index
          wt-tree/index-datum
          wt-tree/index-pair
          wt-tree/rank
          wt-tree/min
          wt-tree/min-datum
          wt-tree/min-pair
          wt-tree/delete-min
          wt-tree/delete-min!

          ;; ocelotl extras
          wt-tree/update
          )
  (import (rnrs))

;;;  A tree type is a collection of those procedures that depend on the ordering
;;;  relation.

(define-record-type (tree-type %make-tree-type tree-type?)
  (fields
   (immutable key<? tree-type/key<?)
   (immutable alist->tree tree-type/alist->tree)
   (immutable add tree-type/add)
   (immutable insert! tree-type/insert!)
   (immutable delete tree-type/delete)
   (immutable delete! tree-type/delete!)
   (immutable member? tree-type/member?)
   (immutable lookup tree-type/lookup)
   ;;min        ; ?  also delmin, max, delmax, delmin!, delmax!
   (immutable split-lt tree-type/split-lt)
   (immutable split-gt tree-type/split-gt)
   (immutable union tree-type/union)
   (immutable union-merge tree-type/union-merge)
   (immutable intersection tree-type/intersection)
   (immutable difference tree-type/difference)
   (immutable subset? tree-type/subset?)
   (immutable rank tree-type/rank)))


;;;  Tree representation
;;;
;;;  WT-TREE is a wrapper for trees of nodes
;;;
(define-record-type (wt-tree %make-wt-tree wt-tree?)
  (fields
   (mutable type tree/type set-tree/type!)
   (mutable root tree/root set-tree/root!)))

;;;  Nodes are the thing from which the real trees are built.

(define (make-node k v l r w) (vector w l k r v))
(define (node/k node) (vector-ref node 2))
(define (node/v node) (vector-ref node 4))
(define (node/l node) (vector-ref node 1))
(define (node/r node) (vector-ref node 3))
(define (node/w node) (vector-ref node 0))

(define empty  'empty)
(define (empty? x) (eq? x 'empty))

(define (node/size node)
  (if (empty? node) 0  (node/w node)))

(define (node/singleton k v) (make-node k v empty empty 1))

(define (with-n-node node receiver)
  (receiver (node/k node) (node/v node) (node/l node) (node/r node)))


;;;
;;;  Constructors for building node trees of various complexity
;;;

(define (n-join k v l r)
  (make-node k v l r (fx+ 1 (fx+ (node/size l) (node/size r)))))

(define (single-l a.k a.v x r)
  (with-n-node r
    (lambda (b.k b.v y z) (n-join b.k b.v (n-join a.k a.v x y) z))))

(define (double-l a.k a.v x r)
  (with-n-node r
    (lambda (c.k c.v r.l z)
      (with-n-node r.l
        (lambda (b.k b.v y1 y2)
          (n-join b.k b.v
                  (n-join a.k a.v x y1)
                  (n-join c.k c.v y2 z)))))))

(define (single-r b.k b.v l z)
  (with-n-node l
    (lambda (a.k a.v x y) (n-join a.k a.v x (n-join b.k b.v y z)))))

(define (double-r c.k c.v l z)
  (with-n-node l
    (lambda (a.k a.v x l.r)
      (with-n-node l.r
        (lambda (b.k b.v y1 y2)
          (n-join b.k b.v
                  (n-join a.k a.v x y1)
                  (n-join c.k c.v y2 z)))))))

(define wt-tree-ratio 5)

(define (t-join k v l r)
  (define (simple-join) (n-join k v l r))
  (let ((l.n  (node/size l))
        (r.n  (node/size r)))
    (cond ((fx<? (fx+ l.n r.n) 2)   (simple-join))
          ((fx>? r.n (fx* wt-tree-ratio l.n))
           ;; right is too big
           (let ((r.l.n  (node/size (node/l r)))
                 (r.r.n  (node/size (node/r r))))
             (if (fx<? r.l.n r.r.n)
                 (single-l k v l r)
                 (double-l k v l r))))
          ((fx>? l.n (fx* wt-tree-ratio r.n))
           ;; left is too big
           (let ((l.l.n  (node/size (node/l l)))
                 (l.r.n  (node/size (node/r l))))
             (if (fx<? l.r.n l.l.n)
                 (single-r k v l r)
                 (double-r k v l r))))
          (else
           (simple-join)))))

;;;
;;;  Node tree Procedures that are independent of key<?
;;;

(define (node/min node)
  (cond  ((empty? node)           (error:empty 'min))
         ((empty? (node/l node))  node)
         (else                   (node/min (node/l node)))))

(define (node/delmin node)
  (cond ((empty? node)           (error:empty 'delmin))
        ((empty? (node/l node))  (node/r node))
        (else   (t-join (node/k node) (node/v node)
                        (node/delmin (node/l node)) (node/r node)))))

(define (node/concat2 node1 node2)
  (cond ((empty? node1)   node2)
        ((empty? node2)   node1)
        (else
         (let ((min-node (node/min node2)))
           (t-join (node/k min-node) (node/v min-node)
                   node1 (node/delmin node2))))))

(define (node/inorder-fold procedure base node)
  (define (fold base node)
    (if (empty? node)
        base
        (with-n-node node
          (lambda (k v l r)
            (fold (procedure k v (fold base r)) l)))))
  (fold base node))

(define (node/for-each procedure node)
  (if (not (empty? node))
      (with-n-node node
        (lambda (k v l r)
          (node/for-each procedure l)
          (procedure k v)
          (node/for-each procedure r)))))

(define (node/height node)
  (if (empty? node)
      0
      (+ 1 (max (node/height (node/l node)) (node/height (node/r node))))))

(define (node/index node index)
  (define (loop node index)
    (let ((size.l  (node/size (node/l node))))
      (cond ((fx<? index size.l)  (loop (node/l node) index))
            ((fx>? index size.l)  (loop (node/r node)
                                         (fx- index (fx+ 1 size.l))))
            (else                  node))))
  (let ((bound  (node/size node)))
    (if (or (< index 0)
            (>= index bound)
            (not (fixnum? index)))
        (error 'node/index "argument outside valid range" index)
        (loop node index))))

(define (error:empty owner)
  (error owner "operation requires non-empty tree"))

(define (make-wt-tree-type key<?)

  (define (key>? x y)  (key<? y x))

  (define (node/find k node)
    ;; returns either the node or #f.
    ;; Loop takes D comparisons (D is the depth of the tree) rather than the
    ;; traditional compare-low, compare-high which takes on average
    ;; 1.5(D-1) comparisons
    (define (loop this best)
      (cond ((empty? this)  best)
            ((key<? k (node/k this))   (loop (node/l this) best))
            (else (loop (node/r this) this))))
    (let ((best (loop node #f)))
      (cond ((not best)               #f)
            ((key<? (node/k best) k)  #f)
            (else                     best))))

  (define (node/rank k node rank)
    (cond ((empty? node)             #f)
          ((key<? k (node/k node))  (node/rank k (node/l node) rank))
          ((key>? k (node/k node))
           (node/rank k (node/r node)
                        (fx+ 1 (fx+ rank (node/size (node/l node))))))
          (else                     (fx+ rank (node/size (node/l node))))))

  (define (node/add node k v)
    (if (empty? node)
        (node/singleton k v)
        (with-n-node node
          (lambda (key val l r)
            (cond ((key<? k key)   (t-join key val (node/add l k v) r))
                  ((key<? key k)   (t-join key val l (node/add r k v)))
                  (else            (n-join key v   l r)))))))

  (define (node/delete x node)
    (if (empty? node)
        empty
        (with-n-node node
          (lambda (key val l r)
            (cond ((key<? x key)   (t-join key val (node/delete x l) r))
                  ((key<? key x)   (t-join key val l (node/delete x r)))
                  (else            (node/concat2 l r)))))))

  (define (node/concat tree1 tree2)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
           (let ((min-node (node/min tree2)))
             (node/concat3 (node/k min-node) (node/v min-node) tree1
                           (node/delmin tree2))))))

  (define (node/concat3 k v l r)
    (cond ((empty? l)   (node/add r k v))
          ((empty? r)   (node/add l k v))
          (else
           (let ((n1  (node/size l))
                 (n2  (node/size r)))
             (cond ((fx<? (fx* wt-tree-ratio n1) n2)
                    (with-n-node r
                                 (lambda (k2 v2 l2 r2)
                                   (t-join k2 v2 (node/concat3 k v l l2) r2))))
                   ((fx<? (fx* wt-tree-ratio n2) n1)
                    (with-n-node l
                                 (lambda (k1 v1 l1 r1)
                                   (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
                   (else
                    (n-join k v l r)))))))

  (define (node/split-lt node x)
    (cond ((empty? node)  empty)
          ((key<? x (node/k node))
           (node/split-lt (node/l node) x))
          ((key<? (node/k node) x)
           (node/concat3 (node/k node) (node/v node) (node/l node)
                         (node/split-lt (node/r node) x)))
          (else (node/l node))))

  (define (node/split-gt node x)
    (cond ((empty? node)  empty)
          ((key<? (node/k node) x)
           (node/split-gt (node/r node) x))
          ((key<? x (node/k node))
           (node/concat3 (node/k node) (node/v node)
                         (node/split-gt (node/l node) x) (node/r node)))
          (else (node/r node))))

  (define (node/union tree1 tree2)
    (cond  ((empty? tree1)  tree2)
           ((empty? tree2)  tree1)
           (else
            (with-n-node tree2
              (lambda (ak av l r)
                (let ((l1  (node/split-lt tree1 ak))
                      (r1  (node/split-gt tree1 ak)))
                  (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))

  (define (node/union-merge tree1 tree2 merge)
    (cond ((empty? tree1)  tree2)
          ((empty? tree2)  tree1)
          (else
           (with-n-node tree2
             (lambda (ak av l r)
               (let* ((node1  (node/find ak tree1))
                      (l1     (node/split-lt tree1 ak))
                      (r1     (node/split-gt tree1 ak))
                      (value  (if node1
                                  (merge ak av (node/v node1))
                                  av)))
                 (node/concat3 ak value
                               (node/union-merge l1 l merge)
                               (node/union-merge r1 r merge))))))))

  (define (node/difference tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   tree1)
          (else
           (with-n-node tree2
             (lambda (ak av l r)
               (let ((l1  (node/split-lt tree1 ak))
                     (r1  (node/split-gt tree1 ak)))
                 av
                 (node/concat (node/difference l1 l)
                              (node/difference r1 r))))))))

  (define (node/intersection tree1 tree2)
    (cond ((empty? tree1)   empty)
          ((empty? tree2)   empty)
          (else
           (with-n-node tree2
             (lambda (ak av l r)
               (let ((l1  (node/split-lt tree1 ak))
                     (r1  (node/split-gt tree1 ak)))
                 (if (node/find ak tree1)
                     (node/concat3 ak av (node/intersection l1 l)
                                   (node/intersection r1 r))
                     (node/concat (node/intersection l1 l)
                                  (node/intersection r1 r)))))))))

  (define (node/subset? tree1 tree2)
    (or (empty? tree1)
        (and (fx<=? (node/size tree1) (node/size tree2))
             (with-n-node tree1
               (lambda (k v l r)
                 v
                 (cond ((key<? k (node/k tree2))
                        (and (node/subset? l (node/l tree2))
                             (node/find k tree2)
                             (node/subset? r tree2)))
                       ((key>? k (node/k tree2))
                        (and (node/subset? r (node/r tree2))
                             (node/find k tree2)
                             (node/subset? l tree2)))
                       (else
                        (and (node/subset? l (node/l tree2))
                             (node/subset? r (node/r tree2))))))))))


  ;;; Tree interface: stripping off or injecting the tree types

  (define (tree/map-add tree k v)
    (%make-wt-tree (tree/type tree)
                   (node/add (tree/root tree) k v)))

  ;(define (tree/set-add tree k)  (tree/map-add tree k #f))

  (define (tree/insert! tree k v)
    (set-tree/root! tree (node/add (tree/root tree) k v)))

  (define (tree/delete tree k)
    (%make-wt-tree (tree/type tree)
                   (node/delete k (tree/root tree))))

  (define (tree/delete! tree k)
    (set-tree/root! tree (node/delete k (tree/root tree))))

  (define (tree/split-lt tree key)
    (%make-wt-tree (tree/type tree)
                   (node/split-lt (tree/root tree) key)))

  (define (tree/split-gt tree key)
    (%make-wt-tree (tree/type tree)
                   (node/split-gt (tree/root tree) key)))

  (define (tree/union tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/union (tree/root tree1) (tree/root tree2))))

  (define (tree/union-merge tree1 tree2 merge)
    (%make-wt-tree (tree/type tree1)
                   (node/union-merge (tree/root tree1) (tree/root tree2)
                                     merge)))

  (define (tree/intersection tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                   (node/intersection (tree/root tree1) (tree/root tree2))))

  (define (tree/difference tree1 tree2)
    (%make-wt-tree (tree/type tree1)
                  (node/difference (tree/root tree1) (tree/root tree2))))

  (define (tree/subset? tree1 tree2)
    (node/subset? (tree/root tree1) (tree/root tree2)))

  (define (alist->tree alist)
    (define (loop alist node)
      (cond ((null? alist)  node)
            ((pair? alist)  (loop (cdr alist)
                                  (node/add node (caar alist) (cdar alist))))
            (else
             (error 'alist->tree "argument has wrong type (expected alist)" alist))))
    (%make-wt-tree my-type (loop alist empty)))

  (define (tree/get tree key default)
    (let ((node  (node/find key (tree/root tree))))
      (if node
          (node/v node)
          default)))

  (define (tree/rank tree key)  (node/rank key (tree/root tree) 0))

  (define (tree/member? key tree)
    (and (node/find key (tree/root tree))
         #t))

  (define my-type
    (%make-tree-type
     key<?                ;  key<?
     alist->tree          ;  alist->tree
     tree/map-add         ;  add
     tree/insert!         ;  insert!
     tree/delete          ;  delete
     tree/delete!         ;  delete!
     tree/member?         ;  member?
     tree/get             ;  lookup
     tree/split-lt        ;  split-lt
     tree/split-gt        ;  split-gt
     tree/union           ;  union
     tree/union-merge     ;  union-merge
     tree/intersection    ;  intersection
     tree/difference      ;  difference
     tree/subset?         ;  subset?
     tree/rank            ;  rank
     ))

  my-type)



;;;
;;;
;;;

(define (guarantee-tree/report tree procedure)
  (error procedure "wrong argument type (expected weight-balanced tree)" tree))

(define (guarantee-tree tree procedure)
  (if (not (wt-tree? tree))
      (guarantee-tree/report tree procedure)))

(define (guarantee-tree-type type procedure)
  (if (not (tree-type? type))
      (error procedure "wrong argument type (expected weight-balanced tree type)" type)))

(define (guarantee-compatible-trees/report tree1 tree2 procedure)
  (guarantee-tree tree1 procedure)
  (guarantee-tree tree2 procedure)
  (error procedure "trees have incompatible types"
         tree1 tree2 (tree/type tree1) (tree/type tree2)))

(define (guarantee-compatible-trees tree1 tree2 procedure)
  (if (or (not (wt-tree? tree1))
          (not (wt-tree? tree2))
          (not (eq? (tree/type tree1) (tree/type tree2))))
      (guarantee-compatible-trees/report tree1 tree2 procedure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Exported interface
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (make-wt-tree tree-type)
  (%make-wt-tree tree-type empty))

(define (singleton-wt-tree type key value)
  (guarantee-tree-type type 'singleton-wt-tree)
  (%make-wt-tree type (node/singleton key value)))

(define (alist->wt-tree type alist)
  (guarantee-tree-type type 'alist->wt-tree)
  ((tree-type/alist->tree type) alist))

(define (wt-tree/empty? tree)
  (guarantee-tree tree 'wt-tree/empty?)
  (empty? (tree/root tree)))

(define (wt-tree/size tree)
  (guarantee-tree tree 'wt-tree/size)
  (node/size (tree/root tree)))

(define (wt-tree/add tree key datum)
  (guarantee-tree tree 'wt-tree/add)
  ((tree-type/add (tree/type tree)) tree key datum))

(define (wt-tree/delete tree key)
  (guarantee-tree tree 'wt-tree/delete)
  ((tree-type/delete (tree/type tree)) tree key))

(define (wt-tree/add! tree key datum)
  (guarantee-tree tree 'wt-tree/add!)
  ((tree-type/insert! (tree/type tree)) tree key datum))

(define (wt-tree/delete! tree key)
  (guarantee-tree tree 'wt-tree/delete!)
  ((tree-type/delete! (tree/type tree)) tree key))

(define (wt-tree/member? key tree)
  (guarantee-tree tree 'wt-tree/member?)
  ((tree-type/member? (tree/type tree)) key tree))

(define (wt-tree/lookup tree key default)
  (guarantee-tree tree 'wt-tree/lookup)
  ((tree-type/lookup (tree/type tree)) tree key default))

(define (wt-tree/split< tree key)
  (guarantee-tree tree 'wt-tree/split<)
  ((tree-type/split-lt (tree/type tree)) tree key))

(define (wt-tree/split> tree key)
  (guarantee-tree tree 'wt-tree/split>)
  ((tree-type/split-gt (tree/type tree)) tree key))

(define (wt-tree/union tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/union)
  ((tree-type/union (tree/type tree1)) tree1 tree2))

(define (wt-tree/union-merge tree1 tree2 merge)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/union-merge)
  ((tree-type/union-merge (tree/type tree1)) tree1 tree2 merge))

(define (wt-tree/intersection tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)
  ((tree-type/intersection (tree/type tree1)) tree1 tree2))

(define (wt-tree/difference tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)
  ((tree-type/difference (tree/type tree1)) tree1 tree2))

(define (wt-tree/subset? tree1 tree2)
  (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)
  ((tree-type/subset? (tree/type tree1)) tree1 tree2))

(define (wt-tree/set-equal? tree1 tree2)
  (and (wt-tree/subset? tree1 tree2)
       (wt-tree/subset? tree2 tree1)))

(define (wt-tree/fold combiner-key-datum-result init tree)
  (guarantee-tree tree 'wt-tree/fold)
  (node/inorder-fold combiner-key-datum-result init (tree/root tree)))

(define (wt-tree/for-each action-key-datum tree)
  (guarantee-tree tree 'wt-tree/for-each)
  (node/for-each action-key-datum (tree/root tree)))

(define (wt-tree/index tree index)
  (guarantee-tree tree 'wt-tree/index)
  (let ((node  (node/index (tree/root tree) index)))
    (and node (node/k node))))

(define (wt-tree/index-datum tree index)
  (guarantee-tree tree 'wt-tree/index-datum)
  (let ((node  (node/index (tree/root tree) index)))
    (and node (node/v node))))

(define (wt-tree/index-pair tree index)
  (guarantee-tree tree 'wt-tree/index-pair)
  (let ((node  (node/index (tree/root tree) index)))
    (and node (cons (node/k node) (node/v node)))))

(define (wt-tree/rank tree key)
  (guarantee-tree tree 'wt-tree/rank)
  ((tree-type/rank (tree/type tree)) tree key))

(define (wt-tree/min tree)
  (guarantee-tree tree 'wt-tree/min)
  (node/k (node/min (tree/root tree))))

(define (wt-tree/min-datum tree)
  (guarantee-tree tree 'wt-tree/min-datum)
  (node/v (node/min (tree/root tree))))

(define (wt-tree/min-pair tree)
  (guarantee-tree tree 'wt-tree/min-pair)
  (let ((node  (node/min (tree/root tree))))
    (cons (node/k node) (node/v node))))

(define (wt-tree/delete-min tree)
  (guarantee-tree tree 'wt-tree/delete-min)
  (%make-wt-tree (tree/type tree) (node/delmin (tree/root tree))))

(define (wt-tree/delete-min! tree)
  (guarantee-tree tree 'wt-tree/delete-min!)
  (set-tree/root! tree (node/delmin (tree/root tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(define number-wt-type
  ((lambda()
     (make-wt-tree-type  (lambda (x y) (< x y))))))

(define string-wt-type
  ((lambda()
     (make-wt-tree-type  string<?))))



;;; Extras

(define (wt-tree/update tree key updater default)
  (let ((datum (wt-tree/lookup tree key %not-found)))
    (wt-tree/add tree key (updater (if (eq? datum %not-found)
                                       default
                                       datum)))))

(define %not-found (list '%not-found))

)
