;;; choice.sls --- Dependency solver, choice datastructures

;; Copyright (C) 2009 Andreas Rottmann
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

;; This file corresponds to aptitude's "choice.h", "choice_set.h" and
;; "choice_indexed_map.h".

;;; Code:
#!r6rs

(library (dorodango solver choice)
  (export make-install-choice
          make-install-from-dep-source-choice
          choice?
          choice-version
          choice-from-dep-source?
          choice-dep

          choice-with-dep
          choice-with-id
          choice-with-from-dep-source?
          
          generalize-choice
          choice-contains?

          choice=?
          choice<?
          choice-compare
          choice-hash
          choice-wt-type

          dsp-choice

          make-choice-set
          singleton-choice-set
          choice-set?
          choice-set-size
          choice-set-contains?
          choice-set-has-contained-choice?
          choice-set-version-of
          choice-set-choice-contained-by
          choice-set-containing-choice
          choice-set-remove-overlaps
          choice-set-insert-or-narrow
          choice-set-merge
          choice-set-adjoin
          choice-set->list
          choice-set-for-each
          choice-set-fold
          choice-set-traverse
          generalize-choice-set
          in-choice-set
          
          choice-set=?
          choice-set<?
          choice-set-compare
          choice-set-hash

          dsp-choice-set
          guarantee-choice-set
          
          make-choice-table
          choice-table?
          choice-table-size
          choice-table-ref
          choice-table-set!
          choice-table-update!
          choice-table-delete!
          choice-table-visit
          choice-table-contains?
          choice-table-for-each
          choice-table-fold
          choice-table-copy
          
          dsp-choice-table)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :67 compare-procedures)
          (only (spells misc) unspecific)
          (spells hash-utils)
          (spells record-types)
          (spells misc)
          (spells fmt)
          (spells foof-loop)
          (spells tracing)
          (spenet wt-tree)
          (dorodango private utils)
          (dorodango solver universe))


;;; Choice

;; This is simplified compared to aptitude, as we have only on type of
;; choice (no soft dependencies)
(define-record-type* choice
  (make-choice version from-dep-source? dep id)
  ())

(define-functional-fields choice
  version from-dep-source? dep id)

(define make-install-choice
  (let ((who 'make-install-choice))
    (case-lambda
      ((version dep id)
       (make-choice (guarantee-version version who)
                    #f
                    (guarantee-dependency dep who)
                    id))
      ((version id)
       (make-choice (guarantee-version version who) #f #f id)))))

(define (make-install-from-dep-source-choice version dep id)
  (define who 'make-install-from-dep-source-choice)
  (make-choice version #t (guarantee-dependency dep who) id))

(define (generalize-choice choice)
  (choice-with-from-dep-source? choice #f))

(define (choice-contains? choice other)
  (and (version=? (choice-version choice)
                  (choice-version other))
       (cond ((not (choice-from-dep-source? choice))
              #t)
             ((not (choice-from-dep-source? other))
              #f)
             (else
              (dependency=? (choice-dep choice) (choice-dep other))))))

(define (choice=? c1 c2)
  (= 0 (choice-compare c1 c2)))

(define (choice<? c1 c2)
  (< (choice-compare c1 c2) 0))

(define (choice-compare c1 c2)
  (refine-compare
   (version-compare (choice-version c1)
                    (choice-version c2))
   (boolean-compare (choice-from-dep-source? c1)
                    (choice-from-dep-source? c2))
   (let ((d1 (choice-dep c1))
         (d2 (choice-dep c2)))
     (cond ((and (not d1) d2)
            -1)
           ((and d1 (not d2))
            1)
           (else
            (dependency-compare d1 d2))))))

(define (choice-hash choice)
  (hash-combine
   (version-hash (choice-version choice))
   (hash-combine
    (if (choice-from-dep-source? choice) 1 0)
    (cond ((choice-dep choice) => dependency-hash)
          (else 0)))))

(define choice-wt-type
  (make-wt-tree-type choice<?))

(define-guarantor guarantee-choice choice? "choice")

(define (dsp-choice choice)
  (lambda (st)
    ((cat "Install(" (dsp-version (choice-version choice))
          (cond ((choice-dep choice)
                 => (lambda (dep)
                      (cat " "
                           (if (choice-from-dep-source? choice)
                               "<source: "
                               "<")
                           (dsp-dependency dep)
                           ">")))
                (else
                 fmt-null))
          ")") st)))


;;; Choice set

(define (make-choice-set)
  (make-wt-tree package-wt-type))

(define (singleton-choice-set choice)
  (singleton-wt-tree package-wt-type
                     (version-package (choice-version choice))
                     choice))

(define (choice-set? thing)
  ;; This is weak; if we had wt-tree/type, this could be made stronger
  (wt-tree? thing))

(define (choice-set-size choice-set)
  (wt-tree/size choice-set))

(define (choice-set-version-of choice-set package)
  (and=> (wt-tree/lookup choice-set package #f)
         (lambda (choice)
           (choice-version choice))))

(define (choice-set->list choice-set)
  (wt-tree/fold (lambda (package choice lst)
                  (cons choice lst))
                '()
                choice-set))

(define (more-specific-choice c1 c2)
  (define who 'more-specific-choice)
  (cond ((choice-contains? c1 c2)
         c2)
        ((choice-contains? c2 c1)
         c1)
        (else
         (assertion-violation who "conflicting choices" c1 c2))))

(define (choice-set-insert-or-narrow choice-set choice)
  (let ((package (version-package (choice-version choice))))
    (cond ((wt-tree/lookup choice-set package #f)
           => (lambda (existing-choice)
                (let ((specific (more-specific-choice existing-choice choice)))
                  (if (eq? specific existing-choice)
                      choice-set
                      (wt-tree/add choice-set package choice)))))
          (else
           (wt-tree/add choice-set package choice)))))

(define (generalize-choice-set choice-set)
  (wt-tree/fold
   (lambda (package choice result)
     (choice-set-insert-or-narrow result (generalize-choice choice)))
   (make-choice-set)
   choice-set))

(define (choice-set-merge choice-set other)
  (wt-tree/union-merge choice-set
                       other
                       (lambda (package c1 c2) (more-specific-choice c1 c2))))

(define (choice-set-adjoin choice-set choice-list)
  (define who 'choice-set-adjoin)
  (loop ((for choice (in-list choice-list))
         (with result
               (guarantee-choice-set choice-set who)
               (choice-set-insert-or-narrow result choice)))
    => result))

(define (choice-set-has-contained-choice? choice-set choice)
  (and (choice-set-choice-contained-by choice-set choice) #t))

(define (choice-set-choice-contained-by choice-set choice)
  (and=> (wt-tree/lookup choice-set
                         (version-package (choice-version choice))
                         #f)
         (lambda (existing-choice)
           (and (choice-contains? choice existing-choice)
                existing-choice))))

(define (choice-set-containing-choice choice-set choice)
  (and=> (wt-tree/lookup choice-set
                         (version-package (choice-version choice))
                         #f)
         (lambda (existing-choice)
           (and (choice-contains? existing-choice choice)
                existing-choice))))

(define (choice-set-contains? choice-set choice)
  (and (choice-set-containing-choice choice-set choice) #t))

(define (choice-set-remove-overlaps choice-set choice)
  (wt-tree/delete choice-set (version-package (choice-version choice))))

(define (choice-set-for-each proc choice-set)
  (wt-tree/for-each (lambda (package choice)
                      (proc choice))
                    choice-set))

(define (choice-set-fold proc initial choice-set)
  (wt-tree/fold (lambda (package choice seed)
                  (proc choice seed))
                initial
                choice-set))

;; TODO: Find out how expensive `call/cc' is here; it /might/ make
;; sense to provide a terminatable fold in the wt-tree library.
(define (choice-set-traverse proc initial choice-set)
  (call/cc
    (lambda (exit)
      (wt-tree/fold (lambda (package choice seed)
                      (proc choice seed exit))
                    initial
                    choice-set))))

(define-syntax in-choice-set
  (syntax-rules ()
    ((_ (datum-var) (choice-set-expr) cont . env)
     (cont
      (((tree size)                              ;Outer bindings
        (let ((tree choice-set-expr))
          (values tree (wt-tree/size tree))))) 
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((datum-var)                              ;Body bindings
        (wt-tree/index-datum tree index)))
      ()                                         ;Final bindings
      . env))))

(define (choice-set-compare cs1 cs2)
  (vector-compare-as-list
   (lambda (pair1 pair2)
     (pair-compare package-compare choice-compare pair1 pair2))
   cs1
   cs2
   (lambda (cs)
     (wt-tree/size cs))
   (lambda (cs i)
     (wt-tree/index-pair cs i))))

(define (choice-set=? cs1 cs2)
  (= (choice-set-compare cs1 cs2) 0))

(define (choice-set<? cs1 cs2)
  (< (choice-set-compare cs1 cs2) 0))

(define (choice-set-hash choice-set)
  (wt-tree/fold (lambda (package choice hash)
                  (hash-combine (choice-hash choice)
                                (hash-combine (package-hash package)
                                              hash)))
                0
                choice-set))

(define (dsp-choice-set choices)
  (lambda (st)
    ((cat "{" (fmt-join dsp-choice (wt-tree-datums->list choices)) "}") st)))

(define-guarantor guarantee-choice-set choice-set? "choice-set")


;;; Choice table

;; This is called `generic_choice_indexed_map' in aptitude.

(define-record-type* choice-table
  (%make-choice-table (size) (install-versions))
  ())

(define-record-type* version-info
  (make-version-info not-from-dep-source from-dep-source)
  ())

(define (make-choice-table)
  (%make-choice-table 0 (make-wt-tree version-wt-type)))

(define (choice-table-copy table)
  (%make-choice-table (choice-table-size table)
                      (choice-table-install-versions table)))

(define-functional-fields version-info
  not-from-dep-source from-dep-source)

(define %not-found (list '%not-found))

(define (choice-table-update! table choice proc default)
  (define (increment-size!)
    (set-choice-table-size! table (+ 1 (choice-table-size table))))
  (define (updated-info info)
    (cond ((eq? info %not-found)
           (increment-size!)
           (if (choice-from-dep-source? choice)
               (make-version-info %not-found
                                  (singleton-wt-tree dependency-wt-type
                                                     (choice-dep choice)
                                                     (proc default)))
               (make-version-info (proc default) (make-wt-tree dependency-wt-type))))
          ((not (choice-from-dep-source? choice))
           (unless (version-info-not-from-dep-source info)
             (increment-size!))
           (version-info-with-not-from-dep-source info (proc default)))
          (else
           (let ((from-dep-source (version-info-from-dep-source info)))
             (unless (wt-tree/member? (choice-dep choice) from-dep-source)
               (increment-size!))
             (version-info-with-from-dep-source
              info
              (wt-tree/update from-dep-source (choice-dep choice) proc default))))))
  (set-choice-table-install-versions!
   table
   (wt-tree/update (choice-table-install-versions table)
                   (choice-version choice)
                   updated-info
                   %not-found)))

(define (choice-table-set! table choice datum)
  (choice-table-update! table choice (lambda (x) datum) #f))

(define (choice-table-ref table choice default)
  (let ((version-info (wt-tree/lookup (choice-table-install-versions table)
                                      (choice-version choice)
                                      #f)))
    (if (not version-info)
        default
        (let ((datum
               (if (choice-from-dep-source? choice)
                   (wt-tree/lookup (version-info-from-dep-source version-info)
                                   (choice-dep choice)
                                   %not-found)
                   (version-info-not-from-dep-source version-info))))
          (if (eq? datum %not-found)
              default
              datum)))))

(define (choice-table-delete! table choice)
  (define (update-info! new-info)
    (set-choice-table-install-versions!
     table
     (wt-tree/add (choice-table-install-versions table)
                  (choice-version choice)
                  new-info))
    (set-choice-table-size! table (- (choice-table-size table) 1)))
  (and-let* ((version-info
              (wt-tree/lookup (choice-table-install-versions table)
                              (choice-version choice)
                              #f)))
    (cond ((choice-from-dep-source? choice)
           (let* ((from-dep-source (version-info-from-dep-source version-info))
                  (found? (wt-tree/member? (choice-dep choice) from-dep-source)))
             (when found?
               (update-info! (version-info-with-from-dep-source
                              version-info
                              (wt-tree/delete from-dep-source (choice-dep choice)))))))
          ((not (eq? %not-found (version-info-not-from-dep-source version-info)))
           (update-info! (version-info-with-not-from-dep-source
                          version-info
                          %not-found))))))

;; This is invokes `proc' for each entry in the table where the entry's
;; choice is contained in `choice'. If `proc' returns a true value,
;; this value is returned, and no further entries are considered. This
;; means that if this procedure returns #f, either no entries were
;; eligible, or `proc' returned #f on each invokation.
;;
;; NB: This is called `for_each_key_contained_in' in aptitude, and has
;; inverted semantics regading the return value of `proc'.
(define (choice-table-visit table choice proc)
  (let ((version (choice-version choice))
        (dep (choice-dep choice)))
    (define (visit-from-dep-source info)
      (let* ((version-map (version-info-from-dep-source info)))
        (loop continue ((for i (up-from 0 (to (wt-tree/size version-map)))))
          => #f
          (let ((pair (wt-tree/index-pair version-map i)))
            (cond ((proc (make-install-from-dep-source-choice version (car pair) #f)
                         (cdr pair))
                   => values)
                  (else
                   (continue)))))))
    (and=> (wt-tree/lookup (choice-table-install-versions table)
                           (choice-version choice)
                           #f)
           (lambda (info)
             (cond ((choice-from-dep-source? choice)
                    ;; If the choice is a from-dep-source choice, find
                    ;; the particular dependency that it applies to and
                    ;; apply `proc'.
                    (and=> (wt-tree/lookup (version-info-from-dep-source info) dep #f)
                           (lambda (datum)
                             (proc choice datum))))
                   (else
                    ;; If the choice isn't a from-dep-source choice,
                    ;; apply the function to the not-from-dep-source
                    ;; cell and to all the from-dep-source cells.
                    (let ((not-from-dep-source (version-info-not-from-dep-source info)))
                      (if (eq? not-from-dep-source %not-found)
                          (visit-from-dep-source info)
                          (or (proc (make-install-choice version #f)
                                    not-from-dep-source)
                              (visit-from-dep-source info))))))))))

(define (choice-table-contains? table choice)
  (choice-table-visit table choice (lambda (c dep) #t)))

(define (choice-table-fold proc seed table)
  (wt-tree/fold
   (lambda (version info seed)
     (let ((seed (let ((not-from-dep-source (version-info-not-from-dep-source info)))
                   (if (eq? not-from-dep-source %not-found)
                       seed
                       (proc (make-install-choice version #f)
                             not-from-dep-source
                             seed)))))
       (wt-tree/fold
        (lambda (dep datum seed)
          (proc (make-install-from-dep-source-choice version dep #f)
                datum
                seed))
        seed
        (version-info-from-dep-source info))))
   seed
   (choice-table-install-versions table)))

(define (choice-table-for-each proc table)
  (choice-table-fold (lambda (choice datum seed)
                       (proc choice datum))
                     #f
                     table)
  (unspecific))

(define (dsp-choice-table table datum-formatter)
  (define (do-dsp st)
    (cdr (choice-table-fold
                (lambda (choice datum state)
                  (let ((first? (car state))
                        (st (cdr state)))
                    (cons #f
                          ((cat (if first? fmt-null " ")
                                (dsp-choice choice) " -> " (datum-formatter datum)) st))))
                (cons #t st)
                table)))
  (cat "{" do-dsp "}"))



(define (wt-tree-datums->list wt-tree)
  (wt-tree/fold (lambda (key datum lst)
                  (cons datum lst))
                '()
                wt-tree))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
