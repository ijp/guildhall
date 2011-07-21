;;; promotion-set.scm --- Dependency solver, promotion set

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

;; This file corresponds to aptitude's "promotion_set.h".

;;; Code:
#!r6rs

(library (guildhall solver promotions)
  (export make-promotion
          promotion?
          promotion-choices
          promotion-tier
          promotion-valid-condition
          
          promotion-compare
          promotion=?
          promotion<?
          
          promotion-wt-type
          dsp-promotion
          
          make-promotion-set
          promotion-set?
          promotion-set-insert!
          find-highest-incipient-promotions
          find-highest-incipient-promotions-containing)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :67 compare-procedures)
          (wak fmt)
          (wak foof-loop)
          (spells record-types)
          (spells xvector)
          (ocelotl wt-tree)
          (guildhall private utils)
          (guildhall solver logging)
          (guildhall solver choice)
          (guildhall solver expression)
          (guildhall solver universe))

(define-record-type* promotion
  (really-make-promotion choices tier valid-condition)
  ())

(define make-promotion
  (let ((who 'make-promotion))
    (case-lambda
      ((choices tier valid-condition)
       (really-make-promotion (guarantee-choice-set choices who)
                              (guarantee-tier tier who)
                              valid-condition))
      ((choices tier)
       (really-make-promotion (guarantee-choice-set choices who)
                              (guarantee-tier tier who)
                              #f)))))

(define (promotion-compare p1 p2)
  (refine-compare
   (tier-compare (promotion-tier p1)
                 (promotion-tier p2))
   (choice-set-compare (promotion-choices p1)
                       (promotion-choices p2))))

(define (promotion=? p1 p2)
  (= (promotion-compare p1 p2) 0))

(define (promotion<? p1 p2)
  (< (promotion-compare p1 p2) 0))

(define promotion-wt-type (make-wt-tree-type promotion<?))

(define (dsp-promotion p)
  (cat "(T" (dsp-tier (promotion-tier p)) ": " (dsp-choice-set (promotion-choices p))
       (cond ((promotion-valid-condition p)
              => (lambda (valid-condition)
                   (cat "; V:" (expression/dsp valid-condition))))
             (else
              fmt-null))
       ")"))


;;; Promotion set

(define-record-type* promotion-set
  (really-make-promotion-set retracted index)
  ((entries (make-wt-tree tier-wt-type))
   (n-promotions 0)))

(define-record-type* index-entry
  (make-index-entry)
  ((not-from-dep-source '())
   (from-dep-source (make-eq-hashtable))))

(define (make-promotion-set universe retracted)
  (really-make-promotion-set
   retracted
   (make-vector (universe-version-count universe) #f)))


(define (promotion-set-insert! set promotion)
  (let ((p-tier (promotion-tier promotion))
        (highest (find-highest-promotion-for set (promotion-choices promotion)))
        (entries (promotion-set-entries set)))
    (define (remove-superseded-entries)
      (let ((superseded-entries (find-superseded-entries set promotion)))
        (unless (null? superseded-entries)
          (let ((installed-versions (make-eq-hashtable))
                (superseded-entry-set (make-eq-hashtable)))
            (loop ((for entry (in-list superseded-entries)))
              (collect-indexers-for-entry! installed-versions entry)
              (hashtable-set! superseded-entry-set entry #t))
            (log/trace "Removing index entries associated"
                       " with the superseded entries.")
            (drop-index-entries!
             set
             installed-versions
             (lambda (entry)
               (hashtable-ref superseded-entry-set entry #f)))
            (log/trace "Removing the superseded entries themselves.")
            (loop ((for entry (in-list superseded-entries))
                   (for superseded-count (up-from 0)))
              => (begin
                   (set-promotion-set-n-promotions!
                    set
                    (- (promotion-set-n-promotions set) superseded-count)))
              (log/trace "Removing " (dsp-promotion entry))
              (let* ((removed-tier (promotion-tier entry))
                     (tier-entries (wt-tree/lookup entries removed-tier #f)))
                (cond (tier-entries
                       (xvector-remove-first! tier-entries entry eq?)
                       (when (= 0 (xvector-length tier-entries))
                         (log/debug "Tier " (dsp-tier removed-tier) " is empty"
                                    ", deleting it")
                         (wt-tree/delete! entries removed-tier)))
                      (else
                       (internal-error "Tier " (dsp-tier removed-tier)
                                       " has gone missing!")))))))))
    (cond
      ((and highest (tier>=? (promotion-tier highest) p-tier))
       (log/info "Canceling the insertion of " (dsp-promotion promotion)
                 ": it is redundant with the existing promotion "
                 (dsp-promotion highest))
       #f)
      (else
       (remove-superseded-entries)
       (log/trace
        "Inserting " (dsp-promotion promotion) " into tier " (dsp-tier p-tier))
       (let ((p-tier-entries
              (or (wt-tree/lookup entries p-tier #f)
                  (let ((new-entries (make-xvector)))
                    (log/trace "Allocating initial structures for tier "
                               (dsp-tier p-tier))
                    (wt-tree/add! entries p-tier new-entries)
                    new-entries))))
         (xvector-push! p-tier-entries promotion)
         (set-promotion-set-n-promotions!
          set
          (+ 1 (promotion-set-n-promotions set)))
         (log/trace "Building index entries for " (dsp-promotion promotion))
         (let ((index (promotion-set-index set)))
           (choice-set-for-each
            (lambda (choice)
              (create-index-entries! index promotion choice))
            (promotion-choices promotion))))
       #t))))

(define (find-highest-incipient-promotions promotion-set choices output-domain)
  (let* ((output-incipient (make-hashtable choice-hash choice=?))
         (traverser (make-intersection-traverser
                     promotion-set
                     #t
                     (make-incipient-subset-finder output-incipient output-domain)))
         (non-incipient (traverser choices #f)))
    ;; The above code won't find promotions that include only values
    ;; in the output domain.  Look for those by hand.
    (choice-table-for-each
     (make-unary-promotion-finder promotion-set output-incipient)
     output-domain)
    (values output-incipient non-incipient)))

(define (find-highest-incipient-promotions-containing promotion-set
                                                      choices
                                                      choice
                                                      output-domain
                                                      pred)
  (let ((entries (find-index-list promotion-set choice))
        (result (make-hashtable choice-hash choice=?)))
    (unless (null? entries)
      (let ((choices-by-version (make-eq-hashtable)))
        (choice-set-for-each
         (lambda (choice)
           (hashtable-set! choices-by-version (choice-version choice) choice))
         choices)
        (loop ((for promotion (in-list entries)))
          (when (pred promotion)
            (let ((incipient? (collect-choices! result
                                                1
                                                (promotion-choices promotion)
                                                choices-by-version)))
              (when incipient?
                (choice-set-for-each
                 (make-incipient-updater result output-domain promotion)
                 choices)))))))
    result))

(define (find-highest-promotion-for promotion-set choice-set)
  (let ((traverser (make-intersection-traverser promotion-set #t (make-subset-finder))))
    (traverser choice-set #f)))


;; Promotion set auxiliaries

(define (create-index-entries! index promotion choice)
  (let* ((version (choice-version choice))
         (id (version-id version))
         (index-entry
          (or (vector-ref index id)
              (let ((new-index-entry (make-index-entry)))
                (log/trace "Creating a new index cell for " (dsp-version version))
                (vector-set! index id new-index-entry)
                new-index-entry)))
         (from-dep-source (index-entry-from-dep-source index-entry))
         (not-from-dep-source (index-entry-not-from-dep-source index-entry)))
    (cond ((not (choice-from-dep-source? choice))
           (log/trace
            "Inserting " (dsp-choice choice) " into the not-from-dep-source-list.")
           (set-index-entry-not-from-dep-source!
            index-entry
            (cons promotion not-from-dep-source))
           (loop ((for dep (in-vector (hashtable-keys from-dep-source))))
             (log/trace
              "Inserting " (dsp-choice choice) "into the from-dep-source list for "
              (dsp-dependency dep) ".")
             (hashtable-update! from-dep-source
                                dep
                                (lambda (entries)
                                  (cons promotion entries))
                                '())))
          (else
           (log/trace "Inserting " (dsp-choice choice) " into the from-dep-source-list.")
           (hashtable-update! from-dep-source
                              (choice-dep choice)
                              (lambda (entries)
                                (cons promotion
                                      (or entries not-from-dep-source)))
                              #f)))))

(define (collect-indexers-for-entry! installed-versions entry)
  (choice-set-for-each
   (lambda (choice)
     (hashtable-set! installed-versions (choice-version choice) #t))
   (promotion-choices entry)))

;; NB: This subsumes the functionality of
;; `check_choices_in_local_indices' in aptitude.
(define (collect-choices! output num-mismatches choices choices-by-version)
  (loop continue ((for choice (in-choice-set choices))
                  (with remaining-mismatches num-mismatches)
                  (until (< remaining-mismatches 0)))
    => (= remaining-mismatches 0)
    (let ((match?
           (and-let* ((index-choice (hashtable-ref choices-by-version
                                                   (choice-version choice)
                                                   #f)))
             (or (not (choice-from-dep-source? choice))
                 (and (choice-from-dep-source? index-choice)
                      (dependency=? (choice-dep choice)
                                    (choice-dep index-choice)))))))
      (if match?
          (continue)
          (continue (=> remaining-mismatches (- remaining-mismatches 1)))))))

(define (drop-index-entries! promotion-set installed-versions pred)
  (let ((index (promotion-set-index promotion-set)))
    (loop ((for version (in-vector (hashtable-keys installed-versions))))
      (let ((index-entry (vector-ref index (version-id version))))
        (unless index-entry
          (internal-error "The version " (dsp-version version)
                          " didn't actually have index entries, but it should have."))
        (log/trace "Purging dead references from the index entries for "
                   (dsp-version version))
        (let ((not-from-dep-source
               (remp pred (index-entry-not-from-dep-source index-entry)))
              (from-dep-source (index-entry-from-dep-source index-entry)))
          (loop ((for dep entries (in-hashtable from-dep-source)))
            (let ((new-entries (remp pred entries)))
              (if (null? new-entries)
                  (hashtable-delete! from-dep-source dep)
                  (hashtable-set! from-dep-source dep new-entries))))
          (cond ((and (null? not-from-dep-source)
                      (= 0 (hashtable-size from-dep-source)))
                 (log/trace "All index entries for " (dsp-version version)
                            " have been removed, dropping index cell.")
                 (vector-set! index (version-id version) #f))
                (else
                 (set-index-entry-not-from-dep-source! index-entry not-from-dep-source)
                 (set-index-entry-from-dep-source! index-entry from-dep-source))))))))

(define (find-superseded-entries promotion-set promotion)
  (let ((traverser (make-intersection-traverser
                    promotion-set
                    #f
                    (make-supersets-finder (choice-set-size
                                            (promotion-choices promotion))
                                           (promotion-tier promotion)))))
    (traverser (promotion-choices promotion) '())))

;; This whole traverser business is probably over-general and could be
;; simplified.
(define (make-intersection-traverser promotion-set subset? proc)
  (define (traverse proc seed choice-set)
    (choice-set-traverse
     (lambda (choice seed return)
       (let ((entries (find-index-list promotion-set choice)))
         (if (null? entries)
             (cond (subset?
                    seed)
                   (else
                    (log/debug
                     "intersection-traverser: breaking out of set traversal at "
                     (dsp-choice choice) " because nothing matches it and we are"
                     " looking for a superset.")
                    (return seed)))
             (loop ((for entry (in-list entries))
                    (with seed seed (proc entry seed)))
               => seed))))
     seed
     choice-set))
  (let ((hit-counts (make-eq-hashtable)))
    (lambda (choice-set seed)
      (traverse (lambda (entry seed)
                  (hashtable-update! hit-counts entry (lambda (n) (+ 1 n)) 0))
                #f
                choice-set)
      (traverse (lambda (entry seed)
                  (let* ((hit-count (hashtable-ref hit-counts entry #f))
                         (new-seed (if (not hit-count)
                                       seed
                                       (proc entry hit-count seed))))
                    (hashtable-set! hit-counts entry #f)
                    new-seed))
                seed
                choice-set))))

(define (make-supersets-finder required-hits maximum-tier)
  (lambda (entry hit-count entries)
    (cond ((tier<? maximum-tier (promotion-tier entry))
           entries)
          ((= hit-count required-hits)
           (cons entry entries))
          (else
           (when (> hit-count required-hits)
             (internal-error "supersets-finder: invalid hit count " hit-count
                             "; expected maximum of " required-hits "!"))
           entries))))

(define (make-subset-finder)
  (lambda (entry hit-count result)
    (if (and (= hit-count (choice-set-size (promotion-choices entry)))
             (or (not result)
                 (tier<? (promotion-tier result) (promotion-tier entry))))
        entry
        result)))

(define (make-incipient-subset-finder output-incipient output-domain)
  (lambda (entry hit-count non-incipient)
    (let* ((choices (promotion-choices entry))
           (choices-size (choice-set-size choices)))
      (cond ((= (+ hit-count 1) choices-size)
             (choice-set-for-each
              (make-incipient-updater output-incipient output-domain entry)
              choices)
             non-incipient)
            ((= hit-count choices-size)
             (if (or (not non-incipient)
                     (tier<? (promotion-tier non-incipient) (promotion-tier entry)))
                 entry
                 non-incipient))
            (else
             non-incipient)))))

(define (make-incipient-updater output output-domain promotion)
  (lambda (choice)
    (when (choice-table-visit output-domain choice (lambda (choice datum) #t))
      (hashtable-update! output
                         choice
                         (lambda (existing-promotion)
                           (if (or (not existing-promotion)
                                   (tier<? (promotion-tier existing-promotion)
                                           (promotion-tier promotion)))
                               promotion
                               existing-promotion))
                         #f))))

(define (make-unary-promotion-finder promotion-set output)
  (lambda (choice value)
    value ;ignored
    (loop ((for entry (in-list (find-index-list promotion-set choice))))
      (let ((choices (promotion-choices entry)))
        (when (= 1 (choice-set-size choices))
          (let ((p-choice (choice-set-containing-choice choices choice)))
            (hashtable-update!
             output
             p-choice
             (lambda (old-promotion)
               (if old-promotion
                   (or (and (tier<? (promotion-tier old-promotion)
                                    (promotion-tier entry))
                            entry)
                       old-promotion)
                   entry))
             #f)))))))

(define (find-index-list promotion-set choice)
  (or (and-let* ((index-entry (vector-ref (promotion-set-index promotion-set)
                                          (version-id (choice-version choice)))))
        (cond ((not (choice-from-dep-source? choice))
               (index-entry-not-from-dep-source index-entry))
              (else
               (or (hashtable-ref (index-entry-from-dep-source index-entry)
                                  (choice-dep choice)
                                  #f)
                   (index-entry-not-from-dep-source index-entry)))))))


)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
