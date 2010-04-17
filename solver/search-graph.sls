;;; search-graph.sls --- Dependency solver, search graph

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

;; This file corresponds to aptitude's "search_graph.h".

;;; Code:
#!r6rs

(library (dorodango solver search-graph)
  (export make-search-graph
          search-graph?
          search-graph-step
          search-graph-last-step
          search-graph-add-root-step!
          search-graph-add-step!
          search-graph-add-clone!
          search-graph-schedule-promotion-propagation!
          search-graph-run-scheduled-promotion-propagations!
          search-graph-bind-choice!
          search-graph-remove-choice!
          
          step?
          step-actions
          step-num
          step-tier
          step-score
          step-action-score
          step-promotion-queue-location
          step-blessed-solution?
          step-solution?
          
          step-num-unresolved-deps
          step-unresolved-deps-ref
          step-unresolved-deps-min
          step-modify-unresolved-dep!
          step-add-unresolved-dep!
          
          step-deps-solved-by-choice
          step-forbidden-versions
          step-first-child
          step-last-child?

          step-increase-score!
          step-increase-action-score!
          step-add-action!
          step-add-forbidden-version!
          step-add-dep-solved-by-choice!
          
          set-step-score!
          set-step-promotion-queue-location!
          set-step-tier!
          set-step-blessed-solution?!
          set-step-last-child?!

          dsp-step

          make-promotion-queue
          promotion-queue-index
          promotion-queue-action-sum
          promotion-queue-promotion
          promotion-queue-next
          promotion-queue-push!
          in-promotion-queue

          solver-tracker?
          make-solver-tracker
          solver-tracker-size
          solver-tracker-lookup
          solver-tracker-structural-reasons
          solver-tracker-update
          solver-tracker-remove
          solver-tracker-for-each
          solver-tracker-fold
          in-solver-tracker
          
          dsp-solver-tracker
          dsp-solver-tracker-solvers
          
          make-solver-info
          solver-info?
          solver-info-tier
          solver-info-reasons
          solver-info-tier-valid
          solver-info-is-deferred-listener)
  (import (rnrs)
          (only (srfi :1) car+cdr)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :67 compare-procedures)
          (wak foof-loop)
          (wak fmt)
          (ocelotl wt-tree)
          (spells record-types)
          (spells xvector)
          (spells tracing)
          (dorodango private utils)
          (dorodango solver expression)
          (dorodango solver logging)
          (dorodango solver choice)
          (dorodango solver promotions)
          (dorodango solver universe))


;;; A step in the search graph

(define-record-type* step
  (make-step (actions)
             (action-score)
             (score)
             parent
             (tier)
             reason
             is-deferred-listener
             (unresolved-deps)
             (unresolved-deps-by-num-solvers)
             deps-solved-by-choice
             (forbidden-versions))
  ((num #f)
   (last-child? #t)
   (blessed-solution? #f)
   (first-child #f)
   (promotion-queue-location #f)
   (last-promotion-search 0)
   (choice-set-hit-count 0)
   (solver-set-hit-count 0)
   (first-solver-hit #f)
   (clones (make-wt-tree number-wt-type))
   (canonical-clone #f)
   (successor-constraints (make-wt-tree choice-wt-type))
   (promotions (make-wt-tree promotion-wt-type))
   (promotions-list (make-xvector))
   (promotions-list-first-new-promotion 0)))

(define (step-increase-action-score! step amount)
  (set-step-action-score! step (+ (step-action-score step) amount)))

(define (step-increase-score! step amount)
  (set-step-score! step (+ (step-score step) amount)))

(define (step-promotions-list-push! step promotion)
  (set-step-promotions-list! step (cons promotion (step-promotions-list step))))

(define (step-unresolved-deps-ref step dep)
  (wt-tree/lookup (step-unresolved-deps step) dep #f))

(define (step-num-unresolved-deps step)
  (wt-tree/size (step-unresolved-deps step)))

(define (step-solution? step)
  (= 0 (step-num-unresolved-deps step)))

(define (step-add-unresolved-dep! step dep solvers)
  (set-step-unresolved-deps! step (wt-tree/add (step-unresolved-deps step)
                                               dep
                                               solvers))
  (set-step-unresolved-deps-by-num-solvers!
   step
   (wt-tree/add (step-unresolved-deps-by-num-solvers step)
                (cons (solver-tracker-size solvers) dep)
                #t)))

(define (step-modify-unresolved-dep! step dep proc)
  (define (update-solvers old-solvers)
    (let* ((new-solvers (proc old-solvers))
           (old-size (solver-tracker-size old-solvers))
           (new-size (solver-tracker-size new-solvers)))
      (when (not (= old-size new-size))
        (log/trace "Changing the number of solvers of "
                   (dsp-dependency dep) " from " old-size
                   " to " new-size " in step " (step-num step))
        (set-step-unresolved-deps-by-num-solvers!
         step
         (wt-tree/add (wt-tree/delete (step-unresolved-deps-by-num-solvers step)
                                      (cons (solver-tracker-size old-solvers) dep))
                      (cons (solver-tracker-size new-solvers) dep)
                      #t)))
      new-solvers))
  (cond ((wt-tree/lookup (step-unresolved-deps step) dep #f)
         => (lambda (old-solvers)
              (let ((new-solvers (update-solvers old-solvers)))
                (set-step-unresolved-deps!
                 step
                 (wt-tree/add (step-unresolved-deps step) dep new-solvers))
                new-solvers)))
        (else
         #f)))

(define (step-unresolved-deps-min step)
  (let* ((num.dep (wt-tree/min (step-unresolved-deps-by-num-solvers step)))
         (dep (cdr num.dep))
         (solvers (step-unresolved-deps-ref step dep)))
    (unless solvers
      (internal-error "Step " (step-num step) " contains the dependency "
                      (dsp-dependency dep)
                      " in the list of unresolved dependencies by number"
                      " of solvers, but not in the main list of unresolved"
                      "  dependencies."))
    (values dep solvers)))

(define (step-add-clone! step clone-num)
  (set-step-clones! (wt-tree/add (step-clones step) clone-num #t)))

(define (step-add-forbidden-version! step version choice)
  (set-step-forbidden-versions! step (wt-tree/add (step-forbidden-versions step)
                                                  version
                                                  choice)))

(define (step-add-action! step choice)
  (define (drop-dep-solved-by-choice! c)
    (choice-table-delete! (step-deps-solved-by-choice step) c))
  (set-step-actions! step (choice-set-insert-or-narrow (step-actions step) choice))
  (let ((general-choice (generalize-choice choice)))
    (log/trace "Dropping dependencies in step " (step-num step)
               " that are solved by " (dsp-choice general-choice))
    (choice-table-visit
     (step-deps-solved-by-choice step)
     general-choice
     (lambda (dep-choice deps)
       (log/trace "Removing dependencies solved by " (dsp-choice dep-choice))
       (loop ((for dep (in-list deps)))
         (cond ((wt-tree/lookup (step-unresolved-deps step) dep #f)
                => (lambda (solvers)
                     (log/trace "Removing the dependency " (dsp-dependency dep)
                                " with a solver set of "
                                (dsp-solver-tracker-solvers solvers))
                     (set-step-unresolved-deps-by-num-solvers!
                      step
                      (wt-tree/delete (step-unresolved-deps-by-num-solvers step)
                                      (cons (solver-tracker-size solvers) dep)))))
               (else
                (log/trace "The dependency " (dsp-dependency dep)
                           " has not solver set, assuming it was already solved.")))
         (set-step-unresolved-deps! step
                                    (wt-tree/delete (step-unresolved-deps step)
                                                    dep)))
       (drop-dep-solved-by-choice! dep-choice)
       #f ; don't stop
       ))
    (log/trace "Done dropping dependencies in step " (step-num step)
               " that are solved by " (dsp-choice general-choice)))
  (drop-dep-solved-by-choice! choice))

(define (step-add-dep-solved-by-choice! step choice dep)
  (choice-table-update! (step-deps-solved-by-choice step)
                        choice
                        (lambda (deps-solved)
                          (cons dep deps-solved))
                        '()))

(define (dsp-step step)
  (cat "(" (choice-set-size (step-actions step)) " actions): "
       (dsp-choice-set (step-actions step)) ";T" (dsp-tier (step-tier step))
       "S" (step-score step)))


;;; Search graph

(define-record-type* search-graph
  (make-search-graph promotions)
  ((max-propagated-promotions 0)
   (steps (make-xvector))
   (steps-pending-promotion-propagation (make-wt-tree number-wt-type:>))
   (steps-related-to-choices (make-choice-table))))

(define (search-graph-step graph n)
  (xvector-ref (search-graph-steps graph) n))

(define (search-graph-last-step graph)
  (let ((steps (search-graph-steps graph)))
    (xvector-ref steps (- (xvector-length steps) 1))))

(define (%search-graph-add-step! graph step)
  (let ((steps (search-graph-steps graph)))
    (set-step-num! step (xvector-length steps))
    (xvector-push! steps step)
    step))

(define (search-graph-add-step! graph parent tier reason is-deferred-listener)
  (unless (step-first-child parent)
    (set-step-first-child! parent (xvector-length (search-graph-steps graph))))
  (%search-graph-add-step!
   graph
   (make-step (step-actions parent)
              (step-action-score parent)
              (step-score parent)
              parent
              tier
              reason
              is-deferred-listener
              (step-unresolved-deps parent)
              (step-unresolved-deps-by-num-solvers parent)
              (choice-table-copy
               (step-deps-solved-by-choice parent))
              (step-forbidden-versions parent))))

(define (search-graph-add-root-step! graph score)
  (let ((unresolved-deps (make-wt-tree dependency-wt-type))
        (unresolved-deps-by-num-solvers (make-wt-tree integer.dependency-wt-type))
        (deps-solved-by-choice (make-choice-table))
        (forbidden-versions (make-wt-tree version-wt-type)))
    (%search-graph-add-step! graph
                             (make-step (make-choice-set)
                                        0
                                        score
                                        #f
                                        minimum-tier
                                        #f
                                        #f
                                        unresolved-deps
                                        unresolved-deps-by-num-solvers
                                        deps-solved-by-choice
                                        forbidden-versions))))

(define (search-graph-add-clone! graph canonical-num clone-num)
  (let ((canonical-step (search-graph-step graph canonical-num))
        (clone-step (search-graph-step graph clone-num)))
    (cond
      ((= canonical-num (step-canonical-clone clone-step))
       (log/trace "Not marking step " clone-num
                  " as a clone of step " canonical-num
                  " since it is already listed as a clone."))
      (else
       (assert (not (= canonical-num clone-num)))
       (assert (not (step-canonical-clone clone-step)))
       (assert (wt-tree/empty? (step-clones clone-step)))

       (step-add-clone! canonical-step clone-num)
       (set-step-canonical-clone! clone-step canonical-num)
       (let ((clone-parent-num (step-parent clone-step)))
         (when (and (not (wt-tree/empty? (step-promotions canonical-step)))
                    clone-parent-num)
           (log/trace "The canonical step " canonical-num
                      " has some promotions, so scheduling the parent (step "
                      clone-parent-num ") of the clone (step "
                      clone-num ") for backpropagation.")
           (search-graph-add-pending-promotion-propagation! graph clone-parent-num)))))))

(define (search-graph-remove-choice! graph choice step-num reason)
  (log/trace "Marking the choice " (dsp-choice choice)
             " as not present in step " step-num)
  (let ((steps-related-to-choices (search-graph-steps-related-to-choices graph)))
    (and-let* ((steps (choice-table-ref steps-related-to-choices choice #f))
               (dep-steps (wt-tree/lookup steps reason #f)))
      (let* ((new-dep-steps (wt-tree/delete dep-steps step-num))
             (new-steps (if (wt-tree/empty? new-dep-steps)
                            (wt-tree/delete steps reason)
                            (wt-tree/add steps reason new-dep-steps))))
        (choice-table-set! steps-related-to-choices
                           choice
                           new-steps)))))

(define (search-graph-bind-choice! graph choice step-num reason)
  (log/trace "Marking the choice " (dsp-choice choice)
             " as present in step " step-num
             " with dependency " (dsp-dependency reason))
  (let* ((steps-related-to-choices (search-graph-steps-related-to-choices graph))
         (steps (or (choice-table-ref steps-related-to-choices choice #f)
                    (make-wt-tree dependency-wt-type)))
         (new-dep-steps (wt-tree/add (wt-tree/lookup steps
                                                     reason
                                                     (make-wt-tree number-wt-type))
                                     step-num
                                     #t)))
    (choice-table-set! steps-related-to-choices
                       choice
                       (wt-tree/add steps reason new-dep-steps))))

(define (search-graph-schedule-promotion-propagation! graph step-num promotion)
  (define p (dsp-promotion promotion))
  (let* ((target-step (search-graph-step graph step-num))
         (target-promotions (step-promotions target-step)))
    (define (schedule-clone-parents)
      (unless (wt-tree/empty? (step-clones target-step))
        (log/trace "Also scheduling the parents of the clones of step " step-num
                   "for propagation."))
      (wt-tree/for-each
       (lambda (clone-num true)
         (let* ((clone (search-graph-step clone-num))
                (parent-num (step-parent clone)))
           (when (not parent-num)
             (internal-error "Clone (step " clone-num ") has no parent!"))
           (log/trace "Scheduling the parent (step " parent-num
                      ") of a clone (step " clone-num ") of step " step-num
                      " for propagation.")
           (schedule-propagation parent-num)))
       (step-clones target-step))
      (log/trace "Done scheduling the clones of step " step-num " for propagation."))
    (define (schedule-propagation step-num)
      (search-graph-add-pending-promotion-propagation! graph step-num))
    (cond ((step-canonical-clone target-step)
           => (lambda (canonical-num)
                (log/trace "Adding the promotion " p " to step " canonical-num
                           " instead of to its clone, step" step-num ".")
                (search-graph-schedule-promotion-propagation! canonical-num
                                                              promotion)))
          ((= (wt-tree/size target-promotions)
              (search-graph-max-propagated-promotions graph))
           (log/trace "Not adding the promotion " p " to step " step-num
                      " since that step has the maximum number of promotions already."))
          ((wt-tree/lookup target-promotions promotion #f)
           (schedule-clone-parents))
          (else
           (wt-tree/add! target-promotions promotion #t)
           (step-promotions-list-push! target-step promotion)
           (cond ((step-parent target-step)
                  (log/trace "Adding a promotion to step " step-num
                             " and scheduling its parent, step " (step-parent target-step)
                             "for propagation: " p)
                  (schedule-propagation (step-parent target-step)))
                 (else
                  (log/trace "Adding a promotion to step " step-num
                             "; it has no parent, so not scheduling propagation: " p)))
           (schedule-clone-parents)))))

(define (search-graph-run-scheduled-promotion-propagations! graph promotion-adder)
  (define (maybe-collect-child-promotions step-num)
    (let ((parent-step (search-graph-step graph step-num)))
      (unless (step-first-child parent-step)
        (internal-error "No children at step " step-num
                        ", so no promotions to backpropagate."))
      (cond ((search-graph-add-child-promotions!
              graph
              step-num
              (step-first-child parent-step)
              #f
              (step-successor-constraints parent-step)
              maximum-tier
              promotion-adder)
             (let ((grandparent-num (step-parent parent-step)))
               (when grandparent-num
                 (log/trace "Scheduling step " grandparent-num
                            " for promotion propagation.")
                 (search-graph-add-pending-promotion-propagation! graph
                                                                  grandparent-num))))
            (else
             (log/trace "No new promotion at step " step-num)))))
  (loop ((until (wt-tree/empty?
                 (search-graph-steps-pending-promotion-propagation graph))))
    (wt-tree/for-each
     (lambda (step-num datum)
       (maybe-collect-child-promotions step-num))
     (search-graph-steps-pending-promotion-propagation graph))))


;; Private procedures

(define (search-graph-add-pending-promotion-propagation! graph step-num)
  (set-search-graph-steps-pending-promotion-propagation!
   graph
   (wt-tree/add (search-graph-steps-pending-promotion-propagation graph)
                step-num
                #t)))

(define (search-graph-add-child-promotions! graph
                                            parent-num
                                            child-num
                                            child-has-new-promotion?
                                            choices
                                            tier
                                            promotion-adder)
  (let* ((parent (search-graph-step graph parent-num))
         (canonical-parent-num (or (step-canonical-clone parent)
                                   parent-num))
         (canonical-parent (search-graph-step graph canonical-parent-num))
         (canonical-parent-promotions (search-graph-promotions-list graph parent)))
    (log/trace "Propagating promotions from the step " child-num
               " to its " (if (= canonical-parent-num parent-num)
                              "parent"
                              "parent's canonical clone")
               ", step " parent-num)
    (cond ((> (xvector-length canonical-parent-promotions)
              (search-graph-max-propagated-promotions graph))
           (log/trace "Not creating a new promotion: the parent already has "
                      "  too many promotions.")
           #f)
          (else
           (let* ((child (search-graph-step graph child-num))
                  (canonical-child-promotions (search-graph-promotions-list child))
                  (first-new-promotion-index
                   (step-promotions-list-first-new-promotion child)))
             (loop continue
                 ((for i (up-from (if (and (step-last-child? child)
                                           (not child-has-new-promotion?))
                                      first-new-promotion-index
                                      0)
                                  (to (xvector-length canonical-child-promotions))))
                  (while (< (xvector-length canonical-parent-promotions)
                            (search-graph-max-propagated-promotions graph)))
                  (with did-add? #f)
                  (let new-promotion? (>= i first-new-promotion-index))
                  (let promotion (xvector-ref canonical-child-promotions i)))
               => (begin
                    (set-step-promotions-list-first-new-promotion!
                     child
                     (xvector-length canonical-child-promotions))
                    did-add?)
               (log/trace "Using the successor link of step " child-num
                          ", " (dsp-choice (step-reason child))
                          ", to backpropagate the promotion " (dsp-promotion promotion)
                          " and add it to the current choice set"
                          (dsp-choice-set choices))
               (let ((new-choices
                      (choice-set-merge choices (choice-set-remove-overlaps
                                                 choices
                                                 (step-reason child))))
                     (new-tier (if (tier<? (promotion-tier promotion) tier)
                                   (promotion-tier promotion)
                                   tier)))
                 (cond ((tier>=? (step-tier canonical-parent) new-tier)
                        (log/trace "Not backpropagating this promotion: its tier, "
                                   (dsp-tier new-tier) " is not above the tier "
                                   " of step " canonical-parent-num ", "
                                   (step-tier canonical-parent)))
                       ((step-last-child? child)
                        (let ((new-promotion (make-promotion new-choices new-tier)))
                          (promotion-adder canonical-parent-num new-promotion)
                          (log/trace "New backpropagated promotion at step "
                                     canonical-parent-num
                                     ": " (dsp-promotion new-promotion))
                          (continue (=> did-add? #t))))
                       (else                        
                        (continue
                         (=> did-add?
                             (or did-add?
                                 (search-graph-add-child-promotions!
                                  graph
                                  parent-num
                                  (+ child-num 1)
                                  (or child-has-new-promotion? new-promotion?)
                                  new-choices
                                  new-tier)))))))))))))

(define (search-graph-promotions-list graph step)
  (step-promotions-list
   (cond ((step-canonical-clone step)
          => (lambda (clone-num)
               (search-graph-step clone-num)))
         (else
          step))))


;;; Promotion queue

(define-record-type* promotion-queue
  (make-promotion-queue index action-sum)
  ((promotion #f)
   (next #f)))

(define (promotion-queue-push! queue promotion)
  (assert (not (promotion-queue-promotion queue)))
  (set-promotion-queue-promotion! queue promotion)
  (set-promotion-queue-next!
   queue
   (make-promotion-queue (+ 1 (promotion-queue-index queue))
                         (+ (choice-set-size (promotion-choices promotion))
                            (promotion-queue-action-sum queue)))))

(define-syntax in-promotion-queue
  (syntax-rules ()
    ((_ (entry-var) (queue-expr) cont . env)
     (cont
      ()                               ;Outer bindings
      ((entry-var                      ;Loop variables
        queue-expr
        (promotion-queue-next entry-var)))
      ()                               ;Entry bindings
      ((or (not entry-var)             ;Termination conditions
           (not (promotion-queue-promotion entry-var))))
      ()                               ;Body bindings
      ()                               ;Final bindings
      . env))))


;;; Dependency solver tracker

(define-record-type* solver-tracker
  (%make-solver-tracker solvers structural-reasons)
  ())

(define (make-memoized-solver-tracker solvers structural-reasons)
  ;; Here would be the place to add memoizing to save memory
  (%make-solver-tracker solvers structural-reasons))

(define-record-type* solver-info
  (make-solver-info tier reasons tier-valid is-deferred-listener)
  ())

(define (make-solver-tracker solver-info-alist structural-reasons)
  (let ((solvers (make-xvector)))
    (loop ((for entry (in-list (list-sort (lambda (entry1 entry2)
                                            (choice<? (car entry1) (car entry2)))
                                          solver-info-alist))))
      (xvector-push! solvers entry))
    (make-memoized-solver-tracker solvers structural-reasons)))

(define (solver-tracker-size tracker)
  (xvector-length (solver-tracker-solvers tracker)))

(define (entry-by-choice-compare entry choice)
  (choice-compare (car entry) choice))

(define (solver-tracker-lookup tracker solver)
  (let ((solvers (solver-tracker-solvers tracker)))
    (xvector-binary-search solvers
                           solver
                           entry-by-choice-compare
                           (lambda (i entry)
                             (cdr entry)))))

(define (solver-tracker-update tracker choice info)
  (let* ((solvers (solver-tracker-solvers tracker))
         (new-solvers (make-xvector)))
    (receive (i j) (xvector-equal-range solvers entry-by-choice-compare)
      (loop ((for k (up-from 0 (to i))))
        (xvector-push! new-solvers (xvector-ref solvers k)))
      (xvector-push! new-solvers (cons choice info))
      (loop ((for k (up-from j (to (xvector-length solvers)))))
        (xvector-push! new-solvers (xvector-ref solvers k))))
    (make-memoized-solver-tracker new-solvers
                                  (solver-tracker-structural-reasons tracker))))

(define (solver-tracker-remove tracker solver reasons)
  (let ((new-reasons (cond ((pair? reasons)
                            reasons)
                           ((choice-set? reasons)
                            (choice-set->list reasons))
                           (else
                            (assertion-violation
                             'solver-tracker-remove
                             "invalid type for reasons" reasons)))))
    (make-memoized-solver-tracker
     (xvector-remove (solver-tracker-solvers tracker)
                     solver
                     (=? entry-by-choice-compare))
     (append new-reasons (solver-tracker-structural-reasons tracker)))))

(define (solver-tracker-for-each proc tracker)
  (loop ((for solver.info (in-xvector (solver-tracker-solvers tracker))))
    (proc (car solver.info) (cdr solver.info))))

(define (solver-tracker-fold proc seed tracker)
  (loop ((for solver.info (in-xvector (solver-tracker-solvers tracker)))
         (with seed seed (proc (car solver.info) (cdr solver.info) seed)))
    => seed))

(define-syntax in-solver-tracker
  (syntax-rules ()
    ((_ (solver-var info-var) (solver-tracker-expr) cont . env)
     (cont
      (((solvers size)                          ;Outer bindings
        (let ((solvers (solver-tracker-solvers solver-tracker-expr)))
          (values solvers (xvector-length solvers)))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((solver-var info-var)                    ;Body bindings
        (car+cdr (xvector-ref solvers index))))
      ()                                         ;Final bindings
      . env))
    ((_ (solver-var) (solver-tracker-expr) cont . env)
     (in-solver-tracker (solver-var dummy-info-var)
                        (solver-tracker-expr)
                        cont . env))))

(define (dsp-solver-entry entry)
  (cat (dsp-choice (car entry)) " -> " (dsp-solver-info (cdr entry))))

(define (dsp-solver-info info)
  (let ((tier-valid (solver-info-tier-valid info))
        (deferred-listener (solver-info-is-deferred-listener info)))
    (cat "(" (dsp-tier (solver-info-tier info))
         ":" (dsp-choice-set (solver-info-reasons info))
         (if tier-valid
             (cat ", V: " (expression/dsp tier-valid))
             fmt-null)
         (if deferred-listener
             (cat ", L: " (expression/dsp deferred-listener))
             fmt-null)
         ")")))

(define (dsp-solver-entries solver-vec)
  (cat "{" (fmt-join/xvector dsp-solver-entry solver-vec " ") "}"))

;; This outputs the structural reasons in reverse order, to match
;; aptitude's corresponding output; the order of structural reasons
;; should not matter otherwise, AFAICS.
(define (dsp-solver-tracker tracker)
  (cat "(" "[" (fmt-join dsp-choice
                         (reverse (solver-tracker-structural-reasons tracker))
                         ", ")
       "]: " (dsp-solver-tracker-solvers tracker) ")"))

(define (dsp-solver-tracker-solvers tracker)
  (dsp-solver-entries (solver-tracker-solvers tracker)))



;;; Auxiliaries

(define number-wt-type:> (make-wt-tree-type >))

(define integer.dependency-wt-type
  (make-wt-tree-type
   (lambda (pair1 pair2)
     (let ((prio1 (car pair1))
           (prio2 (car pair2)))
       (or (< prio1 prio2)
           (and (= prio1 prio2)
                (dependency<? (cdr pair1) (cdr pair2))))))))

(define (xvector-binary-search vec value cmp k)
  (loop continue ((with start 0)
                  (with end (xvector-length vec))
                  (let i (div (+ start end) 2))
                  (with j #f i))
    (if (or (= start end) (and j (= i j)))
        #f
        (let ((elt (xvector-ref vec i)))
          (if3 (cmp elt value)
               (continue (=> start i))
               (k i elt)
               (continue (=> end i)))))))

(define (xvector-equal-range vec value cmp k)
  (define (lower-bound start end)
    (loop continue ((with start start)
                    (with end end)
                    (let i (div (+ start end) 2)))
      (if (= start end)
          start
          (if<? (cmp (xvector-ref vec i) value)
                (continue (=> start (+ i 1)))
                (continue (=> end i))))))
  (define (upper-bound start end)
    (loop continue ((with start start)
                    (with end end)
                    (let i (div (+ start end) 2)))
      (if (= start end)
          start
          (if>? (cmp (xvector-ref vec i) value)
                (continue (=> end i))
                (continue (=> start (+ i 1)))))))
  (loop continue ((with left 0)
                  (with right (xvector-length vec))
                  (let i (div (+ left right) 2)))
    (if (= left right)
        (k left right)
        (if3 (cmp (xvector-ref vec i) value)
             (continue (=> left (+ i 1)))
             (k (lower-bound left i)
                (upper-bound i right))
             (continue (=> right i))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
