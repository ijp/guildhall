;;; -*- Mode: Scheme -*-

;;;; Extensible Looping Macros, version 9 (BETA)

;;; Copyright (c) 2008, 2011, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This is a variation on Alex Shinn's looping macros described in
;;; message-id <1157562097.001179.11470@i42g2000cwa.googlegroups.com>.
;;; It has diverged substantially from the original macros, and is now
;;; documented at <http://mumble.net/~campbell/tmp/foof-loop.txt> [for
;;; the beta period of foof-loop version 9].
;;;
;;; This file depends on syn-param.scm, also by Taylor R. Campbell, and
;;; SRFI 11 (LET-VALUES).  Ideally, the implementation of LET-VALUES
;;; should gracefully handle single-value clauses to elide superfluous
;;; uses of CALL-WITH-VALUES.

(define-syntax loop
  (syntax-rules ()
    ((loop ((loop-clause0 loop-clause1+ ...) ...)
       body
       ...)
     (loop anonymous-loop ((loop-clause0 loop-clause1+ ...) ...)
       body
       ...
       (anonymous-loop)))

    ((loop name ((loop-clause0 loop-clause1+ ...) ...) body ...)
     (syntactic-error-if-not-name name ("Malformed loop name:" name)
       (%loop start name ((loop-clause0 loop-clause1+ ...) ...) (body ...))))))

;;; We must be very careful about where to add laziness annotations.
;;; In particular, we don't want to wrap only the loop's body, because
;;; if we did that, the outer bindings produced by the iterators would
;;; be evaluate eagerly, which is too soon.  So instead, we wrap the
;;; whole thing in a LAZY, and then wrap every call to the loop as
;;; well.

(define-syntax lazy-loop
  (syntax-rules (=>)
    ((lazy-loop name (loop-clause ...) => result body0 body1+ ...)
     (syntactic-error-if-not-name name ("Invalid lazy loop name:" name)
       (lazy (loop eager-loop (loop-clause ...)
               => result
               (let-syntax ((name
                             (syntax-rules ()
                               ((name . arguments)
                                (lazy (eager-loop . arguments))))))
                 body0 body1+ ...)))))))

;;;; Error Reporting

;;; Use this definition of SYNTACTIC-ERROR if your favourite Scheme
;;; doesn't have one already.  Note that this is distinct from a
;;; SYNTAX-ERROR procedure, since it must signal a compile-time error.

(define-syntax syntactic-error (syntax-rules ()))

(define-syntax syntactic-name?
  (syntax-rules ()
    ((syntactic-name? (a . d) if-yes if-no) if-no)
    ((syntactic-name? #(v ...) if-yes if-no) if-no)
    ((syntactic-name? datum if-yes if-no)
     (let-syntax ((test-ellipsis
                   (syntax-rules ()
                     ((test-ellipsis (variable datum) yes no) yes)
                     ((test-ellipsis otherwise yes no) no))))
       (test-ellipsis (magical mystery list)
                      if-yes
                      (let-syntax ((test-name
                                    (syntax-rules ()
                                      ((test-name datum yes no) yes)
                                      ((test-name otherwise yes no) no))))
                        (test-name magical-mystery-symbol if-yes if-no)))))))

(define-syntax syntactic-ellipsis?
  (syntax-rules ()
    ((syntactic-ellipsis? (a . d) if-yes if-no) if-no)
    ((syntactic-ellipsis? #(v ...) if-yes if-no) if-no)
    ((syntactic-ellipsis? datum if-yes if-no)
     (let-syntax ((test-ellipsis
                   (syntax-rules ()
                     ((test-ellipsis (variable datum) yes no) yes)
                     ((test-ellipsis otherwise yes no) no))))
       (test-ellipsis (magical mystery list) if-yes if-no)))))

(define-syntax syntactic-error-if-not-name
  (syntax-rules ()
    ((syntactic-error-if-not-name name (message irritant ...) if-ok)
     (syntactic-name? name
       if-ok
       (syntactic-error message irritant ...)))))

(define-syntax syntactic-error-if-not-names
  (syntax-rules ()
    ((syntactic-error-if-not-names () (message irritant ...) if-ok)
     if-ok)
    ((syntactic-error-if-not-names (name0 name1+ ...) (message irritant ...)
       if-ok)
     (syntactic-error-if-not-name name0 (message irritant ...)
       (syntactic-error-if-not-names (name1+ ...) (message irritant ...)
         if-ok)))))

;;; Implement these if it is expedient in your Scheme system.

(define-syntax syntactic-error-if-not-bvl
  (syntax-rules ()
    ((syntactic-error-if-not-bvl bvl (message irritant ...) if-ok)
     if-ok)))

(define-syntax syntactic-error-if-not-bvls
  (syntax-rules ()
    ((syntactic-error-if-not-bvls (bvl ...) (message irritant ...) if-ok)
     if-ok)))

;;; Utilities for reporting syntax errors in LOOP clauses.

(define-syntax loop-clause-error
  (syntax-rules (context)

    ((loop-clause-error (context iterator (variable ...) arguments))
     (syntactic-error "Malformed loop clause:"
                      (for variable ... (iterator . arguments))))

    ;; old style.
    ((loop-clause-error (iterator (variable ...) arguments message))
     (syntactic-error message (for variable ... (iterator . arguments))))))

(define-syntax %loop-check
  (syntax-rules ()
    ((%loop-check syntactic-check operand
                  (context iterator (variable ...) arguments)
                  if-ok)
     (syntactic-check operand
         ("Malformed loop clause:" (for variable ... (iterator . arguments)))
       if-ok))

    ((%loop-check syntactic-check operand
                  (iterator (variable ...) arguments message)
                  if-ok)
     (syntactic-check operand
         (message (for variable ... (iterator . arguments)))
       if-ok))))

(define-syntax loop-clause-error-if-not-name
  (syntax-rules ()
    ((loop-clause-error-if-not-name name error-context if-ok)
     (%loop-check syntactic-error-if-not-name name error-context if-ok))))

(define-syntax loop-clause-error-if-not-names
  (syntax-rules ()
    ((loop-clause-error-if-not-names names error-context if-ok)
     (%loop-check syntactic-error-if-not-names names error-context if-ok))))

(define-syntax loop-clause-error-if-not-bvl
  (syntax-rules ()
    ((loop-clause-error-if-not-bvl bvl error-context if-ok)
     (%loop-check syntactic-error-if-not-bvl bvl error-context if-ok))))

(define-syntax loop-clause-error-if-not-bvls
  (syntax-rules ()
    ((loop-clause-error-if-not-bvls bvls error-context if-ok)
     (%loop-check syntactic-error-if-not-bvls bvls error-context if-ok))))

;;;; The Guts of LOOP

(define-syntax %loop
  (syntax-rules (=> for with let let-values while until
                    start go parse-for continue finish simplify-body)

    ((%loop start name loop-clauses body)
     (%loop go name (() () () () () () () ()) loop-clauses body))

    ;; simple case of a single variable, for clarity.
    ((%loop go name state
            ((for variable (iterator argument ...))
             . loop-clauses)
            body)
     (iterator (variable) (argument ...)
               %loop continue iterator name state loop-clauses body))

    ;; for handler with tail patterns.  unfortunately, tail patterns are non-
    ;; standard, so we need the next four clauses rather than this one...
    ;; 
    ;; ((%loop go name state
    ;;         ((for variable0 variable1+ ... (iterator argument ...))
    ;;          . loop-clauses)
    ;;         body)
    ;;  (iterator (variable0 variable1+ ...)
    ;;            (argument ...)
    ;;            %loop continue iterator name state loop-clauses body))

;;;;; for clauses: dealing with iterators

    ((%loop go name state
            ((for variable0 variable1 variable2+ ...) . loop-clauses)
            body)
     (%loop parse-for (variable0 variable1 variable2+ ...)
            ()
            (for variable0 variable1 variable2+ ...)  ;copy for error message.
            name state loop-clauses body))

    ((%loop parse-for ((iterator argument ...))
            variables
            original-clause name state loop-clauses body)
     (iterator variables (argument ...)
               %loop continue iterator name state loop-clauses body))

    ((%loop parse-for (next-variable more0 more1+ ...)
            (variable ...)
            original-clause name state loop-clauses body)
     (%loop parse-for (more0 more1+ ...)
            (variable ... next-variable)
            original-clause name state loop-clauses body))

    ((%loop parse-for (non-list)
            variables
            original-clause name state loop-clauses body)
     (syntactic-error "Malformed for clause in loop:" original-clause))

    ((%loop ((outer-bvl outer-producer) ...)
            ((loop-variable loop-initializer loop-stepper) ...)
            ((entry-bvl entry-producer) ...)
            (termination-condition ...)
            ((body-bvl body-producer) ...)
            ((final-bvl final-producer) ...)
            continue
            iterator
            name
            ((loop-variables ...)
             user-bindings
             user-termination-conditions
             outer-bindings
             entry-bindings
             termination-conditions
             body-bindings
             final-bindings)
            loop-clauses
            body)
     (syntactic-error-if-not-names (loop-variable ...)
         ("Internal error -- malformed loop variables from iterator:" iterator)
       (syntactic-error-if-not-bvls
           (outer-bvl ... entry-bvl ... body-bvl ... final-bvl ...)
           ("Internal error -- malformed bvls from iterator:" iterator)
         (%loop go name
                ((loop-variables ...    ;** preserve order.
                  (loop-variable loop-initializer loop-stepper) ...)
                 user-bindings
                 user-termination-conditions
                 ((outer-bvl outer-producer) ... . outer-bindings)
                 ((entry-bvl entry-producer) ... . entry-bindings)
                 (termination-condition ... . termination-conditions)
                 ((body-bvl body-producer) ... . body-bindings)
                 ((final-bvl final-producer) ... . final-bindings))
                loop-clauses
                body))))

;;;;; user-directed clauses

    ((%loop go name state
            ((with variable initializer) . loop-clauses)
            body)
     (syntactic-error-if-not-name variable
         ("Malformed with clause in loop:" (with variable initializer))
       (%loop go name state
              ((with variable initializer variable) . loop-clauses)
              body)))

    ((%loop go name
            ((loop-variable ...) . more-state)
            ((with variable initializer stepper) . loop-clauses)
            body)
     (syntactic-error-if-not-name variable
         ("Malformed with clause in loop:" (with variable initializer stepper))
       (%loop go name
              ;; preserve ordering of the user's loop variables.
              ((loop-variable ... (variable initializer stepper))
               . more-state)
              loop-clauses
              body)))

    ((%loop go name state ((let variable expression) . loop-clauses) body)
     (syntactic-error-if-not-name variable
         ("Malformed let clause in loop:" (let variable expression))
       (%loop go name state ((let-values (variable) expression) . loop-clauses)
              body)))

    ((%loop go name (loop-variables (user-binding ...) . more-state)
            ((let-values user-bvl user-producer) . loop-clauses)
            body)
     (syntactic-error-if-not-bvl user-bvl
         ("Malformed let-values clause in loop:"
          (let-values user-bvl user-producer))
       (%loop go name (loop-variables
                       ;; preserve order of the user's termination conditions.
                       (user-binding ... (user-bvl user-producer))
                       . more-state)
              loop-clauses
              body)))

;;;;;; user-directed clauses, continued

    ((%loop go name state ((while condition) . loop-clauses) body)
     (%loop go name state ((until (not condition)) . loop-clauses) body))

    ((%loop go name (loop-variables
                     user-bindings
                     (user-termination-condition ...)
                     . more-state)
            ((until user-termination-condition*) . loop-clauses)
            body)
     (%loop go name
            (loop-variables
             user-bindings
             (user-termination-condition ... user-termination-condition*)
             . more-state)
            loop-clauses
            body))

    ;; compatibility forms.  these clauses *must* come after all
    ;; others, because there is no keyword, so these would shadow any
    ;; clauses with keywords.

    ((%loop go name state ((variable initializer) . loop-clauses) body)
     (syntactic-error-if-not-name variable
         ("Malformed named-let-style clause in loop:" (variable initializer))
       (%loop go name state
              ((with variable initializer) . loop-clauses)
              body)))

    ((%loop go name state ((variable initializer stepper) . loop-clauses) body)
     (syntactic-error-if-not-name variable
         ("Malformed do-style clause in loop:" (variable initializer stepper))
       (%loop go name state
              ((with variable initializer stepper) . loop-clauses)
              body)))

    ((%loop go name state (clause . loop-clauses) body)
     (syntactic-error "Malformed loop clause:" clause))

;;;;; finishing -- generating output

    ((%loop go name state () (=> result-form . body))
     (%loop finish name state result-form body))

    ((%loop go name state () body)
     (%loop finish name state (if #f #f) body))

    ((%loop finish name
            (((loop-variable loop-initializer loop-stepper) ...)
             user-bindings
             user-termination-conditions
             outer-bindings
             entry-bindings
             termination-conditions
             body-bindings
             final-bindings)
            result-form
            body)
     (let-values outer-bindings
       (define (loop-procedure loop-variable ...)
         (let-values entry-bindings
           (%loop simplify-body
                  termination-conditions
                  (let-values final-bindings
                    (with-extended-parameter-operators
                        ((name
                          (loop-procedure (loop-variable . loop-stepper)
                                          ...)))
                      result-form))
                  body-bindings
                  user-bindings
                  user-termination-conditions
                  (with-extended-parameter-operators
                      ((name
                        (loop-procedure (loop-variable . loop-stepper)
                                        ...)))
                    . body))))
       (loop-procedure loop-initializer ...)))

;;;;;; simplifying the body

    ;; no iterator- or user-introduced termination conditions at all.
    ;; no test or closure needed.
    ((%loop simplify-body
            ()
            final-form
            body-bindings
            user-bindings
            ()
            body-form)
     (let-values body-bindings
       (let-values user-bindings
         body-form)))

    ;; iterator-introduced termination conditions only.  one test and
    ;; no closure needed.
    ((%loop simplify-body
            (termination-condition ...)
            final-form
            body-bindings
            user-bindings
            ()                          ;no user termination conditions
            body-form)
     (if (or termination-condition ...)
         final-form
         (let-values body-bindings
           (let-values user-bindings
             body-form))))

    ;; the closure is needed here because the body bindings shouldn't
    ;; be visible in the final form.
    ((%loop simplify-body
            ()
            final-form
            body-bindings
            user-bindings
            (user-termination-condition ...)
            body-form)
     (let ((finish (lambda () final-form)))
       (let-values body-bindings
         (let-values user-bindings
           (if (or user-termination-condition ...)
               (finish)
               body-form)))))

    ((%loop simplify-body
            (termination-condition ...)
            final-form
            body-bindings
            user-bindings
            (user-termination-condition ...)
            body-form)
     (let ((finish (lambda () final-form)))
       (if (or termination-condition ...)
           (finish)
           (let-values body-bindings
             (let-values user-bindings
               (if (or user-termination-condition ...)
                   (finish)
                   body-form))))))))

;;;; Accumulators

;;; Accumulators have the following syntax:
;;;
;;;   (FOR <result> (ACCUMULATING <generator>))
;;;   (FOR <result> (ACCUMULATING <generator> (IF <condition>)))
;;;   (FOR <result> (ACCUMULATING <generator> => <mapper>))    ;COND-style
;;;   (FOR <result> (ACCUMULATING <generator> <tester>         ;SRFI-61-style
;;;                               => <mapper>))
;;;
;;; In addition, some of them support initial values, which are
;;; specified with an optional first argument of (INITIAL <initial
;;; value>).  For example, to accumulate a list starting with some tail
;;; <tail>, write
;;;
;;;   (FOR <result-list> (LISTING (INITIAL <tail>) <element>)).

(define-syntax listing
  (syntax-rules (initial)
    ((listing variables ((initial tail-expression) . arguments) next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    ('() cons (lambda (result)
                                (append-reverse result tail)))
                    (context listing
                             variables
                             ((initial tail-expression) . arguments))
                    next . rest))

    ((listing variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() cons reverse)
                    (context listing variables arguments)
                    next . rest))))

(define-syntax listing-reverse
  (syntax-rules (initial)
    ((listing-reverse variables ((initial tail-expression) . arguments)
                      next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    (tail cons)
                    (context listing-reverse
                             variables
                             ((initial tail-expression) . arguments))
                    next . rest))

    ((listing-reverse variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() cons)
                    (context listing-reverse variables arguments)
                    next . rest))))

;;; This is non-reentrant but produces precisely one garbage cons cell.

(define-syntax listing!
  (syntax-rules ()
    ((listing! variables arguments next . rest)
     (%listing! variables arguments (cons #f '())
                (context listing! variables arguments)
                next . rest))))

(define-syntax listing-into!
  (syntax-rules ()
    ((listing-into! variables (first-expression . arguments) next . rest)
     (%listing! variables arguments first-expression
                (context listing-into!
                         variables
                         (first-expression . arguments))
                next . rest))))

(define-syntax %listing!
  (syntax-rules (initial)
    ((%listing! variables ((initial tail-expression) . arguments)
                first-expression
                error-context
                next . rest)
     (%accumulating variables arguments
                    (((first tail)
                      (let ((first first-expression)
                            (tail tail-expression))
                        (set-cdr! first tail)
                        (values first tail))))
                    (first (lambda (datum previous-cell)
                             (let ((next-cell (cons datum tail)))
                               (set-cdr! previous-cell next-cell)
                               next-cell))
                           (lambda (cell) cell (cdr first)))
                    error-context
                    next . rest))

    ((%listing! variables arguments first-expression error-context next . rest)
     (%listing! variables ((initial '()) . arguments)
                first-expression
                error-context
                next . rest))))

;;;;; List Appending Accumulators

(define-syntax appending
  (syntax-rules (initial)
    ((appending variables ((initial tail-expression) . arguments)
                next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    ('() append-reverse (lambda (result)
                                          (append-reverse result tail)))
                    (context appending
                             variables
                             ((initial tail-expression) . arguments))
                    next . rest))

    ((appending variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() append-reverse reverse)
                    (context appending variables arguments)
                    next . rest))))

(define-syntax appending-reverse
  (syntax-rules (initial)
    ((appending-reverse variables ((initial tail-expression) . arguments)
                        next . rest)
     (%accumulating variables arguments (((tail) tail-expression))
                    (tail append-reverse)
                    (context appending-reverse
                             variables
                             ((initial tail-expression) . arguments))
                    next . rest))

    ((appending-reverse variables arguments next . rest)
     (%accumulating variables arguments ()
                    ('() append-reverse)
                    (context appending-reverse variables arguments)
                    next . rest))))

;; (define (append-reverse list tail)
;;   (loop ((FOR elt (IN-LIST list))
;;          (FOR result (LISTING-REVERSE (INITIAL tail) elt)))
;;     => result))

(define (append-reverse list tail)
  (if (pair? list)
      (append-reverse (cdr list) (cons (car list) tail))
      tail))

;;;;; Numerical Accumulators

(define-syntax summing
  (syntax-rules (initial)
    ((summing variables ((initial initial-expression) . arguments) next . rest)
     (%accumulating variables arguments () (initial-expression +)
                    (context summing
                             variables
                             ((initial initial-expression) . arguments))
                    next . rest))

    ((summing variables arguments next . rest)
     (%accumulating variables arguments () (0 +)
                    (context summing variables arguments)
                    next . rest))))

(define-syntax multiplying
  (syntax-rules (initial)
    ((multiplying variables ((initial initial-expression) . arguments)
                  next . rest)
     (%accumulating variables arguments () (initial-expression *)
                    (context multiplying
                             variables
                             ((initial initial-expression) . arguments))
                    next . rest))

    ((multiplying variables arguments next . rest)
     (%accumulating variables arguments () (1 *)
                    (context multiplying variables arguments)
                    next . rest))))

(define-syntax maximizing
  (syntax-rules ()
    ((maximizing variables arguments next . rest)
     (%extremizing variables arguments max
                   (context maximizing variables arguments)
                   next . rest))))

(define-syntax minimizing
  (syntax-rules ()
    ((minimizing variables arguments next . rest)
     (%extremizing variables arguments min
                   (context minimizing variables arguments)
                   next . rest))))

(define-syntax %extremizing
  (syntax-rules (initial)
    ((%extremizing variables ((initial initial-expression) . arguments)
                   chooser
                   error-context next . rest)
     (%accumulating variables arguments (((initial-value) initial-expression))
                    (initial-value chooser)
                    error-context next . rest))

    ((%extremizing variables arguments chooser error-context next . rest)
     (%accumulating variables arguments ()
                    (#f (lambda (datum extreme)
                          (if (and datum extreme)
                              (chooser datum extreme)
                              (or datum extreme))))
                    error-context next . rest))))

(define-syntax %accumulating
  (syntax-rules ()

    ;; there is a finalization step, so the result variable cannot be
    ;; the accumulator variable, and we must apply the finalizer at the
    ;; end.
    ((%accumulating (result-variable) arguments outer-bindings
                    (initializer combiner finalizer)
                    error-context
                    next . rest)
     (loop-clause-error-if-not-name result-variable error-context
       (%%accumulating arguments (accumulator initializer combiner)
                       outer-bindings
                       (((result-variable) (finalizer accumulator)))
                       error-context
                       next . rest)))

    ;; there is no finalizer step, so the accumulation is incremental,
    ;; and can be exploited; therefore, the result variable and the
    ;; accumulator variable are one and the same.
    ((%accumulating (accumulator-variable) arguments outer-bindings
                    (initializer combiner)
                    error-context
                    next . rest)
     (loop-clause-error-if-not-name accumulator-variable error-context
       (%%accumulating arguments (accumulator-variable initializer combiner)
                       outer-bindings
                       ()
                       error-context
                       next . rest)))

    ;; the user supplied more than one variable.  lose lose.
    ((%accumulating variables arguments outer-bindings parameters
                    error-context next . rest)
     (loop-clause-error error-context))))

(define-syntax %%%accumulating
  (syntax-rules ()
    ((%%%accumulating outer-bindings loop-variable final-bindings next . rest)
     (next outer-bindings
           (loop-variable)
           ()                           ;entry bindings
           ()                           ;termination conditions
           ()                           ;body bindings
           final-bindings
           . rest))))

(define-syntax %%accumulating
  (syntax-rules (if =>)
    ((%%accumulating (generator)        ;no conditional
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;loop variable
                                   (combiner generator accumulator))
                      final-bindings next . rest))

    ((%%accumulating (generator (if condition))
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;loop variable
                                   (if condition
                                       (combiner generator accumulator)
                                       accumulator))
                      final-bindings next . rest))

    ((%%accumulating (generator => mapper)
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;loop variable
                                   (cond (generator
                                          => (lambda (datum)
                                               (combiner (mapper datum)
                                                         accumulator)))
                                         (else accumulator)))
                      final-bindings next . rest))

    ((%%accumulating (generator tester => mapper)
                     (accumulator initializer combiner)
                     outer-bindings final-bindings error-context next . rest)
     (%%%accumulating outer-bindings
                      (accumulator initializer       ;loop variable
                                   (receive args generator
                                     (if (apply tester args)
                                         (combiner (apply mapper args)
                                                   accumulator)
                                         accumulator)))
                      final-bindings next . rest))

    ((%%accumulating arguments parameters outer-bindings final-bindings
                     error-context next . rest)
     (loop-clause-error error-context))))

;;;; List Iteration

;;; (FOR <elt> [<pair>] (IN-LIST <list> [<successor>]))
;;;   Step across <list>, letting <pair> be each successive pair in
;;;   <list>, stepping by (<successor> <pair>), or (CDR <pair>) if no
;;;   successor procedure is explicitly provided.  Let <elt> be the car
;;;   of <pair> in the body of the loop.

(define-syntax in-list
  (syntax-rules ()
    ((in-list (element-variable pair-variable)
              (list-expression successor-expression)
              next . rest)
     (loop-clause-error-if-not-names (element-variable pair-variable)
         (context in-list
                  (element-variable pair-variable)
                  (list-expression successor-expression))
       (next (((list) list-expression)                  ;outer bindings
              ((successor) successor-expression))
             ((pair-variable list tail))                ;loop variables
             ()                                         ;entry bindings
             ((not (pair? pair-variable)))              ;termination conditions
             (((element-variable) (car pair-variable))  ;body bindings
              ((tail)             (successor pair-variable)))
             ()                                         ;final bindings
             . rest)))

    ((in-list (element-variable pair-variable) (list-expression) next . rest)
     (loop-clause-error-if-not-names (element-variable pair-variable)
         (context in-list (element-variable pair-variable) (list-expression))
       ;++ this is silly, but it will improve performance in scheme
       ;++ systems that don't beta-reduce (let ((x cdr)) ...).
       (next (((list) list-expression))                 ;outer bindings
             ((pair-variable list tail))                ;loop variables
             ()                                         ;entry bindings
             ((not (pair? pair-variable)))              ;termination conditions
             (((element-variable) (car pair-variable))  ;body bindings
              ((tail)             (cdr pair-variable)))
             ()                                         ;final bindings
             . rest)))

    ((in-list (element-variable) (list-expression successor) next . rest)
     (loop-clause-error-if-not-name element-variable
         (context in-list (element-variable) (list-expression successor))
       (in-list (element-variable pair)
                (list-expression successor)
                next . rest)))

    ((in-list (element-variable) (list-expression) next . rest)
     (loop-clause-error-if-not-name element-variable
         (context in-list (element-variable) (list-expression))
       (in-list (element-variable pair) (list-expression) next . rest)))

    ((in-list variables arguments next . rest)
     (loop-clause-error (context in-list variables arguments)))))

;;;;; Parallel List Iteration

(define-syntax in-lists
  (syntax-rules ()
    ((in-lists (elements-variable pairs-variable)
               (lists-expression tail-expression)
               next . rest)
     (loop-clause-error-if-not-names (element-variable pair-variable)
         (context in-lists
                  (elements-variable pairs-variable)
                  (lists-expression tail-expression))
       (next (((lists) lists-expression))       ;outer bindings
             ((pairs-variable lists cdrs))      ;loop variables
             (((lose? cars cdrs)                ;entry bindings
               (%cars&cdrs pairs-variable tail-expression '())))
             (lose?)                            ;termination conditions
             (((elements-variable) cars))       ;body bindings
             ()                                 ;final bindings
             . rest)))

    ((in-lists (elements-variable pairs-variable) (lists) next . rest)
     (loop-clause-error-if-not-names (elements-variable pair-variable)
         (context in-lists (elements-variable pairs-variable) (lists))
       (in-lists (elements-variable pairs-variable) (lists '()) next . rest)))

    ((in-lists (elements-variable) (lists tail) next . rest)
     (loop-clause-error-if-not-name elements-variable
         (context in-lists (elements-variable) (lists tail))
       (in-lists (elements-variable pairs) (lists tail) next . rest)))

    ((in-lists (elements-variable) (lists) next . rest)
     (loop-clause-error-if-not-name elements-variable
         (context in-lists (elements-variable) (lists))
       (in-lists (elements-variable pairs) (lists '()) next . rest)))

    ((in-lists variables arguments next . rest)
     (loop-clause-error (context in-lists variables arguments)))))

(define (%cars&cdrs lists cars-tail cdrs-tail)
  (loop proceed ((for list (in-list lists))
                 (for cars (listing (initial cars-tail) (car list)))
                 (for cdrs (listing (initial cdrs-tail) (cdr list))))
    => (values #f cars cdrs)
    (if (pair? list)
        (proceed)
        (values #t #f #f))))

;;;; Vector and String Iteration

;;; (FOR <elt> [<index>] (IN-VECTOR <vector> [<start> [<end>]]))
;;;
;;; IN-VECTOR-REVERSE, IN-STRING, and IN-STRING-REVERSE all have the
;;; same syntax.
;;;
;;; The reverse iterators run from end to start; the bounds are still
;;; given in the same order as the forward iterators.

(define-syntax in-vector
  (syntax-rules ()
    ((in-vector variables (vector-expression start/end ...) next . rest)
     (%in-vector (forward vector-ref vector 0 (vector-length vector))
                 variables (vector-expression start/end ...)
                 (context in-vector
                          variables
                          (vector-expression start/end ...))
                 next . rest))))

(define-syntax in-vector-reverse
  (syntax-rules ()
    ((in-vector-reverse variables (vector-expression start/end ...)
                        next . rest)
     (%in-vector (backward vector-ref vector (vector-length vector) 0)
                 variables (vector-expression start/end ...)
                 (context in-vector-reverse
                          variables
                          (vector-expression start/end ...))
                 next . rest))))

(define-syntax in-string
  (syntax-rules ()
    ((in-string variables (vector-expression start/end ...) next . rest)
     (%in-vector (forward string-ref string 0 (string-length string))
                 variables (vector-expression start/end ...)
                 (context in-string
                          variables
                          (vector-expression start/end ...))
                 next . rest))))

(define-syntax in-string-reverse
  (syntax-rules ()
    ((in-string-reverse variables (string-expression start/end ...)
                        next . rest)
     (%in-vector (backward string-ref string (string-length string) 0)
                 variables (string-expression start/end ...)
                 (context in-string-reverse
                          variables
                          (string-expression start/end ...))
                 next . rest))))

;;;;; Random-Access Sequence Generalization

(define-syntax %in-vector
  (syntax-rules (forward backward)
    ((%in-vector (forward vector-ref vector-variable default-start default-end)
                 (element-variable index-variable)
                 (vector-expression start-expression end-expression)
                 error-context next . rest)
     (loop-clause-error-if-not-names (element-variable index-variable)
         error-context
       (next (((vector-variable start end)      ;outer bindings
               (let ((vector-variable vector-expression))
                 (values vector-variable start-expression end-expression))))
             ((index-variable start             ;loop variables
                              (+ index-variable 1)))
             ()                                 ;entry bindings
             ((>= index-variable end))          ;termination conditions
             (((element-variable)               ;body bindings
               (vector-ref vector-variable index-variable)))
             ()                                 ;final bindings
             . rest)))

    ((%in-vector (backward
                  vector-ref vector-variable default-start default-end)
                 (element-variable index-variable)
                 (vector-expression start-expression end-expression)
                 error-context next . rest)
     (loop-clause-error-if-not-names (element-variable index-variable)
         error-context
       (next (((vector-variable start end)      ;outer bindings
               (let ((vector-variable vector-expression))
                 (values vector-variable start-expression end-expression))))
             ((index-variable start             ;loop variables
                              index-variable))
             ()                                 ;entry bindings
             ((<= index-variable end))          ;termination conditions
             (((index-variable)                 ;body bindings
               (- index-variable 1))
              ((element-variable)
               (vector-ref vector-variable (- index-variable 1))))
             ()                                 ;final bindings
             . rest)))

;;;;;; %in-vector, continued

    ;; supply an index variable if absent.
    ((%in-vector iteration-parameters (element-variable) arguments
                 error-context next . rest)
     (loop-clause-error-if-not-name element-variable error-context
       (%in-vector iteration-parameters (element-variable index) arguments
                   error-context next . rest)))

    ;; supply the default start index if necessary.
    ((%in-vector (direction vector-ref variable default-start default-end)
                 variables (vector-expression)
                 error-context next . rest)
     (loop-clause-error-if-not-names variables error-context
       (%in-vector (direction vector-ref variable default-start default-end)
                   variables (vector-expression default-start)
                   error-context next . rest)))

    ;; supply the default end index if necessary.
    ((%in-vector (direction vector-ref variable default-start default-end)
                 variables (vector-expression start-expression)
                 error-context next . rest)
     (loop-clause-error-if-not-names variables error-context
       (%in-vector (direction vector-ref variable default-start default-end)
                   variables (vector-expression start-expression default-end)
                   error-context next . rest)))

    ((%in-vector iteration-parameters modified-variables modified-arguments
                 error-context next . rest)
     (loop-clause-error error-context))))

;;;; Input

;;; (FOR <item> (IN-PORT <input-port> [<reader> [<eof?>]]))
;;;
;;; IN-FILE has the same syntax, but with a pathname in the place of
;;; the input port.

(define-syntax in-port
  (syntax-rules ()
    ((in-port (datum-variable)
              (port-expression reader-expression eof-predicate)
              next . rest)
     (loop-clause-error-if-not-name datum-variable
         (context in-port
                  (datum-variable)
                  (port-expression reader-expression eof-predicate))
       (next (((port) port-expression)          ;outer bindings
              ((reader) reader-expression)
              ((eof?) eof-predicate))
             ()                                 ;loop variables
             (((datum-variable) (reader port))) ;entry bindings
             ((eof? datum-variable))            ;termination conditions
             ()                                 ;body bindings
             ()                                 ;final bindings
             . rest)))

    ;; supply a reader if absent.
    ((in-port (datum-variable) (port-expression) next . rest)
     (loop-clause-error-if-not-name datum-variable
         (context in-port (datum-variable) (port-expression))
       (in-port (datum-variable) (port-expression read-char) next . rest)))

    ;; supply an eof predicate if absent.
    ((in-port (datum-variable) (port-expression reader-expression) next . rest)
     (loop-clause-error-if-not-name datum-variable
         (context in-port (datum-variable) (port-expression reader-expression))
       (in-port (datum-variable)
                (port-expression reader-expression eof-object?)
                next . rest)))

    ((in-port variables arguments next . rest)
     (loop-clause-error (context in-port variables arguments)))))

(define-syntax in-file
  (syntax-rules ()
    ((in-file (datum-variable)
              (pathname-expression reader-expression eof-predicate)
              next . rest)
     (loop-clause-error-if-not-name datum-variable
         (context in-file
                  (datum-variable)
                  (pathname-expression reader-expression eof-predicate))
       (next (((port)                           ;outer bindings
               (open-input-file pathname-expression))
              ((reader) reader-expression)
              ((eof?) eof-predicate))
             ()                                 ;loop variables
             (((datum-variable) (reader port))) ;entry bindings
             ((eof? datum-variable))            ;termination conditions
             ()                                 ;body bindings
             ((()                               ;final bindings
               (begin (close-input-port port)
                      (values))))
             . rest)))

    ;; supply a reader if absent.
    ((in-file (datum-variable) (pathname-expression) next . rest)
     (loop-clause-error-if-not-name datum-variable
         (context in-file (datum-variable) (pathname-expression))
       (in-file (datum-variable) (pathname-expression read-char) next . rest)))

    ;; supply an eof predicate if absent.
    ((in-file (datum-variable)
              (pathname-expression reader-expression)
              next . rest)
     (loop-clause-error-if-not-name datum-varable
         (context in-file
                  (datum-variable)
                  (pathname-expression reader-expression))
       (in-file (datum-variable)
                (pathname-expression reader-expression eof-object?)
                next . rest)))

    ((in-file variables arguments next . rest)
     (loop-clause-error (context in-file variables arguments)))))

;;;; Iterating Up through Numbers

(define-syntax up-from
  (syntax-rules (to by)
    ((up-from (variable)
              (start-expression (to end-expression)
                                (by step-expression))
              next . rest)
     (loop-clause-error-if-not-name variable
         (context up-from
                  (variable)
                  (start-expression (to end-expression) (by step-expression)))
       (next (((start) start-expression)        ;outer bindings
              ((end) end-expression)
              ((step) step-expression))
             ((variable start                   ;loop variables
                        (+ variable step)))
             ()                                 ;entry bindings
             ((>= variable end))                ;termination conditions
             ()                                 ;body bindings
             ()                                 ;final bindings
             . rest)))

    ((up-from (variable)
              (start-expression (by step-expression))
              next . rest)
     (loop-clause-error-if-not-name variable
         (context up-from (variable) (start-expression (by step-expression)))
       (next (((start) start-expression)        ;outer bindings
              ((step) step-expression))
             ((variable start                   ;loop variables
                        (+ variable step)))
             ()                                 ;entry bindings
             ()                                 ;termination conditions
             ()                                 ;body bindings
             ()                                 ;final bindings
             . rest)))

    ;; add a default step of 1.
    ((up-from (variable)
              (start-expression (to end-expression))
              next . rest)
     (loop-clause-error-if-not-name variable
         (context up-from (variable) (start-expression (to end-expression)))
       (up-from (variable)
                (start-expression (to end-expression) (by 1))
                next . rest)))

    ((up-from (variable)
              (start-expression)
              next . rest)
     (loop-clause-error-if-not-name variable
         (context up-from (variable) (start-expression))
       (up-from (variable)
                (start-expression (by 1))
                next . rest)))

    ((up-from variables arguments next . rest)
     (loop-clause-error (context up-from variables arguments)))))

;;;; Iterating Down through Numbers

(define-syntax down-from
  (syntax-rules (to by)
    ((down-from (variable)
                (start-expression (to end-expression)
                                  (by step-expression))
                next . rest)
     (loop-clause-error-if-not-name variable
         (context down-from
                  (variable)
                  (start-expression (to end-expression) (by step-expression)))
       (next (((start) start-expression)        ;outer bindings
              ((end) end-expression)
              ((step) step-expression))
             ((variable start variable))        ;loop variables
             ()                                 ;entry bindings
             ((<= variable end))                ;termination conditions
             (((variable)                       ;body bindings
               (- variable step)))
             ()                                 ;final bindings
             . rest)))

    ((down-from (variable)
                (start-expression (by step-expression))
                next . rest)
     (loop-clause-error-if-not-name variable
         (context down-from (variable) (start-expression (by step-expression)))
       (next (((start) start-expression)        ;outer bindings
             ((step) step-expression))
            ((variable start variable))         ;loop variables
            (((variable) (- variable step)))    ;entry bindings
            ()                                  ;termination conditions
            ()                                  ;body bindings
            ()                                  ;final bindings
            . rest)))

    ;; add a default step of 1.
    ((down-from (variable)
                (start-expression (to end-expression))
                next . rest)
     (loop-clause-error-if-not-name variable
         (context down-from (variable) (start-expression (to end-expression)))
       (down-from (variable)
                  (start-expression (to end-expression) (by 1))
                  next . rest)))

    ((down-from (variable)
                (start-expression)
                next . rest)
     (loop-clause-error-if-not-name variable
         (context down-from (variable) (start-expression))
       (down-from (variable)
                  (start-expression (by 1))
                  next . rest)))

    ((down-from variables arguments next . rest)
     (loop-clause-error (context down-from variables arguments)))))
