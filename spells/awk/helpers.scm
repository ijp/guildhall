;;; helpers.sls --- The guts of the AWK macro

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (c) 1994 by Olin Shivers

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; An awk loop, after the design of David Albertz and Olin Shivers.

;; This implementation is based on the explicit-renaming awk loop
;; implementation in scsh, but does not go to the same lengths
;; wrt. optimizations. In particuliar, I left out the elimination of
;; the `else-var' and `rec-counter' if they are not needed, hoping
;; that the compiler will be smart enough to figure this out. Static
;; regular expressions are compiled out-of-loop, however.

;;; Code:
#!r6rs

(library (spells awk helpers)
  (export get-after-body
          get-range-vars
          expand-loop-body
          optimize-clauses
          parse-clause)
  (import (for (rnrs) run (meta -1))
          (for (srfi :8 receive) (meta -1))
          (for (wak irregex) (meta -1))
          (wak foof-loop)
          (spells alist)
          (spells tracing)
          (spells algebraic-types)
          (spells syntax-utils)
          (for (spells awk range-tests) (meta -1)))

(define-datatype <clause>
  (after-clause (body))
  (else-clause (body))
  (range-clause (type start-test stop-test body))
  (simple-clause (test body))
  (arrow-clause (test expr)))

(define-datatype <test>
  (when-test (expr))
  (counter-test (n))
  (rx-test (expr))
  (dynamic-rx-test (expr)))

(define (parse-clause clause)
  (syntax-case clause ()
    ((after body ...)
     (symbolic-identifier=? 'after #'after)
     (make-after-clause #'(body ...)))
    ((else body ...)
     (symbolic-identifier=? 'else #'else)
     (make-else-clause #'(body ...)))
    ((test => expr)
     (symbolic-identifier=? '=> #'=>)
     (make-arrow-clause (parse-test #'test) #'expr))
    ((range-id start-test stop-test body ...)
     (and (identifier? #'range-id)
          (memq (syntax->datum #'range-id) '(range range: :range :range:)))
     (make-range-clause (syntax->datum #'range-id)
                        (parse-test #'start-test)
                        (parse-test #'stop-test)
                        #'(body ...)))
    ((test body ...)
     (make-simple-clause (parse-test #'test) #'(body ...)))))

(define (get-range-vars clauses)
  (generate-temporaries (filter range-clause? clauses)))

(define (get-after-body clauses svars)
  (loop ((for clause (in-list clauses))
         (for body (appending-reverse
                    (after-clause-body clause)
                    (if (after-clause? clause)))))
    => (if (null? body)
           (list #`(values #,@svars))
           (reverse body))))

(define (parse-test test)
  (syntax-case test ()
    ((when expr)
     (symbolic-identifier=? 'when #'when)
     (make-when-test #'expr))
    ((unquote expr)
     (symbolic-identifier=? 'unquote #'unquote)
     (make-dynamic-rx-test #'expr))
    (rx
     (sre-form? (syntax->datum #'rx))
     (make-rx-test #'rx))
    (n
     (integer? (syntax->datum #'n))
     (make-counter-test #'n))
    (expr
     (make-when-test #'expr))))

;; List extracted from the irregex documentation
(define sre-heads
  '(
    seq                                 ; sequence
    :
    or                                  ; alternation

    ;; optional/multiple patterns
    ?                                   ; 0 or 1 matches
    *                                   ; 0 or more matches
    +                                   ; 1 or more matches
    =                                   ; exactly <n> matches
    >=                                  ; <n> or more matches
    **                                  ; <n> to <m> matches
    ??                     ; non-greedy (non-greedy) pattern: (0 or 1)
    *?                     ; non-greedy kleene star
    **?                    ; non-greedy range

    ;; submatch patterns
    submatch                            ; numbered submatch
    $
    submatch-named                      ; named submatch
    =>
    backref                             ; match a previous submatch

    ;; toggling case-sensitivity
    w/case                      ; enclosed <sre>s are case-sensitive
    w/nocase                    ; enclosed <sre>s are case-insensitive

    ;; character sets
    or                                  ; set union
    ~                                   ; set complement (i.e. [^...])
    -                                   ; set difference
    &                                   ; set intersection
    /                                   ; pairs of chars as ranges

    ;; named character sets
    any
    nonl
    ascii
    lower-case     lower
    upper-case     upper
    alphabetic     alpha
    numeric        num
    alphanumeric   alphanum  alnum
    punctuation    punct
    graphic        graph
    whitespace     white     space
    printing       print
    control        cntrl
    hex-digit      xdigit

    ;; assertions and conditionals
    bos eos                  ; beginning/end of string
    bol eol                  ; beginning/end of line
    bow eow                  ; beginning/end of word
    nwb                      ; non-word-boundary
    look-ahead               ; zero-width look-ahead assertion
    look-behind              ; zero-width look-behind assertion
    neg-look-ahead         ; zero-width negative look-ahead assertion
    neg-look-behind        ; zero-width negative look-behind assertion
    atomic                 ; for (?>...) independent patterns
    if                     ; conditional patterns
    commit                 ; don't backtrack beyond this (i.e. cut)

    ;; backwards compatibility
    posix-string                        ; embed a POSIX string literal
  ))

(define (sre-form? form)
  (or (string? form)
      (char? form)
      (and (pair? form)
           (let ((head (car form)))
             (or (and (symbol? head)
                      (memq head sre-heads))
                 ;; (<string> ...) ; set(s) of chars
                 (for-all string? form))))))

;; Takes a list of (parsed) clauses, and returns:
;; - An list of clauses, whith all static rx tests replaced by
;;   variable references
;; - A LET-list of bindings needed to be installed around the context
;;   where using the above syntax objects
(define (optimize-clauses clauses)
  (loop continue ((for clause (in-list clauses))
                  (with opt-clauses '())
                  (with bindings '()))
    => (values (reverse opt-clauses) bindings)
    (let ()
      (define (opt-tests tests constructor . args)
        (loop next-test ((for test (in-list tests))
                         (with opt-tests '())
                         (with vars (generate-temporaries (filter rx-test? tests)))
                         (with test-bindings '()))
          => (continue
              (=> opt-clauses (cons (apply constructor
                                           (append (reverse opt-tests) args))
                                    opt-clauses))
              (=> bindings (append test-bindings bindings)))
          (cases <test> test
            ((rx-test expr)
             (next-test (=> opt-tests (cons (make-rx-test (car vars)) opt-tests))
                        (=> vars (cdr vars))
                        (=> test-bindings (cons #`(#,(car vars) (irregex '#,expr))
                                                test-bindings))))
            ((dynamic-rx-test expr)
             (next-test (=> opt-tests (cons (make-rx-test expr) opt-tests))))
            (else
             (next-test (=> opt-tests (cons test opt-tests)))))))
      (cases <clause> clause
        ((simple-clause test body)
         (opt-tests (list test) make-simple-clause body))
        ((range-clause type start-test stop-test body)
         (opt-tests (list start-test stop-test)
                    (lambda args
                      (apply make-range-clause type args))
                    body))
        ((arrow-clause test expr)
         (opt-tests (list test) make-arrow-clause expr))
        (else
         (continue (=> opt-clauses (cons clause opt-clauses))))))))

;; Make a Scheme expression out of a test form.
;; Integer i		=>  (= i <record-counter>)
;; SRE s		=>  (regexp-search <re> <record>)
;; (when e)		=>  e
;; Expression e	=>  e
;; 
;; If FOR-VALUE? is true, then we do regexp searches with REGEXP-SEARCH,
;; otherwise, we use the cheaper REGEXP-SEARCH?.

(define (->simple-clause-test test rec-var rec-counter)
  (cases <test> test
    ((when-test expr) expr)
    ((rx-test expr) #`(irregex-search #,expr #,rec-var))
    ((counter-test expr) #`(= #,expr #,rec-counter))))

(define (range-clause-step type start-test stop-test body
                                 range-var rec-var rec-counter svars)
  (let ((step-vars #`(#,range-var #,@svars)))
    (with-syntax ((start-thunk
                   #`(lambda ()
                       #,(->simple-clause-test start-test rec-var rec-counter)))
                  (stop-thunk
                   #`(lambda ()
                       #,(->simple-clause-test stop-test rec-var rec-counter))))
      #`((^else-var #,@step-vars)
         (receive (this-record? #,range-var)
                  (#,(range-tester type) start-thunk stop-thunk #,range-var)
           (if this-record?
               #,(clause-action body #'#f range-var svars)
               #,@(null-clause-action step-vars)))))))

(define (range-tester type)
  (case type
    ((range)   #'next-range)
    ((range:)  #'next-range:)
    ((:range)  #'next-:range)
    ((:range:) #'next-:range:)))

;; Expand into the body of the awk loop -- the code that tests & executes
;; each clause, and then jumps to the top of the loop.
(define (expand-loop-body rec-var rec-counter range-vars svars clauses)
  (loop continue ((for clause (in-list clauses))
                  (with rvars range-vars)
                  (with steps '()))
    => (with-syntax (((step ...) (reverse steps)))
         #`((let ((^else-var #t)
                  (#,rec-counter (+ #,rec-counter 1)))
              (let*-values (step ...)
                (^loop-var #,rec-counter #,@range-vars #,@svars)))))
      (let ((null-clause-maybe (null-clause-action svars)))
        (define (mktest test)
          (->simple-clause-test test rec-var rec-counter))
        (define (proceed/basic-clause action)
          (continue (=> steps (cons #`((^else-var #,@svars) #,action) steps))))
        (cases <clause> clause
          ((after-clause body)
           (continue)) ;; skip after clause
          ((else-clause body)
           (proceed/basic-clause
            #`(if ^else-var
                  #,(clause-action body #'#t #f svars)
                  #,@(sloppy-mult-values #`(#t #,@svars)))))
          ((arrow-clause test expr)
           (proceed/basic-clause
            #`(let ((tv #,(mktest test)))
                (if tv
                    #,(clause-action (list #`(#,expr tv)) #'#f #f svars)
                    #,@null-clause-maybe))))
          ((simple-clause test body)
           (proceed/basic-clause
            #`(if #,(mktest test)
                  #,(clause-action body #'#f #f svars)
                  #,@null-clause-maybe)))
          ((range-clause type start-test stop-test body)
           (continue (=> steps
                         (cons (range-clause-step type start-test stop-test body
                                                  (car rvars)
                                                  rec-var rec-counter svars)
                               steps))
                     (=> rvars (cdr rvars))))))))

;; The clause didn't execute. Return the svars unchanged, and also
;; return the current else-value. We return a 0 or 1 element
;; expression list -- if no values are being expected this returns the
;; empty list.

(define (null-clause-action svars)
  (sloppy-mult-values #`(^else-var #,@svars)))

;; ()      => ()
;; (v1)    => (v1)
;; (v1 v2) => ((VALUES v1 v2))
;;
;; Return an expression list, not an expression. (Either 1 or 0 expressions.)
;; Use this one when we don't care what happens if we are returning 0 vals.
;; It pairs up with MV-LET below, which ignores the number of values
;; returned to it when expecting zero values.

(define (sloppy-mult-values vals)
  (syntax-case vals ()
    ((x y . z)
     #`((values #,@vals)))
    ((x)
     vals)))

;; BODY is a list of expressions from a loop clause. We want to evaluate it, 
;; under some conditions.
;; - The body evaluates to multiple values, one for each state variable.
;;   However, if there are no state variables, we want to *ignore* the
;;   values produced by the body, and explicitly return 0 values,
;;   not blow up if the body should happen not to return exactly zero values.

(define (clause-action body is-else? range-var svars)
  (syntax-case svars ()
    ((x . y)
     #`(receive #,svars (begin #,@body)
         (values #,is-else? #,@(if range-var #`(#,range-var) #'()) #,@svars)))
    (()
     ;; No state vars -- ignore value computed by BODY forms.
     #`(begin #,@body #,@(if range-var
                             #`((values #,is-else? #,range-var))
                             #`(#,is-else?))))))


)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (cases 2))
;; End:
