;;; -*- Mode: Scheme -*-

;;;; Nested Loops with foof-loop, Version 10 (BETA)

;;; Copyright (c) 2008, Taylor R. Campbell
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

(define-syntax nested-loop
  (syntax-rules ()
    ((nested-loop continuation ((state initial) ...) combiner
         clause0 clause1+ ...)
     (%nested-loop loop continuation ((state initial) ...) combiner
         clause0 clause1+ ...))))

(define-syntax nested-lazy-loop
  (syntax-rules ()
    ((nested-loop continuation ((state initial) ...) combiner
         clause0 clause1+ ...)
     (%nested-loop lazy-loop continuation ((state initial) ...) combiner
         clause0 clause1+ ...))))

(define-syntax %nested-loop
  (syntax-rules (parallel nested do let let-values if not and or)

    ((%nested-loop looper continuation ((state initial) ...) combiner
       expression)
     (let ((state initial) ...)
       (combiner (lambda () expression) continuation)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (parallel (iterator ...) ...)
         clause0 clause1+ ...)
     (looper continue ((with state initial)
                       ...
                       (iterator ...)
                       ...)
       => (continuation state ...)
       (%nested-loop looper (lambda (state ...) (continue state ...))
           ((state state) ...)
           combiner
           clause0 clause1+ ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (nested clause ...)
         clause0 clause1+ ...)
     (%nested-loop looper continuation ((state initial) ...) combiner
         clause ... clause0 clause1+ ...))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (do command ...)
         clause0 clause1+ ...)
     (begin command ...
            (%nested-loop looper continuation ((state initial) ...) combiner
                clause0 clause1+ ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (let ((variable value) ...))
         clause0 clause1+ ...)
     (let ((variable value) ...)
       (%nested-loop looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (let variable value)
         clause0 clause1+ ...)
     (let ((variable value))
       (%nested-loop looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (let-values ((bvl expression) ...))
         clause0 clause1+ ...)
     (let-values ((bvl expression) ...)
       (%nested-loop looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (let-values bvl expression)
         clause0 clause1+ ...)
     (let-values ((bvl expression))
       (%nested-loop looper continuation ((state initial) ...) combiner
           clause0 clause1+ ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (if condition)
         clause0 clause1+ ...)
     (if condition
         (%nested-loop looper continuation ((state initial) ...) combiner
             clause0 clause1+ ...)
         (continuation initial ...)))

    ((%nested-loop looper continuation ((state initial) ...) combiner
         ((iterator ...) ...)
         clause0 clause1+ ...)
     (%nested-loop looper continuation ((state initial) ...) combiner
         (parallel (iterator ...) ...)
         clause0 clause1+ ...))

    ;** this clause must come last!  it would shadow the others.

    ((%nested-loop looper continuation ((state initial) ...) combiner
         (iterator ...)
         clause0 clause1+ ...)
     (%nested-loop looper continuation ((state initial) ...) combiner
         (parallel (iterator ...))
         clause0 clause1+ ...))))

;;;; Iteration

(define-syntax iterate*
  (syntax-rules (=>)
    ((iterate* ((state initial) ...) => result stepper clause0 clause1+ ...)
     (nested-loop (lambda (state ...) result)
         ((state initial) ...) stepper clause0 clause1+ ...))
    ((iterate* ((state initial) ...) stepper clause0 clause1+ ...)
     (nested-loop values* ((state initial) ...) stepper
         clause0 clause1+ ...))))

(define-syntax iterate
  (syntax-rules (=>)
    ((iterate ((state initial) ...) => result stepper clause0 clause1+ ...)
     (iterate* ((state initial) ...) => result
         (lambda (body continuation)
           (receive (state ...) (stepper (body) state ...)
             (continuation state ...)))
         clause0 clause1+ ...))
    ((iterate ((state initial) ...) stepper clause0 clause1+ ...)
     (iterate* ((state initial) ...)
         (lambda (body continuation)
           (receive (state ...) (stepper (body) state ...)
             (continuation state ...)))
         clause0 clause1+ ...))))

(define-syntax iterate!
  (syntax-rules ()
    ((iterate! clause0 clause1+ ...)
     (iterate* ()                       ;no state
         (lambda (body continuation) (body) (continuation))
         clause0 clause1+ ...))))

(define-syntax iterate-values
  (syntax-rules (=>)

    ((iterate-values ((state initial) ...) => result
         clause0 clause1+ ...)
     (iterate* ((state initial) ...) => result call-with-values
         clause0 clause1+ ...))

    ((iterate-values updater ((state initial) ...) => result
         clause0 clause1+ ...)
     ;++ this should be visible only in the final expression.  however,
     ;++ that requires tail patterns, which are non-standard.
     (with-extended-parameter-operators
         ((updater (values* (state . state) ...)))
       (iterate-values ((state initial) ...) => result clause0 clause1+ ...)))

    ((iterate-values ((state initial) ...) clause0 clause1+ ...)
     (iterate* ((state initial) ...) call-with-values
         clause0 clause1+ ...))

    ((iterate-values updater ((state initial) ...) clause0 clause1+ ...)
     (with-extended-parameter-operators
         ((updater (values* (state . state) ...)))
       (iterate* ((state initial) ...) call-with-values
         clause0 clause1+ ...)))))

;++ Hack for MIT Scheme, whose multiple return values are broken.

(define-syntax values*
  (syntax-rules ()
    ((values* single) single)
    ((values* multiple ...) (values multiple ...))))

;;;; Recursion

(define-syntax recur*
  (syntax-rules ()
    ((recur* base-case combiner clause0 clause1+ ...)
     (nested-loop (lambda () base-case)
         ()                             ;no state
         combiner
         clause0 clause1+ ...))))

(define-syntax lazy-recur*
  (syntax-rules ()
    ((lazy-recur* base-case combiner clause0 clause1+ ...)
     (nested-lazy-loop (lambda () base-case)
         ()                             ;no state
         combiner
         clause0 clause1+ ...))))

(define-syntax recur
  (syntax-rules ()
    ((recur base-case combiner clause0 clause1+ ...)
     (recur* base-case
         (lambda (body continuation)
           (combiner (body) (continuation)))
         clause0 clause1+ ...))))

(define-syntax lazy-recur
  (syntax-rules ()
    ((lazy-recur base-case combiner clause0 clause1+ ...)
     (lazy-recur* base-case
         (lambda (body continuation)
           (combiner (body) (continuation)))
         clause0 clause1+ ...))))

(define-syntax recur-values
  (syntax-rules (=>)
    ((recur-values base-case => result clause0 clause1+ ...)
     (call-with-values (lambda ()
                         (recur-values base-case clause0 clause1+ ...))
       result))

    ((recur-values base-case clause0 clause1+ ...)
     (recur* base-case
         (lambda (receiver-body recursion)
           (call-with-values recursion (receiver-body)))
         clause0 clause1+ ...))))

;;;; Collecting Lists & Streams

(define-syntax collect-list-reverse
  (syntax-rules (initial)

    ((collect-list-reverse (initial tail-expression) clause0 clause1+ ...)
     (iterate ((tail tail-expression)) cons clause0 clause1+ ...))

    ((collect-list-reverse clause0 clause1+ ...)
     (collect-list-reverse (initial '()) clause0 clause1+ ...))))

;;; The first definition of COLLECT-LIST is probably the one that you
;;; want.  On the other hand, what follows in comments is elegant, and
;;; shows the flexibility of the mchanism, especially when compared
;;; with the definition of COLLECT-STREAM.

(define-syntax collect-list
  (syntax-rules (initial)

    ((collect-list (initial tail-expression) clause0 clause1+ ...)
     (append-reverse (collect-list-reverse clause0 clause1+ ...)
                     tail-expression))

    ((collect-list clause0 clause1+ ...)
     (reverse (collect-list-reverse clause0 clause1+ ...)))))

; (define-syntax collect-list
;   (syntax-rules (INITIAL)
;
;     ((COLLECT-LIST (INITIAL tail-expression) clause0 clause1+ ...)
;      (RECUR tail-expression CONS clause0 clause1+ ...))
;
;     ((COLLECT-LIST clause0 clause1+ ...)
;      (COLLECT-LIST (INITIAL '()) clause0 clause1+ ...))))

(define-syntax collect-stream
  (syntax-rules (initial)

    ((collect-stream (initial tail-expression) clause0 clause1+ ...)
     (lazy-recur tail-expression stream-cons clause0 clause1+ ...))

    ((collect-stream clause0 clause1+ ...)
     (collect-stream (initial stream-nil) clause0 clause1+ ...))))

(define-syntax collect-list!
  (syntax-rules (initial)

    ((collect-list! (initial tail-expression) clause0 clause1+ ...)
     (let ((pair (cons #f tail-expression)))
       (collect-list-into! pair clause0 clause1+ ...)
       (cdr pair)))

    ((collect-list! clause0 clause1+ ...)
     (collect-list! (initial '()) clause0 clause1+ ...))))

(define-syntax collect-list-into!
  (syntax-rules ()
    ((collect-list-into! pair-expression clause0 clause1+ ...)
     (iterate* ((pair pair-expression))
         (lambda (body continuation)
           (let ((tail (cons (body) (cdr pair))))
             (set-cdr! pair tail)
             (continuation tail)))
         clause0 clause1+ ...))))

;;;; Collecting Vectors and Strings

(define-syntax collect-vector
  (syntax-rules ()
    ((collect-vector clause0 clause1+ ...)
     (list->vector (collect-list clause0 clause1+ ...)))))

(define-syntax collect-string
  (syntax-rules ()
    ((collect-string clause0 clause1+ ...)
     (list->string (collect-list clause0 clause1+ ...)))))

;;; The following definition of COLLECT-DISPLAY can collect any object,
;;; whose printed representation is computed using DISPLAY; it relies
;;; on SRFI 6 (Basic String Ports) to accomplish this.

(define-syntax collect-display
  (syntax-rules ()
    ((collect-display clause0 clause1+ ...)
     (let ((output-port (open-output-string)))
       (iterate* ()                      ;no state
           (lambda (body continuation)
             (display (body) output-port)
             (continuation))
           clause0 clause1+ ...)
       (get-output-string output-port)))))

;;;;; Expanding Vector and String Collection

;;; These are slower than the definitions with lists.  Go figure.

; (define-syntax collect-vector
;   (syntax-rules ()
;     ((COLLECT-VECTOR clause0 clause1+ ...)
;      (%COLLECT-VECTOR
;       (MAKE-VECTOR VECTOR-LENGTH VECTOR-SET! IN-VECTOR)
;       DATUM
;       ()           ;No check for the data.
;       clause0 clause1+ ...))))
;
; (define-syntax collect-string
;   (syntax-rules ()
;     ((COLLECT-STRING clause0 clause1+ ...)
;      (%COLLECT-VECTOR
;       (MAKE-STRING STRING-LENGTH STRING-SET! IN-STRING)
;       DATUM
;       ((IF (NOT (CHAR? DATUM))
;            (ERROR "Non-character in COLLECT-STRING:" DATUM)))
;       clause0 clause1+ ...))))
;
; (define-syntax %collect-vector
;   (syntax-rules ()
;     ((%COLLECT-VECTOR
;       (make-vector vector-length vector-set! in-vector)
;       datum (check ...)
;       clause0 clause1+ ...)
;      (RECEIVE (LENGTH CHUNK-INDEX CHUNK CHUNKS)
;          (ITERATE ((LENGTH 0)
;                    (CHUNK-INDEX 0)
;                    (CHUNK (make-vector #x10))
;                    (CHUNKS '()))
;              (LAMBDA (datum LENGTH CHUNK-INDEX CHUNK CHUNKS)
;                check ...
;                (LET ((CHUNK-LENGTH (vector-length CHUNK)))
;                  (IF (< CHUNK-INDEX CHUNK-LENGTH)
;                      (BEGIN
;                        (vector-set! CHUNK CHUNK-INDEX datum)
;                        (VALUES LENGTH
;                                (+ CHUNK-INDEX 1)
;                                CHUNK
;                                CHUNKS))
;                      (LET ((CHUNK*
;                             (make-vector
;                              (IF (>= CHUNK-LENGTH #x1000)
;                                  #x1000
;                                  (* CHUNK-LENGTH 2)))))
;                        (vector-set! CHUNK* 0 datum)
;                        (VALUES (+ LENGTH CHUNK-LENGTH)
;                                1        ;We filled in the first slot,
;                                CHUNK*   ;  so start at index 1.
;                                (CONS CHUNK CHUNKS))))))
;              clause0 clause1+ ...)
;        (LET* ((TOTAL-LENGTH (+ LENGTH CHUNK-INDEX))
;               (RESULT (make-vector TOTAL-LENGTH)))
;          (LOOP ((FOR ELEMENT OFFSET (in-vector CHUNK 0 CHUNK-INDEX)))
;            (vector-set! RESULT (+ LENGTH OFFSET) ELEMENT))
;          (LOOP ((FOR CHUNK (IN-LIST CHUNKS))
;                 (WITH BASE LENGTH BASE*)
;                 (LET BASE* (- BASE (vector-length CHUNK))))
;            (LOOP ((FOR ELEMENT OFFSET (in-vector CHUNK)))
;              (vector-set! RESULT (+ BASE* OFFSET) ELEMENT)))
;          RESULT)))))

;;;;; Non-reentrant Vector and String Collection

;;; For the following definitions, we defer the responsibility of
;;; bounds checking and error signalling to VECTOR-SET! and
;;; STRING-SET!.  This may not be a good idea.

(define-syntax collect-into-vector!
  (syntax-rules (from)

    ((collect-into-vector! vector-expression (from start-expression)
       clause0 clause1+ ...)
     (let ((vector vector-expression)
           (start start-expression))
       (iterate* ((index start))
           (lambda (body continuation)
             (vector-set! vector index (body))
             (continuation (+ index 1)))
           clause0 clause1+ ...)))

    ((collect-into-vector! vector-expression clause0 clause1+ ...)
     (collect-into-vector! vector-expression (from 0) clause0 clause1+ ...))))

(define-syntax collect-into-string!
  (syntax-rules (from)

    ((collect-into-string! string-expression (from start-expression)
       clause0 clause1+ ...)
     (let ((string string-expression)
           (start start-expression))
       (iterate* ((index start))
           (lambda (body continuation)
             (string-set! string index (body))
             (continuation (+ index 1)))
           clause0 clause1+ ...)))

    ((collect-into-string! string-expression clause0 clause1+ ...)
     (collect-into-string! string-expression (from 0) clause0 clause1+ ...))))

;;; These should probably have bang suffixes to emphasize that they are
;;; non-reentrant.

(define-syntax collect-vector-of-length
  (syntax-rules ()
    ((collect-vector-of-length length clause0 clause1+ ...)
     (let ((vector (make-vector length)))
       (collect-into-vector! vector clause0 clause1+ ...)
       vector))))

(define-syntax collect-string-of-length
  (syntax-rules ()
    ((collect-string-of-length length clause0 clause1+ ...)
     (let ((string (make-string length)))
       (collect-into-string! string clause0 clause1+ ...)
       string))))

;;;; Numerical Collection

(define-syntax collect-sum
  (syntax-rules (initial)

    ((collect-sum (initial value-expression) clause0 clause1+ ...)
     (iterate ((sum value-expression)) + clause0 clause1+ ...))

    ((collect-sum clause0 clause1+ ...)
     (collect-sum (initial 0) clause0 clause1+ ...))))

(define-syntax collect-product
  (syntax-rules (initial)

    ((collect-product (initial value-expression) clause0 clause1+ ...)
     (iterate ((product value-expression)) * clause0 clause1+ ...))

    ((collect-product clause0 clause1+ ...)
     (collect-product (initial 1) clause0 clause1+ ...))))

(define-syntax collect-count
  (syntax-rules ()
    ((collect-count clause0 clause1+ ...)
     (collect-sum clause0 clause1+ ... 1))))

(define-syntax collect-average
  (syntax-rules ()
    ((collect-average clause0 clause1+ ...)
     (receive (sum count)
              (iterate* ((sum 0) (count 0))
                  (lambda (body continuation)
                    (continuation (+ sum (body)) (+ count 1)))
                  clause0 clause1+ ...)
       (/ sum count)))))

;;;; Collecting Extrema

(define-syntax collect-extremum
  (syntax-rules (initial)

    ((collect-extremum comparator-expression (initial initial-expression)
         clause0 clause1+ ...)
     (let ((comparator comparator-expression))
       (iterate ((extremum initial-expression))
           (lambda (datum extremum)
             (if (comparator datum extremum) datum extremum))
           clause0 clause1+ ...)))

    ((collect-extremum comparator-expression clause0 clause1+ ...)
     (let ((comparator comparator-expression))
       (iterate ((extremum #f))
           (lambda (datum extremum)
             (if (and datum extremum)
                 (if (comparator datum extremum) datum extremum)
                 (or datum extremum)))
         clause0 clause1+ ...)))))

(define-syntax collect-minimum
  (syntax-rules (initial)

    ((collect-minimum (initial initial-expression) clause0 clause1+ ...)
     (iterate ((minimum initial-expression)) min clause0 clause1+ ...))

    ((collect-minimum clause0 clause1+ ...)
     (iterate ((minimum #f))
         (lambda (datum minimum)
           (if (and datum minimum)
               (min datum minimum)
               (or datum minimum)))
         clause0 clause1+ ...))))

(define-syntax collect-maximum
  (syntax-rules (initial)

    ((collect-maximum (initial initial-expression) clause0 clause1+ ...)
     (iterate ((maximum initial-expression)) max clause0 clause1+ ...))

    ((collect-maximum clause0 clause1+ ...)
     (iterate ((maximum #f))
         (lambda (datum maximum)
           (if (and datum maximum)
               (max datum maximum)
               (or datum maximum)))
         clause0 clause1+ ...))))

;;;;; Generalization by Multiple Values

(define-syntax collect-extremum*
  (syntax-rules (initial)

    ((collect-extremum* comparator-expression
         (initial key-expression element-expression)
         clause0 clause1+ ...)
     (let ((comparator comparator-expression)
           (initial-key key-expression)
           (initial-element element-expression))
       (iterate* ((extreme-key initial-key)
                  (extreme-element initial-element))
           (lambda (body continuation)
             (receive (key element) (body)
               (if (comparator key extreme-key)
                   (continuation key element)
                   (continuation extreme-key extreme-element))))
           clause0 clause1+ ...)))

    ((collect-extremum* comparator-expression clause0 clause1+ ...)
     (let ((comparator comparator-expression))
       (iterate* ((extreme-key #f)
                  (extreme-element #f))
           (lambda (body continuation)
             (receive (key element) (body)
               (if key
                   (if extreme-key
                       (if (comparator key extreme-key)
                           (continuation key element)
                           (continuation extreme-key extreme-element))
                       (continuation key element))
                   (continuation extreme-key extreme-element))))
           clause0 clause1+ ...)))))

(define-syntax collect-minimum*
  (syntax-rules (initial)

    ((collect-minimum* (initial key-expression element-expression)
         clause0 clause1+ ...)
     (collect-extremum* < (initial key-expression element-expression)
         clause0 clause1+ ...))

    ((collect-minimum* clause0 clause1+ ...)
     (collect-extremum* < clause0 clause1+ ...))))

(define-syntax collect-maximum*
  (syntax-rules (initial)

    ((collect-maximum* (initial key-expression element-expression)
         clause0 clause1+ ...)
     (collect-extremum* < (initial key-expression element-expression)
         clause0 clause1+ ...))

    ((collect-maximum* clause0 clause1+ ...)
     (collect-extremum* < clause0 clause1+ ...))))

;;;;; Generalization by Selectors

(define-syntax collect-extremum-by
  (syntax-rules (initial)

    ((collect-extremum-by comparator-expression selector-expression
         (initial initial-expression)
         clause0 clause1+ ...)
     (let ((comparator comparator-expression)
           (selector selector-expression)
           (initial-element initial-expression))
       (iterate* ((extreme-key (selector initial-element))
                  (extreme-element initial-element))
           => extreme-element
           (lambda (body continuation)
             (let* ((element (body))
                    (key (selector element)))
               (if (comparator key extreme-key)
                   (continuation key element)
                   (continuation extreme-key extreme-element))))
           clause0 clause1+ ...)))

    ((collect-extremum-by comparator-expression selector-expression
         clause0 clause1+ ...)
     (let ((comparator comparator-expression)
           (selector selector-expression))
       (iterate* ((extreme-key #f) (extreme-element #f))
           => extreme-element
           (lambda (body continuation)
             (let* ((element (body))
                    (key (selector element)))
               (if key
                   (if extreme-key
                       (if (comparator key extreme-key)
                           (continuation key element)
                           (continuation extreme-key extreme-element))
                       (continuation key element))
                   (continuation extreme-key extreme-element))))
           clause0 clause1+ ...)))))

(define-syntax collect-minimum-by
  (syntax-rules (initial)

    ((collect-minimum-by selector-expression (initial initial-expression)
         clause0 clause1+ ...)
     (collect-extremum-by < selector-expression (initial initial-expression)
         clause0 clause1+ ...))

    ((collect-minimum-by selector-expression clause0 clause1+ ...)
     (collect-extremum-by < selector-expression clause0 clause1+ ...))))

(define-syntax collect-maximum-by
  (syntax-rules (initial)

    ((collect-maximum-by selector-expression (initial initial-expression)
         clause0 clause1+ ...)
     (collect-extremum-by > selector-expression (initial initial-expression)
         clause0 clause1+ ...))

    ((collect-maximum-by selector-expression clause0 clause1+ ...)
     (collect-extremum-by > selector-expression clause0 clause1+ ...))))

;;;; Miscellaneous

;;; COLLECT-FIRST and COLLECT-OR work nicely.  COLLECT-LAST and
;;; COLLECT-AND have the unfortunate property that the final expression
;;; is not evaluated in a tail position, which is very hard to arrange
;;; in the general case.  For example, compare these two definitions of
;;; (a reduced version of) EVERY from SRFI 1:
;;;
;;; (define (every predicate list)
;;;   (and (pair? list)
;;;        (let loop ((list list))
;;;          (let ((tail (cdr list)))
;;;            (if (pair? tail)
;;;                (and (predicate (car list))
;;;                     (loop tail))
;;;                (predicate (car list)))))))
;;;
;;; (define (every predicate list)
;;;   (collect-and (for element (in-list list))
;;;     (predicate element)))
;;;
;;; The first definition duplicates the call to PREDICATE so that the
;;; last is in a tail position.  COLLECT-AND cannot do this.

(define-syntax collect-first
  (syntax-rules (default)

    ((collect-first (default default-expression) clause0 clause1+ ...)
     (nested-loop (lambda () default-expression)
         ()                             ;no state
         (lambda (body continuation)
           continuation                 ;ignore
           (body))
         clause0 clause1+ ...))

    ((collect-first clause0 clause1+ ...)
     (collect-first (default (error "nothing generated in collect-first."))
         clause0 clause1+ ...))))

(define-syntax collect-last
  (syntax-rules (default)
    ((collect-last (default default-expression) clause0 clause1+ ...)
     (nested-loop (lambda (result) result)
         ((result default-expression))
         (lambda (body continuation) (continuation (body)))
         clause0 clause1+ ...))))

(define-syntax collect-or
  (syntax-rules ()
    ((collect-or clause0 clause1+ ...)
     (nested-loop (lambda () #f)
         ()                             ;no state
         (lambda (body continuation) (or (body) (continuation)))
         clause0 clause1+ ...))))

(define-syntax collect-and
  (syntax-rules ()
    ((collect-and clause0 clause1+ ...)
     (nested-loop (lambda (result) result)
         ((result #f))
         (lambda (body continuation)
           (let ((result (body))) (and result (continuation result))))
         clause0 clause1+ ...))))
