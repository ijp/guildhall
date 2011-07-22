;;; -*- Mode: Scheme -*-

;;;; Testing Utility for Scheme

;;; Copyright (C) 2007, 2009 Taylor R. Campbell.
;;;
;;; This file is part of TRC-Testing.
;;;
;;; TRC-Testing is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; TRC-Testing is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with TRC-Testing.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Parameters:
;;;
;;; (WITH-TEST-CASE-RUN <name> <description> <thunk>)
;;; (WITH-TEST-SUITE-RUN <name> <description> <thunk>)
;;; (NILADIC-TEST)
;;; (MONADIC-TEST <thunk>)
;;; (POLYADIC-TEST <thunks>)
;;; (COMPONENT-TEST <thunk>)
;;; (TEST-FAILURE <message> <irritant> ...)
;;; (TEST-FAILURE:PREDICATE-DATUM <predicate> <expression> <datum>)
;;; (TEST-FAILURE:COMPARE-DATUM <comparator>
;;;                             <expected-expression> <expected-datum>
;;;                             <actual-expression> <actual-datum>)

(define-record-type <test-suite>
    (%make-test-suite name description tests)
    test-suite?
  (name test-suite/name)
  (description test-suite/description)
  (tests test-suite/tests set-test-suite/tests!))

(define (make-test-suite name description)
  (%make-test-suite name description '()))

(define-record-type <test-case>
    (make-test-case name description constructor)
    test-case?
  (name test-case/name)
  (description test-case/description)
  (constructor test-case/constructor))

(define (add-test! suite name test)
  (let ((tests (test-suite/tests suite)))
    (cond ((assv name tests)
           => (lambda (probe)
                (set-cdr! probe test)))
          (else
           (set-test-suite/tests! suite (cons (cons name test) tests))))))

(define (run-test-case test-case)
  (with-test-case-run (test-case/name test-case)
      (test-case/description test-case)
    (lambda ()
      (receive (setup teardown bodies) ((test-case/constructor test-case))
        (define (body->thunk body)
          (lambda ()
            (dynamic-wind setup body teardown)))
        (cond ((not (pair? bodies))
               (niladic-test))
              ((not (pair? (cdr bodies)))
               (monadic-test (body->thunk (car bodies))))
              (else
               (polyadic-test (map body->thunk bodies))))))))

(define (run-test-suite test-suite)
  (with-test-suite-run (test-suite/name test-suite)
      (test-suite/description test-suite)
    (lambda ()
      (for-each (lambda (name.test)
                  (component-test (lambda () (run-test (cdr name.test)))))
                (reverse (test-suite/tests test-suite))))))

(define (run-test test)
  (cond ((test-case? test) (run-test-case test))
        ((test-suite? test) (run-test-suite test))
        (else (error "Invalid test:" test))))

(define (find-test suite name)
  (let loop ((tests (test-suite/tests suite)))
    (cond ((not (pair? tests))
           (error "No such test by name in suite:" name suite))
          ((eqv? name (caar tests))
           (cdar tests))
          (else
           (loop (cdr tests))))))

;;;; Test Macros

(define-syntax test-predicate
  (syntax-rules ()
    ((test-predicate predicate expression)
     (let ((datum expression))
       (if (not (predicate expression))
           (test-failure:predicate-datum 'predicate 'expression datum))))))

(define-syntax test-compare
  (syntax-rules ()
    ((test-compare comparator expected-expression actual-expression)
     (let ((expected-datum expected-expression)
           (actual-datum actual-expression))
       (if (not (comparator expected-datum actual-datum))
           (test-failure:compare-datum 'comparator
                                       'expected-expression expected-datum
                                       'actual-expression actual-datum))))))

(define-syntax test-eq
  (syntax-rules ()
    ((test-eq expected-expression actual-expression)
     (test-compare eq? expected-expression actual-expression))))

(define-syntax test-eqv
  (syntax-rules ()
    ((test-eq expected-expression actual-expression)
     (test-compare eqv? expected-expression actual-expression))))

(define-syntax test-equal
  (syntax-rules ()
    ((test-eq expected-expression actual-expression)
     (test-compare equal? expected-expression actual-expression))))

;;;; Syntactic Sugar

(define-syntax define-test-suite
  (syntax-rules ()
    ((define-test-suite (suite-name parent) description)
     (define suite-name
       (let ((suite-name (make-test-suite 'suite-name 'description)))
         (add-test! parent 'suite-name suite-name)
         suite-name)))
    ((define-test-suite suite-name description)
     (define suite-name (make-test-suite 'suite-name 'description)))))

(define-syntax define-test-case
  (syntax-rules ()
    ((define-test-case test-suite name test-case)
     (define-values () ;make this expand into a definition
       (add-test! test-suite `name test-case)))
    ((define-test-case test-suite test-case-name (option ...) test ...)
     (define-values () ;ditto
       (let ((name `test-case-name))
         (add-test! test-suite name
           (test-case ,name (option ...) test ...)))))))

(define-syntax test-case
  (syntax-rules ()
    ;; do the syntactically fast case with no options.
    ;; with-extended-parameter-operators* is *slow*.
    ((test-case test-case-name () test ...)
     (%test-case test-case-name #f (test ...) ((values)) ((values))))
    ((test-case test-case-name (option ...) test ...)
     (with-extended-parameter-operators*
         ((%test-case*
           ()                      ;no named parameter pattern literals
           (%test-case
            (name               ((name ?name)) ?name #f)
            (description        ((description ?description)) ?description #f)
            (tests              ((tests . ?tests)) ?tests #f)
            ;; unfortunately, because of...issues with ellipsis, we
            ;; can't write the actual patterns we want to write here
            ;; for non-empty proper list bodies.
            (setup              ((setup . ?setup-body)) ?setup-body ((values)))
            (teardown           ((teardown . ?teardown-body))
                                ?teardown-body
                                ((values))))))
       ;; force named parameters by using leading ones.
       (%test-case* (name test-case-name) (tests test ...) option ...)))))

(define-syntax %test-case
  (syntax-rules ()
    ((%test-case name
                 description
                 (test ...)
                 (setup-body0 setup-body1 ...)
                 (teardown-body0 teardown-body1 ...))
     (make-test-case `name
                     'description
                     (lambda ()
                       (values (lambda () setup-body0 setup-body1 ...)
                               (lambda () teardown-body0 teardown-body1 ...)
                               (list (lambda () test)
                                     ...)))))))
