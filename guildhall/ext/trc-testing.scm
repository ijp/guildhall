;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

#!r6rs

(library (guildhall ext trc-testing)
  (export
   define-test-suite
   make-test-suite
   test-suite?
   test-suite/name
   test-suite/description
   test-suite/tests

   define-test-case
   test-case
   make-test-case
   test-case?
   test-case/name
   test-case/description
   test-case/constructor

   add-test!
   run-test-case
   run-test-suite
   run-test
   find-test

   test-predicate
   test-compare
   test-eq
   test-eqv
   test-equal

   ;; Reexport these for convenience.  Ordinarily reexportation is
   ;; anathema, but I think that here it is probably safe, because
   ;; most users will never use TESTING-PARAMETERS anyway.
   test-failure
   test-failure:predicate-datum
   test-failure:compare-datum

   test-verbosity      with-test-verbosity     set-test-verbosity!
   test-debug-errors?  with-test-debug-errors? set-test-debug-errors?!)

  (import (except (rnrs base) error)
          (rename (rnrs base) (error rnrs:error))
          (only (guile) include-from-path)
          (rnrs lists)
          (rnrs mutable-pairs)
          (srfi :8 receive)
          (srfi :9 records)
          (guildhall ext private define-values)
          (guildhall ext private include)
          (guildhall ext trc-testing parameters))

  (define (error . args)
    (apply rnrs:error "trc-testing" args))

  ;; Hack.  We shouldn't be messing in another module's path.
  (include-from-path "guildhall/ext/foof-loop/private/syn-param.scm")
  (include-file/downcase ((guildhall ext trc-testing private) test))

  )
