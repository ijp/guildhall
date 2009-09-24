;;; solver.scm --- Dependency solver unit tests

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>
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

;; This file is based on the test cases for aptitude's resolver which
;; are written in a DSL and exectuted by the test.cc driver. This
;; being Lisp, there's no need to seperate test specifications and the
;; driver.

;;; Code:

(import (rnrs)
        (dorodango solver)
        (dorodango solver dummy-db)
        (dorodango solver universe)
        (spells match)
        (spells fmt)
        (spells testing)
        (spells logging))

;; Set this to `#t' to show the universe that the solver will operate
;; in.
(define show-universe? #f)

;; Set this to `#t' to get detailed traces of the solver's
;; operation. This is useful for debugging in general, and comparing
;; with aptitude's resolvers' debugging output specifically.
(define debug-output? #f)

(define (simple-log-formatter entry)
  (let ((port (current-output-port))
        (obj (log-entry-object entry)))
    (if (procedure? obj)
        (obj port)
        (display obj port))
    (newline port)))

(when debug-output?
  (set-logger-properties! root-logger
                          `((threshold trace)
                            (handlers ,simple-log-formatter))))

(define (make-test-universe packages dependencies)
  (let ((db (make-dummy-db)))
    (for-each (lambda (package)
                (apply dummy-db-add-package! db package))
              packages)
    (for-each (match-lambda
               ((source version relation . targets)
                (dummy-db-add-dependency! db source version (eq? relation '<>) targets)))
              dependencies)
    (let ((universe (dummy-db->universe db)))
        (when show-universe?
          (fmt #t (dsp-universe universe)))
        universe)))

(define (test-solutions expected solver)
  (for-each (match-lambda
             ((max-steps 'any)
              (test-eqv #t (solution? (find-next-solution! solver max-steps))))
             ((max-steps #f)
              (test-eqv #f (find-next-solution! solver max-steps))))
            expected))

(define-test-suite solver-tests
  "Dependency resolver")

(define-test-case solver-tests test1 ()
  (let ((universe (make-test-universe
                   '((p1 (1 2 3) 1)
                     (p2 (1 2 3) 1)
                     (p3 (1 2 3) 1)
                     (p4 (1 2 3) 1))
                   '((p1 1 -> (p2 . 2) (p2 . 3))
                     (p1 2 -> (p2 . 2) (p2 . 3))
                     (p1 3 -> (p2 . 2) (p2 . 3))
                     
                     (p2 1 -> (p3 . 1) (p3 . 2) (p3 . 3))
                     (p2 2 -> (p3 . 1) (p3 . 2) (p3 . 3))
                     (p2 1 <> (p1 . 2) (p1 . 3))
                     
                     (p3 1 -> (p4 . 1) (p4 . 2) (p4 . 3))
                     (p3 2 -> (p4 . 1) (p4 . 2) (p4 . 3))
                     (p3 3 -> (p4 . 1) (p4 . 2) (p4 . 3))))))
    (test-solutions '((10000 any)
                      (10000 any)
                      (10000 #f))
      (make-solver universe))))

(run-test-suite solver-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing as-match (test-solutions 1))
;; End:
