;;; solver.scm --- Dependency solver unit tests

;; Copyright (C) 2011 Free Software Foundation, Inc.
;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>
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
        (dorodango solver logging)
        (spells match)
        (wak fmt)
        (wak trc-testing)
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

(define (make-test-db packages dependencies)
  (let ((db (make-dummy-db)))
    (for-each (lambda (package)
                (apply dummy-db-add-package! db package))
              packages)
    (for-each (match-lambda
               ((source version relation . targets)
                (dummy-db-add-dependency! db source version (eq? relation '<>) targets)))
              dependencies)
    db))

(define (test-db->universe db)
  (let ((universe (dummy-db->universe db)))
    (when show-universe?
      (fmt #t (dsp-universe universe)))
    universe))

(define (make-test-universe packages dependencies)
  (test-db->universe (make-test-db packages dependencies)))

(define (make-joint-scores db versions.score-list)
  (map (lambda (versions.score)
         (cons
          (map (lambda (pkg.version)
                 (dummy-db-version-ref db (car pkg.version) (cdr pkg.version)))
               (car versions.score))
          (cdr versions.score)))
       versions.score-list))

(define (make-version-scores db version.score-list)
  (map (lambda (version.score)
         (cons (dummy-db-version-ref db (caar version.score) (cdar version.score))
               (cdr version.score)))
       version.score-list))

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

(define-test-case solver-tests carglass ()
  (let* ((db (make-test-db
              '((car (1) 1)
                (engine (1 2 #f) #f)
                (turbo (1 #f) 1)
                (wheel (2 3 #f) #f)
                (tyre (1 2 #f) #f)
                (door (1 2 #f) #f)
                (window (0 1 2 #f) #f)
                (glass (1 2 #f) #f))
              '((car 1 -> (engine . 1) (engine . 2))
                (car 1 -> (wheel . 2) (wheel . 3))
                (car 1 -> (door . 1) (door . 2))
                (wheel 3 -> (tyre . 1) (tyre . 2))
                (door 2 -> (window . 0) (window . 1) (window . 2))
                (window 1 -> (glass . 1))
                (window 2 -> (glass . 2))
                (tyre 2 -> (glass . 1) (glass . #f)))))
         (version-scores (make-version-scores db '(((engine . 2) . 100)
                                                   ((wheel . 3) . 100)
                                                   ((tyre . 2) . 100)
                                                   ((door . 2) . 100)
                                                   ((window . 2) . 100)
                                                   ((glass . 2) . 100)))))
    (test-solutions '((10000 any)
                      (10000 any)
                      (10000 any))
      (make-solver (test-db->universe db)
                   `((version-scores . ,version-scores))))))

(define-test-case solver-tests test3 ()
  (let ((db (make-test-db '((p1 (1 2 3) 1)
                            (p2 (1 2 3) 1)
                            (p3 (1 2 3) 1)
                            (p4 (1 2 3) 1))
                          '((p1 1 -> (p2 . 2) (p2 . 3))
                            (p1 2 -> (p2 . 2) (p2 . 3))
                            (p1 3 -> (p2 . 2) (p2 . 3))
                            
                            (p2 1 -> (p3 . 1) (p3 . 2) (p3 . 3))
                            (p2 2 -> (p3 . 2) (p3 . 3))
                            (p2 1 <> (p1 . 2) (p1 . 3))
                            
                            (p3 1 -> (p4 . 1) (p4 . 2) (p4 . 3))
                            (p3 2 -> (p4 . 1) (p4 . 2) (p4 . 3))
                            (p3 3 -> (p4 . 1) (p4 . 2) (p4 . 3))))))
    (test-solutions '((10000 any)
                      (10000 any)
                      (10000 any)
                      (10000 #f))
      (make-solver (test-db->universe db)
                   `((version-scores
                      . ,(make-version-scores db '(((p2 . 3) . 100))))
                     (joint-scores
                      . ,(make-joint-scores
                          db
                          `((((p2 . 2) (p3 . 2)) . 500)
                            (((p2 . 2) (p3 . 3)) . 10000)))))))))

(when debug-output?
  (set-logger-properties! root-logger
                          `((threshold trace)
                            (handlers ,simple-log-formatter))))

(exit (run-test-suite solver-tests))

;; Local Variables:
;; scheme-indent-styles: (trc-testing as-match (test-solutions 1))
;; End:
