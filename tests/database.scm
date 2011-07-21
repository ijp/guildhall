;;; database.scm --- Tests for the package database

;; Copyright (C) 2011 Free Software Foundation, Inc.
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

;;; Code:
#!r6rs

(import (except (rnrs) delete-file file-exists?)
        (wak foof-loop)
        (spells pathname)
        (spells filesys)
        (spells logging)
        (wak trc-testing)
        (spells test-runner environment)
        (only (spells misc)
              scheme-implementation)
        (guildhall private utils)
        (guildhall inventory)
        (guildhall package)
        (guildhall destination)
        (guildhall hooks)
        (guildhall repository)
        (guildhall database))

(define debug-output? #f)


;;; Utilities

(define test-dir (->pathname '((",database-test.tmp"))))
(define dest-dir (pathname-join test-dir "dest"))

(define (assert-clear-stage)
  (when (file-exists? test-dir)
    (test-failure "working stage not clear" test-dir)))

(define (open-test-database repositories)
  (let ((db (open-database
             (pathname-join test-dir "db")
             (make-fhs-destination 'test
                                   (merge-pathnames dest-dir (working-directory)))
             repositories
             (scheme-implementation))))
    (database-add-bundle! db (pathname-join (this-directory) "bundle"))
    db))

(define (clear-stage)
  (rm-rf test-dir))


;;; Main functionality

(define-test-suite db-tests
  "Package database")

(define package:foo (make-package 'foo '((0))))
(define package:bar (make-package 'bar '((0))))
(define package:not-there (make-package 'not-there '((0))))
(define package:file-conflict-foo (make-package 'file-conflict-foo '((0))))
(define package:hook (make-package 'hook '((0))))
(define package:hook-crash (make-package 'hook-crash '((0))))

(define-test-case db-tests locking ((setup (assert-clear-stage))
                                    (teardown (clear-stage)))
  (let ((db-1 (open-test-database '()))
        (cookie (list 'cookie)))
    (test-eq cookie
      (guard (c ((database-locked-error? c)
                 cookie))
        (open-test-database '())))
    (close-database db-1)))

(define-test-case db-tests install+remove
  ((setup
    (assert-clear-stage))
   (teardown
    (clear-stage)))
  (begin
    ;; Install package
    (let ((db (open-test-database '())))
      (test-eqv #t (database-unpack! db package:foo))
      (let ((item (database-lookup db 'foo '((0)))))
        (test-eqv #t (database-item? item))
        (test-eqv #t (database-item-installed? item)))
      (close-database db))
    (let* ((db (open-test-database '()))
           (item (database-lookup db 'foo '((0)))))
      ;; Test installation correctness
      (test-eqv #t (database-item? item))
      (test-eqv #t (database-item-installed? item))
      (test-equal '(libraries ("foo" "a.scm"))
        (inventory->tree
         (package-category-inventory (database-item-package item)
                                     'libraries)))
      (test-equal `(("bin" "foo")
                    ("share"
                     ("libr6rs-foo" ("programs" "foo"))
                     ("r6rs-libs" ("foo" "a.scm"))))
        (directory->tree dest-dir))

      ;; Test removal
      (test-eqv #t (database-remove! db 'foo))
      (test-equal '()
                  (directory->tree dest-dir))
      (close-database db))
    (let* ((db (open-test-database '()))
           (item (database-lookup db 'foo '((0)))))
      (test-eqv #t (database-item? item))
      (test-eqv #f (database-item-installed? item))
      (close-database db))))

(define-test-case db-tests non-existing ((setup
                                          (assert-clear-stage))
                                         (teardown
                                          (clear-stage)))
  (call-with-database (open-test-database '())
    (lambda (db)
      (test-eqv #f (database-unpack! db package:not-there))
      (test-eqv #f (database-remove! db 'not-there)))))

(define-test-case db-tests locked ((setup (assert-clear-stage))
                                   (teardown (clear-stage)))
  (let ((exception-cookie (list 'exception)))
    (test-eq exception-cookie
      (guard (c ((database-locked-error? c)
                 exception-cookie))
        (call-with-database (open-test-database '())
          (lambda (db)
            (call-with-database (open-test-database '())
              (lambda (db-nested)
                'fail))))))))

(define-test-case db-tests file-conflict
  ((setup
    (assert-clear-stage))
   (teardown
    (clear-stage)))
  (begin
    (let ((db (open-test-database '())))
      (database-unpack! db package:foo)
      (close-database db))
    (let ((db (open-test-database '())))
      (test-equal '(conflict foo file-conflict-foo)
        (guard
            (c ((database-file-conflict? c)
                (list 'conflict
                      (package-name (database-file-conflict-package c))
                      (package-name (database-file-conflict-offender c)))))
          (database-unpack! db package:file-conflict-foo)
          'no-exception))
      
      (database-remove! db 'foo)
      (database-unpack! db package:file-conflict-foo)
      (let ((item (database-lookup db 'file-conflict-foo '((0)))))
        (test-eqv #t (database-item? item))
        (test-eqv #t (database-item-installed? item)))
      (close-database db))))

(define-test-case db-tests directory-removal
  ((setup
    (assert-clear-stage))
   (teardown
    (clear-stage)))
  (begin
    (let ((db (open-test-database '())))
      (test-eqv #t (database-unpack! db package:foo))
      (test-eqv #t (database-unpack! db package:bar))
      (close-database db))
    (test-equal `(("bin" "foo")
                  ("share"
                   ("libr6rs-foo" ("programs" "foo"))
                   ("r6rs-libs"
                    ("bar" "b.scm")
                    ("foo" "a.scm"))))
      (directory->tree dest-dir))
    (let ((db (open-test-database '())))
      (test-eqv #t (database-remove! db 'bar))
      (close-database db))
    (test-equal `(("bin" "foo")
                  ("share"
                   ("libr6rs-foo" ("programs" "foo"))
                   ("r6rs-libs" ("foo" "a.scm"))))
      (directory->tree dest-dir))))

(define-test-case db-tests setup ((setup (assert-clear-stage))
                                  (teardown (clear-stage)))
  (let ()
    (define (test-inventories db)
      (let ((item (database-lookup db 'hook 'installed)))
        (test-eqv #t (database-item? item))
        (test-equal '(libraries "test.scm")
          (inventory->tree
           (package-category-inventory (database-item-package item)
                                       'libraries)))))
    (let ((db (open-test-database '())))
      (test-eqv #t (database-unpack! db package:hook))
      (database-setup! db 'hook)
      (test-inventories db)
      (test-equal `(("share" ("r6rs-libs" "test.scm")))
        (directory->tree dest-dir))
      (close-database db))
    (call-with-database (open-test-database '())
      test-inventories)))

(define-test-case db-tests crash ((setup (assert-clear-stage))
                                  (teardown (clear-stage)))
  (let ((exception-cookie (list 'cookie)))
    (call-with-database (open-test-database '())
      (lambda (db)
        (test-eqv #t (database-unpack! db package:hook-crash))
        (test-eq exception-cookie
          (guard (c ((hook-runner-exception? c)
                     exception-cookie))
            (database-setup! db 'hook-crash)))))))


;;; Tests involving a repository

(define-test-suite (db-tests.repository db-tests)
  "Repository")

(define test-repositories
  (list (make-file-repository
         'test
         (merge-pathnames "repository" (this-directory)))))

(define package:discrepancy (make-package 'discrepancy '((1))))

(define-test-case db-tests.repository discrepancy ((setup (assert-clear-stage))
                                                   (teardown (clear-stage)))
  (let ((cookie (list 'cookie)))
    (call-with-database (open-test-database test-repositories)
      (lambda (db)
        (database-update! db)
        (test-eq cookie
          (guard (c ((database-bundle-discrepancy? c)
                     cookie))
            (database-unpack! db package:discrepancy)))))))

(when debug-output?
  (set-logger-properties!
   logger:dorodango
   `((threshold trace)
     (handlers
      ,(lambda (entry)
         (default-log-formatter entry (current-output-port)))))))

(set-test-debug-errors?! #t)
(exit (run-test-suite db-tests))

;; Local Variables:
;; scheme-indent-styles: (foof-loop trc-testing)
;; End:
