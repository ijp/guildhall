;;; actions.scm --- Tests for the action library

;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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
        (spells pathname)
        (spells filesys)
        (wak trc-testing)
        (spells test-runner environment)
        (guildhall private utils)
        (guildhall private zip)
        (guildhall inventory)
        (guildhall actions))

(define-test-suite actions-tests
  "Action library")

(define test-dir (->pathname '((",actions-test.tmp"))))

(define (setup-stage)
  (when (file-exists? test-dir)
    (test-failure "working stage not clear" test-dir))
  (create-directory test-dir))

(define (clear-stage)
  (rm-rf test-dir))


(define-test-suite (actions-tests.create-bundle actions-tests)
  "create-bundle")

(define (run-create-bundle bundle-filename directories)
  (create-bundle bundle-filename
                 directories
                 (read-package-lists (find-pkg-list-files directories) '())
                 #f)
  (call-with-port (open-file-input-port (->namestring bundle-filename))
    (lambda (port)
      (sort-tagged-tree (inventory->tree (zip-port->inventory port))
                        string<?))))

(define-test-case actions-tests.create-bundle single-dir/multi-pkg
  ((setup    (setup-stage))
   (teardown (clear-stage)))
  (let ((bundle-dir (pathname-join (this-directory) '(("bundle"))))
        (bundle-filename (pathname-with-file test-dir '("test-bundle-1" "zip"))))
    (test-equal '(root ("test-bundle-1"
                        "core.scm"
                        "pkg-list.scm"
                        ("programs" "multi.sps")))
      (run-create-bundle bundle-filename
                         (list (pathname-join bundle-dir '(("multi"))))))))


(define-test-case actions-tests.create-bundle multi-dir
  ((setup    (setup-stage))
   (teardown (clear-stage)))
  (let ((bundle-dir (pathname-join (this-directory) '(("bundle"))))
        (bundle-filename (pathname-with-file test-dir '("test-bundle-2" "zip"))))
    (test-equal '(root ("test-bundle-2"
                        ("bar" "b.scm" "pkg-list.scm")
                        ("foo"
                         ("libraries" "a.scm")
                         "pkg-list.scm"
                         ("programs" "foo.sps"))))
      (run-create-bundle bundle-filename
                         (list (pathname-join bundle-dir '(("foo")))
                               (pathname-join bundle-dir '(("bar"))))))))

(set-test-debug-errors?! #t)

(exit (run-test-suite actions-tests))

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
