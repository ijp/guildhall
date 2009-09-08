;;; database.scm --- Tests for the package database

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(define test-dir (->pathname '((",database-test.tmp"))))

(define (assert-clear-stage)
  (when (file-exists? test-dir)
    (test-failure "working stage not clear" test-dir)))

(define (open-test-database)
  (let ((db (open-database (pathname-join test-dir "db")
                           (make-fhs-destination 'test (pathname-join test-dir "dest"))
                           '())))
    (database-add-bundle! db (pathname-join (this-directory) "bundle"))
    db))

(define (delete-recursively pathname)
  (when (file-directory? pathname)
    (let ((directory (pathname-as-directory pathname)))
      (loop ((for filename (in-directory directory)))
        (delete-recursively (pathname-with-file directory filename)))))
  (delete-file pathname))

(define (clear-stage)
  (delete-recursively test-dir))

(define-test-suite db-tests
  "Package database")

(define-test-case db-tests install
  ((setup
    (assert-clear-stage))
   #;
   (teardown
    (clear-stage)))
  (begin
    (let ((db (open-test-database)))
      (database-install! db 'foo '())
      (close-database db))
    (let* ((db (open-test-database))
           (item (database-find db 'foo '())))
      (test-eqv #t (database-item? item))
      (test-eqv #t (database-item-installed? item))
      (test-equal '(libraries ("foo" "a.sls"))
        (inventory->tree
         (package-category-inventory (database-item-package item)
                                     'libraries))))))

(run-test-suite db-tests)

;; Local Variables:
;; scheme-indent-styles: (foof-loop trc-testing)
;; End:
