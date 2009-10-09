;;; dependencies.sls --- Package database <-> solver integration

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
#!r6rs

(library (dorodango database dependencies)
  (export database->universe
          irreparable-packages)
  (import (rnrs)
          (srfi :8 receive)
          (spells foof-loop)
          (spells lazy-streams)
          (spells match)
          (spells tracing)
          (dorodango database)
          (prefix (dorodango package)
                  db:)
          (dorodango solver internals))

(define (construct-package id name db-items version-count)
  (let* ((package (make-package id name))
         (uninstalled-version (make-version version-count #f package)))
    (loop ((for item (in-list db-items))
           (for version-id (up-from (+ 1 version-count)))
           (let version (make-version version-id
                                      (database-item-version item)
                                      package))
           (for versions (listing-reverse (initial (list uninstalled-version))
                                          version))
           (with current-version
                 uninstalled-version
                 (if (database-item-installed? item)
                     version
                     current-version)))
      => (begin
           (set-package-versions! package versions)
           (set-package-current-version! package current-version)
           package))))

;; Construct the solver universe from a package database. This is a
;; two step process: first, all packages and their versions are
;; extracted from the database, then, for each version in the
;; database, the dependencies are computed. Note that the latter step
;; may also add packages (with "uninstalled" as single version), as
;; dependencies might refer to packages that are not known to the
;; database.
(define (database->universe database)
  (let ((package-table (make-eq-hashtable)))
    (define (list-dependencies versions db-items version-count)
      (loop ((for version (in-list versions))
             (for db-item (in-list db-items))
             (let-values (version-dependencies new-count adjusted-version-count)
               (construct-dependencies version db-item count version-count))
             (for dependencies (appending-reverse version-dependencies))
             (with count 0 new-count)
             (with version-count version-count adjusted-version-count))
        => (values dependencies version-count)))
    (define (construct-dependencies version db-item count version-count)
      (let ((db-package (database-item-package db-item)))
        (loop continue
            ((for depend-clause
                  (in-list (db:package-property db-package 'depends '())))
             (let-values (targets adjusted-version-count)
               (match depend-clause
                 ((package-name)
                  (list-dependency-targets package-name version-count))
                 (_
                  (error 'database->universe
                         "unrecognized dependency clause"
                         depend-clause))))
             (with dependencies '())
             (for count (up-from count))
             (with version-count version-count adjusted-version-count))
          => (values dependencies count version-count)
          (let ((dependency (make-dependency count version targets)))
            (version-add-dependency! version dependency)
            (loop ((for target (in-list targets)))
              (version-add-reverse-dependency! target dependency))
            (continue (=> dependencies (cons dependency dependencies)))))))
    (define (list-dependency-targets package-name version-count)
      (receive (package version-count)
               (cond ((hashtable-ref package-table package-name #f)
                      => (lambda (package)
                           (values package version-count)))
                     (else
                      (let ((package (make-package (hashtable-size package-table)
                                                   package-name)))
                        (set-package-versions!
                         package
                         (list (make-version version-count #f package)))
                        (hashtable-set! package-table package-name package)
                        (values package (+ version-count 1)))))
        (values (filter version-tag (package-versions package))
                version-count)))
    (loop ((for package-name db-items (in-database database))
           (let package (construct-package (hashtable-size package-table)
                                           package-name
                                           db-items
                                           version-count))
           (for versions (appending-reverse
                          (filter version-tag (package-versions package))))
           (for item-list (appending-reverse db-items))
           (with version-count 0 (+ version-count
                                    (length (package-versions package)))))
      => (let*-values (((dependencies version-count)
                        (list-dependencies versions item-list version-count))
                       ((package-names packages)
                        (hashtable-entries package-table)))
           (values
             (make-universe (vector->stream packages)
                            (hashtable-size package-table)
                            version-count
                            (list->stream dependencies))
             package-table))
      (hashtable-set! package-table package-name package))))


(define (unresolvable-dependencies universe)
  (loop ((for dependency (in-stream (universe-dependency-stream universe)))
         (for result
              (listing-reverse dependency
                               (if (null? (dependency-targets dependency))))))
    => result))

(define (irreparable-packages universe)
  (let ((package-table (make-eq-hashtable)))
    (define (mark-package-and-reverse-deps version)
      (unless (hashtable-ref package-table (version-package version) #f)
        (hashtable-set! package-table (version-package version) #t)
        (loop ((for reverse-dep (in-list (version-reverse-dependencies version))))
          (mark-package-and-reverse-deps (dependency-source reverse-dep)))))
    (loop ((for dependency (in-list (unresolvable-dependencies universe))))
      (let ((source (dependency-source dependency)))
        (when (version=? source (package-current-version (version-package source)))
          (mark-package-and-reverse-deps source))))
    (map (lambda (package)
           (db:make-package (package-name package)
                            (version-tag (package-current-version package))))
         (vector->list (hashtable-keys package-table))))))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
