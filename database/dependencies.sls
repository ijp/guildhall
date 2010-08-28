;;; dependencies.sls --- Package database <-> solver integration

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

;; The universe constructed by this library uses the database
;; package's version as `version-tag' (or #f for the "uninstalled
;; version").

;;; Code:
#!r6rs

(library (dorodango database dependencies)
  (export database->universe
          irreparable-packages

          dependency-info?
          dependency-info-package
          dependency-info-choices)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (wak foof-loop)
          (wak foof-loop nested)
          (wak riastreams)
          (spells record-types)
          (spells match)
          (spells tracing) ;debug
          (dorodango database)
          (prefix (dorodango package)
                  db:)
          (dorodango solver internals))

(define-record-type* dependency-info
  (make-dependency-info package choices)
  ())

(define (construct-package id name db-items version-count)
  (let* ((package (make-package id name))
         (uninstalled-version (make-version version-count #f package)))
    (loop ((for item (in-list db-items))
           (for version-id (up-from (+ 1 version-count)))
           (let version (make-version version-id
                                      (database-item-version item)
                                      package))
           (for versions (listing (initial (list uninstalled-version))
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
;;
;; This procedure returns two values: the constructed "universe",
;; which serves as input to the solver, and an hashtable that maps
;; package names to the universe's packages.
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
            ((for dependency-choices (in-list (db:package-dependencies db-package)))
             (let-values (targets adjusted-version-count)
               (list-dependency-targets dependency-choices version-count))
             (with dependencies '())
             (for count (up-from count))
             (with version-count version-count adjusted-version-count))
          => (values dependencies count version-count)
          (let ((dependency (make-dependency count
                                             (make-dependency-info
                                              db-package
                                              dependency-choices)
                                             version
                                             targets)))
            (version-add-dependency! version dependency)
            (loop ((for target (in-list targets)))
              (version-add-reverse-dependency! target dependency))
            (continue (=> dependencies (cons dependency dependencies)))))))
    
    (define (list-dependency-targets dependency-choices version-count)
      (iterate-values ((targets '()) (version-count version-count))
          (for choice (in-list dependency-choices))
          (let-values (package updated-version-count)
            (get/create-package (db:dependency-choice-target choice) version-count))
          (let constraint (db:dependency-choice-version-constraint choice))
          (for version (in-list (package-versions package)))
          (if (and-let* ((db-version (version-tag version)))
                (db:version-constraint-satisfied? constraint db-version)))
        (values (cons version targets) updated-version-count)))
    
    (define (get/create-package package-name version-count)
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
               (values package (+ version-count 1))))))
    
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
