;;; dummy-db.scm --- Dependency solver, dummy database

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

;; This library provides a dummy package database for testing
;; purposes.

;;; Code:
#!r6rs

(library (guildhall solver dummy-db)
  (export make-dummy-db
          dummy-db?
          dummy-db-version-ref
          dummy-db-add-package!
          dummy-db-add-dependency!

          dummy-db->universe)
  (import (rnrs)
          (only (srfi :1) append-map filter-map delete-duplicates)
          (wak foof-loop)
          (wak riastreams)
          (spells record-types)
          (guildhall solver internals))

(define-record-type* dummy-db
  (make-dummy-db)
  ((packages '())
   (version-count 0)
   (dependency-count 0)
   (dependencies '())
   (package-table (make-eq-hashtable))))

(define (dummy-db-version-ref db name version-tag)
  (find-version/assert (dummy-db-package db name) version-tag))

(define (dummy-db-add-package! db name versions current-version-tag)
  (let* ((package-table (dummy-db-package-table db))
         (package (make-package (hashtable-size package-table)
                                name)))
    (loop ((for tag (in-list versions))
           (for version-count (up-from (dummy-db-version-count db)))
           (let version (make-version version-count tag package))
           (for versions (listing version))
           (with current-version
                 #f
                 (if (eqv? current-version-tag (version-tag version))
                     version
                     current-version)))
      => (begin
           (set-package-versions! package versions)
           (set-package-current-version! package current-version)
           (set-dummy-db-version-count! db version-count)))
    (set-dummy-db-packages! db (cons package (dummy-db-packages db)))
    (hashtable-set! package-table name package)))

(define (find-version package tag)
  (find (lambda (version)
          (eqv? tag (version-tag version)))
        (package-versions package)))

(define (dummy-db-package db name)
  (or (hashtable-ref (dummy-db-package-table db) name #f)
      (assertion-violation 'dummy-db-ref-package "unknown package" name)))

(define (inverted-dependency-targets packages targets)
  (append-map
   (lambda (package)
     (filter-map (lambda (version)
                   (and (not (memp (lambda (target)
                                     (eqv? version target))
                                   targets))
                        version))
                 (package-versions package)))
   packages))

(define (find-version/assert package tag)
  (or (find-version package tag)
      (assertion-violation
       'find-version
       "package doesn't have this version" package tag)))

(define (dummy-db-add-dependency! db source source-version conflict? targets)
  (loop ((for target (in-list targets))
         (let package (dummy-db-package db (car target)))
         (for packages (listing package))
         (for targets (listing (find-version/assert package (cdr target)))))
    => (let* ((source (find-version/assert (dummy-db-package db source)
                                           source-version))
              (targets (if conflict?
                           (inverted-dependency-targets packages targets)
                           targets))
              (dependency (make-dependency
                           (dummy-db-dependency-count db)
                           #f
                           source
                           (delete-duplicates targets version=?))))
         (version-add-dependency! source dependency)
         (for-each (lambda (target)
                     (version-add-reverse-dependency! target dependency))
                   targets)
         (set-dummy-db-dependencies! db (cons dependency (dummy-db-dependencies db)))
         (set-dummy-db-dependency-count! db (+ (dummy-db-dependency-count db) 1)))))

(define (dummy-db->universe db)
  (make-universe (list->stream (reverse (dummy-db-packages db)))
                 (hashtable-size (dummy-db-package-table db))
                 (dummy-db-version-count db)
                 (list->stream (reverse (dummy-db-dependencies db)))))

)
