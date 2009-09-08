;;; database.sls --- Dorodango package database

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

(library (dorodango database)
  (export database?
          open-database
          close-database
          
          database-add-bundle!
          database-add-bundles!
          database-find
          database-search

          database-update!
          database-install!
          (rename (item? database-item?)
                  (item-package database-item-package)
                  (item-name database-item-name)
                  (item-version database-item-version)
                  (item-installed? database-item-installed?)))
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) filter-map)
          (srfi :8 receive)
          (only (srfi :13) string-suffix?)
          (spells foof-loop)
          (spells pathname)
          (spells filesys)
          (only (spells record-types)
                define-functional-fields)
          (dorodango inventory)
          (dorodango bundle)
          (dorodango package)
          (dorodango repository)
          (dorodango destination))

(define-record-type database
  (fields directory
          destination
          repositories
          inventories
          pkg-table))

(define managed-categories '(libraries documentation programs))

(define (open-database directory destination repositories)
  (let* ((directory (pathname-as-directory directory))
         (status-directory (status-subdirectory directory destination)))
    (unless (file-exists? status-directory)
      (create-directory* status-directory))
    (receive (pkg-table inventories)
             (load-installed-packages status-directory)
      (let ((db (make-database directory
                               destination
                               repositories
                               inventories
                               pkg-table)))
        (load-available-files! db directory repositories)
        db))))

(define (status-subdirectory directory destination)
  (pathname-join directory `(("status" ,(destination-name destination)))))

(define (database-status-directory db)
  (status-subdirectory (database-directory db) (database-destination db)))

(define (close-database db)
  ;; TODO: mark the database as closed, so that future operations will
  ;; fail
  (let ((closed-bundles (make-eq-hashtable))
        (pkg-table (database-pkg-table db)))
    (receive (pkg-names items-vector) (hashtable-entries pkg-table)
      (loop ((for items (in-vector items-vector)))
        (loop ((for item (in-list items)))
          (cond ((item-bundle item)
                 => (lambda (bundle)
                      (unless (hashtable-ref closed-bundles bundle #f)
                        (close-bundle bundle)
                        (hashtable-set! closed-bundles bundle #t))))))))
    (hashtable-clear! pkg-table)))

(define (load-available-files! db cache-dir repositories)
  (loop ((for repository (in-list repositories)))
    (let ((pathname (repository-available-pathname repository)))
      (load-available-file! db repository pathname))))

(define (remove-repositories! db repositories)
  (define (item-without-repositories item)
    (let ((remaining (filter (lambda (source)
                               (not (memq (source-repository source)
                                          repositories)))
                             (item-sources item))))
      (if (and (null? remaining)
               (eq? 'available (item-state item)))
          #f
          (item-with-sources item remaining))))
  (let ((pkg-table (database-pkg-table db)))
    (loop ((for pkg-name (in-vector (hashtable-keys pkg-table))))
      (hashtable-update! pkg-table
                         pkg-name
                         (lambda (items)
                           (filter-map item-without-repositories items))
                         '()))))

(define (load-available-file! db repository pathname)
  (define (lose msg . irritants)
    (apply error 'load-available-file! msg (cons pathname irritants)))
  (call-with-input-file (->namestring pathname)
    (lambda (port)
      (loop ((for form (in-port port read)))
        (cond ((parse-package-form form (lambda (properties) '()))
               => (lambda (package)
                    (add-package-source!
                     db
                     package
                     (make-source
                      repository
                      (or (package-property package 'location #f)
                          (lose "missing location in available file" form))))))
              (else
               (lose "invalid package form in available file" form)))))))

(define (load-installed-packages directory)
  (define (lose msg . irritants)
    (apply error 'load-installed-packages msg irritants))
  (let ((pkg-table (make-eq-hashtable))
        (empty-inventories (map (lambda (category)
                                  (make-inventory category 'category))
                                managed-categories)))
    (loop continue ((for filename (in-directory directory))
                    (with inventories empty-inventories))
      => (values pkg-table inventories)
      (if (string-suffix? ".info" filename)
          (let ((package (load-package-info
                          (pathname-with-file directory filename))))
            (hashtable-update! pkg-table
                               (package-name package)
                               (lambda (items)
                                 (if items
                                     (lose "duplicate package in status file" )
                                     (list (make-item package 'installed '() #f))))
                               #f)
            (continue
             (=> inventories (merge-inventory-lists
                              inventories
                              (package-inventories package)))))
          (continue)))))

(define (load-package-info pathname)
  (call-with-input-file (->namestring pathname)
    (lambda (port)
      (let* ((package-form (get-datum port))
             (package (or (parse-package-form package-form (lambda (properties) '()))
                          (error 'load-package-info
                                 "invalid package form encountered"
                                 package-form
                                 pathname)))
             (name (package-name package)))
        (loop ((for form (in-file (->namestring pathname) read))
               (for inventories (listing (tree->inventory form name))))
          => (package-with-inventories package inventories))))))

(define (save-package-info pathname package)
  (call-with-output-file/atomic (->namestring pathname)
    (lambda (port)
      (put-datum port (package->form package))
      (put-string port "\n\n")
      (loop ((for category (in-list managed-categories)))
        (put-datum port
                   (cond ((package-category-inventory package category)
                          => inventory->tree)
                         (else
                          (list category))))
        (put-string port "\n")))))

(define (merge-inventory-lists a-inventories b-inventories)
  (define (conflict a b)
    (error 'merge-inventories "conflict!" a b))
  (loop continue ((for a-inventory (in-list a-inventories))
                  (with result '()))
    => (reverse result)
    (cond ((find-inventory b-inventories (inventory-name a-inventory))
           => (lambda (b-inventory)
                (continue
                 (=> result
                     (cons (merge-inventories a-inventory b-inventory conflict)
                           result)))))
          (else
           (continue)))))

(define (find-inventory inventories name)
  (find (lambda (inventory)
          (eq? name (inventory-name inventory)))
        inventories))


(define-record-type item
  (fields package state sources bundle))

(define-functional-fields item
  package state sources bundle)

(define (item-name item)
  (package-name (item-package item)))

(define (item-version item)
  (package-version (item-package item)))

(define-record-type source
  (fields repository location))

(define (database-add-bundle! db pathname)
  (let ((bundle (open-directory-input-bundle pathname
                                             (bundle-options no-inventory))))
    (loop ((for package (in-list (bundle-packages bundle))))
      (add-package-source! db
                           package
                           (make-source null-repository pathname)))
    (close-bundle bundle)))

(define (database-add-bundles! db pathnames)
  (loop ((for pathname (in-list pathnames)))
    (database-add-bundle! db pathname)))

(define (add-package-source! db package source)
  (define (update-items items)
    (loop continue ((for item (in-list items))
                    (with result '() (cons item result))
                    (with found? #f))
      => (reverse
          (if found?
              result
              (cons (make-item package 'available (list source) #f)
                    result)))
      (if (same-package? package (item-package item))
          (continue
           (=> found? #t)
           (=> result
               (cons (item-modify-sources
                      item
                      (lambda (sources) (append sources (list source))))
                     result)))
          (continue))))
  (hashtable-update! (database-pkg-table db)
                     (package-name package)
                     update-items
                     '()))

(define (version-match? package version)
  (or (not version)
      (package-version=? (package-version) version)))

(define (same-package? p1 p2)
  (and (eq? (package-name p1)
            (package-name p2))
       (equal? (package-version p1)
               (package-version p2))))

(define (database-search db name version)
  (filter (lambda (item)
            (version-match? (item-package item) version))
          (hashtable-ref (database-pkg-table db) name '())))

(define database-update!
  (case-lambda
    ((db name version proc default)
     (define (update-items items)
       (loop continue ((for item (in-list items))
                       (with result '() (cons item result))
                       (with found? #f))
         => (reverse (cond (found? result)
                           (default
                            (cons default result))
                           (else
                            (assertion-violation
                             'database-update!
                             "requested item not found and no default provided"
                             db name version))))
         (if (package-version=? version (package-version (item-package item)))
             (continue (=> found? #t)
                       (=> result (cons (proc item) result)))
             (continue))))
     (hashtable-update! (database-pkg-table db)
                        name
                        update-items
                        '()))
    ((db name version proc)
     (database-update! db name version proc #f))))

(define (open-bundle! db item)
  (let ((sources (item-sources item)))
    ;; TODO: what to do with other sources here?
    (when (null? sources)
      (error 'open-bundle! "no sources for package" (item-package item)))
    (let* ((source (car sources))
           (bundle (open-directory-input-bundle
                    (repository-fetch-bundle
                     (source-repository source)
                     (source-location source)))))
      (loop ((for package (in-list (bundle-packages bundle))))
        (database-update! db
                          (package-name package)
                          (package-version package)
                          (lambda (item)
                            (make-item package
                                       (item-state item)
                                       (item-sources item)
                                       bundle))
                          (make-item package 'available (list source) #f)))
      bundle)))

(define (database-find db name version)
  (find (lambda (item)
          (package-version=? (package-version (item-package item))
                             version))
        (hashtable-ref (database-pkg-table db) name '())))

(define (item-installed? item)
  (eq? 'installed (item-state item)))

(define (database-install! db name version)
  (define (lose msg . irritants)
    (apply error 'database-install! msg irritants))
  (define (do-install! desired-item)
    (let* ((bundle (open-bundle! db desired-item))
           (package (bundle-package-ref bundle name version))
           (status-dir (database-status-directory db)))
      (for-each display (list "installing " (package-identifier package) "\n"))
      (extract-package bundle package (database-destination db))
      (save-package-info (pathname-with-file status-dir (make-file name "info"))
                         package)
      
      (database-update! db name version (lambda (item)
                                          (item-with-state item 'installed)))))
  (let ((items (hashtable-ref (database-pkg-table db) name '()))
        (desired-item (database-find db name version)))
    (cond ((not desired-item)
           (lose "no such package in database" name version))
          ((exists (lambda (item)
                     (and (item-installed? item)
                          (not (eq? item desired-item))))
                   items)
           => (lambda (item)
                (lose "another version of package already installed" item)))
          ((not (item-installed? desired-item))
           (do-install! desired-item)))))

(define (extract-package bundle package destination)
  (loop ((for category (in-list (package-categories package))))
    (define (extract-file pathname extractor)
      (cond ((destination-pathname destination
                                   package
                                   category
                                   pathname)
             => (lambda (destination-pathname)
                  (create-directory*
                   (pathname-with-file destination-pathname #f))
                  (call-with-port (open-file-output-port
                                   (->namestring destination-pathname))
                    extractor)))))
    (let ((inventory (package-category-inventory package category)))
      (bundle-walk-inventory bundle inventory extract-file))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
