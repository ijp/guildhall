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

;; The package database keeps track of installed and available
;; packages, and allows to install and remove individual packages. It
;; does *not* deal with package dependencies.

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
          in-database
          
          database-install!
          database-remove!

          (rename (item? database-item?)
                  (item-package database-item-package)
                  (item-name database-item-name)
                  (item-version database-item-version)
                  (item-installed? database-item-installed?))

          database-file-conflict?
          database-file-conflict-package
          database-file-conflict-offender
          database-file-conflict-pathname)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) filter-map)
          (srfi :8 receive)
          (only (srfi :13) string-suffix?)
          (spells foof-loop)
          (spells pathname)
          (spells filesys)
          (spells fmt)
          (only (spells record-types)
                define-functional-fields)
          (spells logging)
          (spells tracing)
          (dorodango private utils)
          (dorodango inventory)
          (dorodango bundle)
          (dorodango package)
          (dorodango repository)
          (dorodango destination))

;;; Data types and conditions

(define-record-type database
  (fields directory
          destination
          repositories
          file-table
          pkg-table))

(define-record-type item
  (fields package state sources bundle))

(define-functional-fields item
  package state sources bundle)

(define (item-name item)
  (package-name (item-package item)))

(define (item-version item)
  (package-version (item-package item)))

(define (item-installed? item)
  (eq? 'installed (item-state item)))

(define-record-type source
  (fields repository location))

(define-condition-type &database-file-conflict &error
  make-file-conflict database-file-conflict?
  (package database-file-conflict-package)
  (offender database-file-conflict-offender)
  (pathname database-file-conflict-pathname))

(define (file-conflict item package pathname)
  (raise (condition (make-file-conflict item package pathname))))


;;; Database loading

(define (open-database directory destination repositories)
  (let* ((directory (pathname-as-directory directory))
         (status-directory (status-subdirectory directory destination)))
    (unless (file-exists? status-directory)
      (create-directory* status-directory))
    (receive (pkg-table file-table)
             (load-installed-packages status-directory destination)
      (let ((db (make-database directory
                               destination
                               repositories
                               file-table
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

(define (load-installed-packages directory destination)
  (define (lose msg . irritants)
    (apply error 'load-installed-packages msg irritants))
  (let ((pkg-table (make-eq-hashtable))
        (file-table (make-hashtable pathname-hash pathname=?)))
    (loop ((for filename (in-directory directory)))
      => (values pkg-table file-table)
      (when (string-suffix? ".info" filename)
        (let ((package (load-package-info
                        (pathname-with-file directory filename))))
          (hashtable-update! pkg-table
                             (package-name package)
                             (lambda (items)
                               (if items
                                   (lose "duplicate package in status file" )
                                   (list (make-item package 'installed '() #f))))
                             #f)
          (update-file-table! file-table
                              destination
                              package))))))

(define (update-file-table! table destination package)
  (define (fill! directory category inventory)
    (loop ((for cursor (in-inventory inventory)))
      (let ((pathname (pathname-with-file directory (inventory-name cursor))))
        (cond ((destination-pathname destination package category pathname)
               => (lambda (real-pathname)
                    (hashtable-update!
                     table
                     real-pathname
                     (if (inventory-leaf? cursor)
                         (lambda (value)
                           (if value
                               (file-conflict value
                                              package
                                              real-pathname)
                               package))
                         (lambda (value)
                           (cond ((package? value)
                                  (file-conflict value
                                                 package
                                                 real-pathname))
                                 (else
                                  (fill! (pathname-as-directory pathname)
                                         category
                                         cursor)
                                  (cons package (or value '()))))))
                     #f)))))))
  (loop ((for inventory (in-list (package-inventories package))))
    (let ((category (inventory-name inventory)))
      (fill! (make-pathname #f '() #f) category inventory))))

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
        (loop ((for form (in-port port read))
               (for inventories (listing (tree->inventory form name))))
          => (package-with-inventories package inventories))))))


;;; Source manipulation

(define (database-add-bundle! db pathname)
  (let ((bundle (open-input-bundle pathname (bundle-options no-inventory))))
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
      (if (package=? package (item-package item))
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

;; This is not yet used
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


;;; Querying

(define (database-find db package)
  (let ((version (package-version package)))
    (find (lambda (item)
            (package-version=? version (package-version (item-package item))))
          (hashtable-ref (database-pkg-table db) (package-name package) '()))))

(define (database-search db name version)
  (let ((items (hashtable-ref (database-pkg-table db) name '())))
    (if (not version)
        items
        (filter (lambda (item)
                  (version-match? (item-package item) version))
                items))))

(define (version-match? package version)
  (or (not version)
      (package-version=? (package-version) version)))

(define-syntax in-database
  (syntax-rules (sorted-by)
    ((_ (name-var items-var) (db-expr) cont . env)
     (cont
      (((name-vec items-vec)                       ;Outer bindings
        (hashtable-entries (database-pkg-table db-expr))))
      ((i (- (vector-length name-vec) 1) (- i 1))) ;Loop variables
      ()                                           ;Entry bindings
      ((< i 0))                                    ;Termination conditions
      (((name-var) (vector-ref name-vec i))        ;Body bindings
       ((items-var) (vector-ref items-vec i)))     
      ()                                           ;Final bindings
      . env))
    ((_ (name-var items-var) (db-expr (sorted-by <?)) cont . env)
     (cont
      (((table size name-vec)                           ;Outer bindings
        (let ((table (database-pkg-table db-expr)))
          (values table
                  (hashtable-size table)
                  (vector-sort <? (hashtable-keys table))))))
      ((i 0 (+ i 1)))                              ;Loop variables
      ()                                           ;Entry bindings
      ((= i size))                                 ;Termination conditions
      (((name-var items-var)                       ;Body bindings
        (let ((name (vector-ref name-vec i)))
          (values name (hashtable-ref table name #f)))))
      ()                                           ;Final bindings
      . env))))

(define database-update!
  (case-lambda
    ((db package proc default)
     (let ((version (package-version package)))
       (define (update-items items)
         (loop continue ((for item (in-list items))
                         (with result '() (cons item result))
                         (with found? #f))
           => (reverse (cond (found?  result)
                             (default (cons default result))
                             (else
                              (assertion-violation
                               'database-update!
                               "requested item not found and no default provided"
                               db package))))
           (if (package-version=? version (package-version (item-package item)))
               (continue (=> found? #t)
                         (=> result 
                             (cond ((proc item)
                                    => (lambda (replacement)
                                         (cons replacement result)))
                                   (else
                                    result))))
               (continue))))
       (hashtable-update! (database-pkg-table db)
                          (package-name package)
                          update-items
                          '())))
    ((db package proc)
     (database-update! db package proc #f))))


;;; Installation & removal

(define (database-package-info-pathname db package)
  (pathname-with-file (database-status-directory db)
                      (make-file (package-name package) "info")))

(define (database-install! db package)
  (define (lose msg . irritants)
    (apply error 'database-install! msg irritants))
  (define (do-install! desired-item)
    (let* ((bundle (open-bundle! db desired-item))
           (package (bundle-package-ref bundle package)))
      (log/db 'info (cat "installing " (package-identifier package)))
      (update-file-table! (database-file-table db)
                          (database-destination db)
                          package)
      (extract-package bundle package (database-destination db))
      (save-package-info (database-package-info-pathname db package) package)
      (database-update! db package (lambda (item)
                                     (item-with-state item 'installed)))))
  (let ((items (hashtable-ref (database-pkg-table db)
                              (package-name package)
                              '()))
        (desired-item (database-find db package)))
    (cond ((not desired-item)
           (lose "no such package in database" package))
          ((exists (lambda (item)
                     (and (item-installed? item)
                          (not (eq? item desired-item))))
                   items)
           => (lambda (item)
                (lose "another version of package already installed" item)))
          ((not (item-installed? desired-item))
           (do-install! desired-item)))))

(define (open-bundle! db item)
  (let ((sources (item-sources item)))
    ;; TODO: what to do with other sources here?
    (when (null? sources)
      (error 'open-bundle! "no sources for package" (item-package item)))
    (let* ((source (car sources))
           (bundle (open-input-bundle (repository-fetch-bundle
                                       (source-repository source)
                                       (source-location source)))))
      (loop ((for package (in-list (bundle-packages bundle))))
        (database-update! db
                          package
                          (lambda (item)
                            (make-item package
                                       (item-state item)
                                       (item-sources item)
                                       bundle))
                          (make-item package 'available (list source) #f)))
      bundle)))

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
                  (let ((filename (->namestring destination-pathname)))
                    (log/db 'debug "installing " filename)
                    (call-with-port (open-file-output-port filename)
                      extractor))))))
    (let ((inventory (package-category-inventory package category)))
      (bundle-walk-inventory bundle inventory extract-file))))

(define managed-categories '(libraries documentation programs))

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

(define (database-remove! db package)
  (define (lose msg . irritants)
    (apply error 'database-remove! msg irritants))
  (let ((item (or (database-find db package)
                  (lose "no such package in database" package))))
    (when (item-installed? item)
      (let ((package (item-package item)))
        (log/db 'info (cat "removing " (package-identifier package)))
        (remove-package-files! db package)
        (delete-file (database-package-info-pathname db package))
        (database-update! db
                          package
                          (lambda (item)
                            (if (null? (item-sources item))
                                #f
                                (item-with-state item 'available))))))))

(define (remove-package-files! db package)
  (let ((file-table (database-file-table db))
        (destination (database-destination db)))
    (define (delete-inventory category inventory path)
      (loop continue ((for cursor (in-inventory inventory)))
        (let ((pathname (destination-pathname
                         destination
                         package
                         category
                         (make-pathname #f path (inventory-name cursor)))))
          (when pathname
            (cond ((inventory-container? cursor)
                   (delete-inventory category
                                     cursor
                                     (append path
                                             (list (inventory-name cursor))))
                   (let ((packages
                          (remp (lambda (providing-package)
                                  (package=? providing-package
                                             package))
                                (hashtable-ref file-table pathname #f))))
                     (cond ((null? packages)
                            (log/db 'debug (cat "removing directory "
                                                (dsp-pathname pathname)))
                            (delete-file pathname)
                            (hashtable-delete! file-table pathname))
                           (else
                            (hashtable-set! file-table pathname packages)))))
                  (else
                   (log/db 'debug (cat "deleting " (dsp-pathname pathname)))
                   (delete-file pathname)
                   (hashtable-delete! file-table pathname))))
          (continue))))
    (loop ((for category (in-list (package-categories package))))
      (let ((package-inventory (package-category-inventory package category)))
        (delete-inventory category package-inventory '())))))

(define logger:dorodango.db (make-logger logger:dorodango 'db))
(define log/db (make-fmt-log logger:dorodango.db))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
