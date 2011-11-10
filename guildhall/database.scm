;;; database.scm --- Dorodango package database

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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
;;
;; The main data structure is the package table, which maps package
;; names (symbols) to lists of database items. Those lists are always
;; kept sorted such that the newest item (according to the package
;; version) comes first.
;;
;; The database also keeps a cache of downloaded bundles, storing them
;; in subdirectories named after the locations of the respective
;; repositories.

;;; Code:
#!r6rs

(library (guildhall database)
  (export database?
          open-database
          close-database
          call-with-database
          
          database-add-bundle!
          database-add-bundles!
          database-reap-bundles

          database-package-names
          database-items
          database-lookup
          in-database

          database-update!
          database-unpack!
          database-setup!
          database-remove!

          database-clear-cache!
          
          (rename (item? database-item?)
                  (item-package database-item-package)
                  (item-name database-item-name)
                  (item-version database-item-version)
                  (item-state database-item-state)
                  (item-installed? database-item-installed?))

          database-file-conflict?
          database-file-conflict-package
          database-file-conflict-offender
          database-file-conflict-pathname

          database-locked-error?
          locked-database-pathname

          database-corruption-error?
          corrupt-database-pathname

          database-unavailable-package-error?
          database-unavailable-package

          database-bundle-discrepancy?
          database-discrepant-bundle

          logger:dorodango.db)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) append-reverse filter-map lset-adjoin)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13) string-map string-suffix?)
          (srfi :67 compare-procedures)
          (guildhall ext foof-loop)
          (guildhall ext foof-loop nested)
          (only (guile) assq-ref)
          (guildhall spells pathname)
          (guildhall spells filesys)
          (guildhall ext fmt)
          (ice-9 match)
          (only (guildhall spells record-types)
                define-functional-fields)
          (guildhall spells logging)
          (guildhall private utils)
          (guildhall inventory)
          (guildhall bundle)
          (guildhall package)
          (guildhall repository)
          (guildhall destination)
          (guildhall hooks))

;;; Data types and conditions

(define-record-type database
  (fields directory
          destination
          repositories
          cache-dir
          file-table
          pkg-table
          (mutable closed?)
          (mutable hook-runner)))

(define-record-type item
  (fields package state sources bundle))

(define-functional-fields item
  package state sources bundle)

(define (item-name item)
  (package-name (item-package item)))

(define (item-version item)
  (package-version (item-package item)))

(define (item-installed? item)
  (and (memq (item-state item) '(unpacked installed)) #t))

(define-record-type source
  (fields repository location))

(define-condition-type &database-file-conflict &error
  make-file-conflict database-file-conflict?
  (package database-file-conflict-package)
  (offender database-file-conflict-offender)
  (pathname database-file-conflict-pathname))

(define (file-conflict item package pathname)
  (raise (condition (make-file-conflict item package pathname))))

(define-condition-type &database-locked &error
  make-database-locked-error database-locked-error?
  (pathname locked-database-pathname))

(define-condition-type &database-corruption-error &error
  make-database-corruption-error database-corruption-error?
  (pathname corrupt-database-pathname))

(define (database-corruption who pathname message . irritants)
  (raise (condition (make-who-condition who)
                    (make-database-corruption-error pathname)
                    (make-message-condition message)
                    (make-irritants-condition irritants))))

(define-condition-type &database-unavailable-package &error
  make-database-unavailable-package-error database-unavailable-package-error?
  (package database-unavailable-package))

(define-condition-type &database-bundle-discrepancy &error
  make-database-bundle-discrepancy database-bundle-discrepancy?
  (bundle database-discrepant-bundle))


;;; Database loading

;;@ Open a package database.
(define open-database
  (case-lambda
    ((directory destination repositories cache-dir)
     (log/db 'debug "opening database in " (dsp-pathname directory) " ...")
     (let* ((directory (pathname-as-directory directory))
            (status-directory (status-subdirectory directory))
            (new? (not (file-exists? status-directory))))
       (let ((db (make-database directory
                                destination
                                repositories
                                cache-dir
                                (make-hashtable pathname-hash
                                                pathname=?) ;file-table
                                (make-eq-hashtable) ;pkg-table
                                #f ;closed?
                                #f ;hook-runner
                                )))
         (when new?
           (create-directory* status-directory))
         (lock-database-directory directory)
         (load-installed-packages! db)
         (load-available-files! db repositories)
         (iterate! (for repository (in-list repositories))
           (create-directory* (database-cache-directory db repository)))
         (when new?
           (call-with-destination-support-bundle
               destination
             '()
             (lambda (bundle)
               (database-add-bundle! db bundle)
               (database-unpack! db package:destination-support)
               (database-setup! db (package-name package:destination-support))
               (database-reap-bundles db))))
         db)))
    ((directory destination repositories)
     (let ((directory (pathname-as-directory directory)))
       (open-database directory
                      destination
                      repositories
                      (pathname-join directory '(("cache"))))))))

(define (database-cache-directory db repo)
  (pathname-join (database-cache-dir db)
                 (make-pathname #f
                                (list (encoded-repository-location repo))
                                #f)))

(define (encoded-repository-location repo)
  (string-map (lambda (c)
                (if (or (char-alphabetic? c)
                        (char-numeric? c))
                    c
                    #\_))
              (repository-location repo)))

(define (status-subdirectory directory)
  (pathname-join directory `(("status"))))

(define (database-status-directory db)
  (status-subdirectory (database-directory db)))

(define (lock-database-directory directory)
  (unless (create-lock-file (pathname-with-file directory "lock"))
    (raise (make-database-locked-error directory))))

(define (unlock-database-directory directory)
  (delete-file (pathname-with-file directory "lock")))

;;@ Close a previously opened database.
(define (close-database db)
  (guarantee-open-database 'close-database db)
  (database-reap-bundles db)
  (unlock-database-directory (database-directory db))
  (database-closed?-set! db #t))


;; Close all bundles referenced by @var{db}.
(define (database-reap-bundles db)
  (let ((closed-bundles (make-eq-hashtable))
        (pkg-table (database-pkg-table db)))
    (define (item-without-bundle item)
      (cond ((item-bundle item)
             => (lambda (bundle)
                  (unless (hashtable-ref closed-bundles bundle #f)
                    (close-bundle bundle)
                    (hashtable-set! closed-bundles bundle #t))
                  (item-with-bundle item #f)))
            (else
             item)))
    (iterate! (for pkg-name (in-vector (hashtable-keys pkg-table)))
      (hashtable-update! pkg-table
                         pkg-name
                         (lambda (items)
                           (map item-without-bundle items))
                         '()))))

;;@ Calls @var{proc} with @var{db} as argument and closes the database
;; upon return of the call to @var{proc}. The database is @emph{not}
;; closed when @var{proc} is exited via a continuation or exception.
(define (call-with-database db proc)
  (receive results (proc db)
    (close-database db)
    (apply values results)))

(define (guarantee-open-database who db)
  (unless (database? db)
    (assertion-violation who "expected database" db))
  (when (database-closed? db)
    (assertion-violation who "database has already been closed" db))
  db)

(define (load-available-files! db repositories)
  ;; The repositories are loaded in reverse order, as later-added
  ;; sources will be preferred over earlier-added ones, and we want
  ;; the resulting source order correspond to the order of the
  ;; repositories.
  (loop ((for repository (in-list (reverse repositories))))
    (let ((pathname (repository-available-pathname
                     repository
                     (database-cache-directory db repository))))
      (when (file-exists? pathname)
        (load-available-file! db repository pathname)))))

(define (load-available-file! db repository pathname)
  (define (lose msg . irritants)
    (apply database-corruption 'load-available-file! pathname msg irritants))
  (call-with-input-file (->namestring pathname)
    (lambda (port)
      (loop ((for form (in-port port read)))
        (cond ((parse-package-form form (lambda (properties) '()))
               => (lambda (package)
                    (match (package-property package 'location #f)
                      (((location ___))
                       (add-package-source! db package
                                            (make-source repository location)))
                      (_
                       (lose "missing or invalid location in available file" form)))))
              (else
               (lose "invalid package form in available file" form)))))))

(define (load-installed-packages! db)
  (let ((directory (database-status-directory db)))
    (define (lose filename msg . irritants)
      (apply database-corruption
             'load-installed-packages
             (pathname-with-file directory filename)
             msg
             irritants))
    (let ((pkg-table (database-pkg-table db))
          (file-table (database-file-table db)))
      (loop ((for filename (in-directory directory)))
        => (values pkg-table file-table)
        (when (string-suffix? ".info" filename)
          (let ((item (load-item-info
                       (pathname-with-file directory filename))))
            (hashtable-update!
             pkg-table
             (package-name (item-package item))
             (lambda (items)
               (if items
                   (lose filename "duplicate package in status file" items)
                   (list item)))
             #f)
            (update-file-table! file-table
                                (database-destination db)
                                (item-package item))))))))

(define (update-file-table! table destination package)
  (define (fill-table directory category inventory)
    (loop ((for cursor (in-inventory inventory)))
      (let ((pathname (pathname-with-file directory (inventory-name cursor))))
        (define (calculate-new-value old-value real-pathname)
          (cond ((inventory-leaf? cursor)
                 (if old-value
                     (file-conflict old-value
                                    package
                                    real-pathname)
                     package))
                ((package? old-value)
                 (file-conflict old-value
                                package
                                real-pathname))
                (else
                 (fill-table (pathname-as-directory pathname)
                             category
                             cursor)
                 (lset-adjoin package=? (or old-value '()) package))))
        (let ((real-pathname
               (destination-pathname destination package category pathname)))
          
          (loop ((with ancestor-directory
                       (pathname-container real-pathname)
                       (pathname-container ancestor-directory))
                 (until (pathname=? ancestor-directory
                                    (destination-prefix destination))))
            (hashtable-update! table
                               ancestor-directory
                               (lambda (old-value)
                                 (lset-adjoin package=? old-value package))
                               '()))
          (hashtable-set! table
                          real-pathname
                          (calculate-new-value
                           (hashtable-ref table real-pathname #f)
                           real-pathname))))))
  (loop ((for inventory (in-list (package-inventories package))))
    (let ((category (inventory-name inventory)))
      (fill-table (make-pathname #f '() #f) category inventory))))

(define (load-item-info pathname)
  (call-with-input-file (->namestring pathname)
    (lambda (port)
      (let* ((item-properties (get-datum port))
             (package-form (get-datum port))
             (package (parse-package-form package-form (lambda (properties) '())))
             (name (package-name package)))
        (loop ((for form (in-port port read))
               (for inventories (listing (tree->inventory form name))))
          => (make-item (package-with-inventories package inventories)
                        (assq-ref item-properties 'state)
                        '()
                        #f))))))


;;; Source manipulation

;;@ Add a bundle as a source of packages to the database @var{db}.
;;
;; If @var{pathname-or-bundle} is a pathname, the bundle it refers is
;; scanned for its contents, and the available packages added and the
;; bundle is closed afterwards; it will be re-opened on demand.  This
;; requires that the pathname of the bundle must stay available while
;; @var{db} is kept open.
;;
;; If @var{pathname-or-bundle} is bundle, a reference to it is added
;; to @var{db} for package inside the bundle.  Note that it is
;; currently not possible to add a reference for packages that already
;; have an opened bundle associated with them.  For such packages, the
;; bundle provided in @var{pathname-or-bundle} is ignored.
(define (database-add-bundle! db pathname-or-bundle)
  (guarantee-open-database 'database-add-bundle! db)
  (cond ((bundle? pathname-or-bundle)
         (let ((bundle pathname-or-bundle))
           (define (set-item-bundle item)
             (if (item-bundle item)
                 item
                 (item-with-bundle item bundle)))
           (iterate! (for package (in-list (bundle-packages bundle)))
             (update-package-item! db
                                   package
                                   set-item-bundle
                                   (make-item package 'available '() #f)))))
        (else
         (let ((pathname pathname-or-bundle))
           (call-with-input-bundle pathname
               (bundle-options no-inventory)
             (lambda (bundle)
               (iterate! (for package (in-list (bundle-packages bundle)))
                 (add-package-source! db
                                      package
                                      (make-source null-repository pathname)))))))))

;;@ Add, as with @xref{dorodango database
;; database-add-bundle!,database-add-bundle!}, each element of the
;; list @var{pathnames} to the database.
(define (database-add-bundles! db pathnames)
  (loop ((for pathname (in-list pathnames)))
    (database-add-bundle! db pathname)))

;; Later-added sources override earlier ones
(define (add-package-source! db package source)
  (update-package-item! db
                        package
                        (lambda (item)
                          (item-modify-sources
                           item
                           (lambda (sources)
                             (append (list source) sources))))
                        (make-item package 'available '() #f)))

;; Insert or update an an item for @var{package}. If a corresponding
;; item already is found in @var{db}, @var{proc} will be applied to it
;; and the value returned by @var{proc} will replace the original
;; item.  If not matching item is found, @var{proc} will be applied to
;; @var{default}, and the value returned will be inserted as new item.
(define (update-package-item! db package proc default)
  (define (update-items items)
    (loop continue ((for item (in-list items))
                    (with result '() (cons item result))
                    (with inserted? #f))
      => (reverse
          (if inserted?
              result
              (cons (proc default) result)))
      (if3 (package-version-compare (package-version package)
                                    (package-version (item-package item)))
           (continue)
           (continue (=> inserted? #t)
                     (=> result (cons (proc item) result)))
           (if inserted?
               (continue)
               (continue
                (=> inserted? #t)
                (=> result (cons* item (proc default) result)))))))
  (hashtable-update! (database-pkg-table db)
                     (package-name package)
                     update-items
                     '()))

;;@ Update the database with the package availability information of
;; its repositories.
(define (database-update! db)
  (guarantee-open-database 'database-update! db)
  (loop ((for repository (in-list (reverse (database-repositories db)))))
    (cond ((repository-fetch-available
            repository
            (database-cache-directory db repository))
           => (lambda (available-pathname)
                (log/db 'info (cat "loading available information for repository `"
                                   (repository-name repository) "'"))
                (remove-repository! db repository)
                (load-available-file! db repository available-pathname))))))

(define (remove-repository! db repository)
  (define (item-without-repository item)
    (let ((remaining (filter (lambda (source)
                               (not (eq? (source-repository source)
                                         repository)))
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
                           (filter-map item-without-repository items))
                         '()))))

(define (database-get-hook-runner db)
  (cond ((database-hook-runner db)
         => values)
        (else
         (let ((hook-runner (spawn-hook-runner (database-destination db))))
           (database-hook-runner-set! db hook-runner)
           hook-runner))))


;;; Querying

(define (database-package-names db)
  (vector->list (hashtable-keys (database-pkg-table db))))

;;@ Lookup the package identified by @var{name} and @var{version} in
;; the database @var{db} and return its item, or @code{#f} if the
;; specified item can not be found. Besides specifying a specific
;; package version via @var{version}, this argument can also be one of
;; the following special values to specify a lookup strategy for the
;; package version:
;;
;; @table @code @item #f Return the installed version, or the latest
;;version if the package is not installed.
;;
;; @item 'installed
;; Return the installed version, or @code{#f} if the package is
;; not installed.
;;
;; @item 'newest
;; Return the newest version.
;; @end table
(define (database-lookup db name version)
  (define who 'database-lookup)
  (guarantee-open-database who db)
  (let ((items (database-items db name)))
    (cond ((eqv? version #f)
           (or (find item-installed? items)
               (and (pair? items) (car items))))
          ((symbol? version)
           (case version
             ((installed) (find item-installed? items))
             ((newest)    (and (pair? items) (car items)))
             (else        (assertion-violation who "invalid version" version))))
          (else
           (find-item-by-version items version)))))

;;@ Return the list of items for the package named @var{name}.
(define (database-items db name)
  (guarantee-open-database 'database-items db)
  (hashtable-ref (database-pkg-table db) name '()))

(define (find-item-by-version items version)
  (find (lambda (item)
          (package-version=? version (package-version
                                      (item-package item))))
        items))

;;@ Foof-loop iterator.
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

(define database-update-item!
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
                               'database-update-item!
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
     (database-update-item! db package proc #f))))


;;; Installation & removal

(define (database-package-info-pathname db package)
  (pathname-with-file (database-status-directory db)
                      (make-file (package-name package) "info")))

;:@ Unpack a package.
(define (database-unpack! db package)
  (define who 'database-unpack!)
  (define (do-install! desired-item)
    (let* ((bundle (open-bundle! who db desired-item))
           (bundle-package (bundle-package-ref bundle package)))
      (unless bundle-package
        (raise (condition
                (make-database-bundle-discrepancy bundle)
                (make-message-condition "missing package")
                (make-irritants-condition (list package)))))
      (log/db 'info
              (cat "unpacking " (dsp-package-identifier bundle-package) " ..."))
      (update-file-table! (database-file-table db)
                          (database-destination db)
                          bundle-package)
      (extract-package bundle bundle-package (database-destination db))
      (let ((unpacked-item (item-with-package
                            (item-with-state desired-item 'unpacked)
                            bundle-package)))
        (save-item-info db unpacked-item)
        (database-update-item! db bundle-package (lambda (item) unpacked-item)))
      #t))
  (guarantee-open-database who db)
  (and-let* ((items (database-items db (package-name package)))
             (desired-item (find-item-by-version items (package-version package))))
    (cond ((exists (lambda (item)
                     (and (item-installed? item)
                          (not (eq? item desired-item))))
                   items)
           => (lambda (item)
                (database-remove! db (package-name package))
                (do-install! desired-item)))
          ((not (item-installed? desired-item))
           (do-install! desired-item))
          (else
           #f))))

(define (open-bundle! who db item)
  (define (lose msg package)
    (raise (condition
            (make-who-condition who)
            (make-database-unavailable-package-error package)
            (make-message-condition msg))))
  (or (item-bundle item)
      (really-open-bundle! db item)
      (lose "could not obtain bundle for package" (item-package item))))

(define (really-open-bundle! db item)
  (define (update-bundle-packages bundle source)
    (loop ((for package (in-list (bundle-packages bundle))))
      (database-update-item! db
                             package
                             (lambda (item)
                               (make-item package
                                          (item-state item)
                                          (item-sources item)
                                          bundle))
                             (make-item package 'available (list source) #f))))
  (let ((sources (item-sources item)))
    (when (null? sources)
      (assertion-violation 'really-open-bundle!
                           "no sources for package" (item-package item)))
    (loop continue ((for source (in-list sources)))
      => #f
      (let ((repo (source-repository source)))
        (cond ((repository-fetch-bundle
                repo
                (source-location source)
                (database-cache-directory db repo))
               => (lambda (bundle-pathname)
                    (let ((bundle (open-input-bundle bundle-pathname)))
                      (update-bundle-packages bundle source)
                      bundle)))
              (else
               (continue)))))))

(define (extract-package bundle package destination)
  (loop ((for category (in-list (package-categories package))))
    (loop ((for pathname extractor (in-bundle-inventory
                                    bundle
                                    (package-category-inventory package
                                                                category))))
      (destination-install destination
                           package
                           category
                           pathname
                           extractor))))

(define (save-item-info db item)
  (let ((package (item-package item)))
    (call-with-output-file/atomic (database-package-info-pathname db package)
      (lambda (port)
        (put-datum port `((state . ,(item-state item))))
        (put-string port "\n\n")
        (put-datum port (package->form package))
        (put-string port "\n\n")
        (loop ((for inventory (in-list (package-inventories package))))
          (put-datum port (inventory->tree inventory))
          (put-string port "\n"))))))

;;@ Remove a package from the database.
(define (database-remove! db package-name)
  (guarantee-open-database 'database-remove! db)
  (and-let* ((item (find item-installed?
                         (hashtable-ref (database-pkg-table db) package-name '()))))
    (let ((package (item-package item)))
      (log/db 'info (cat "removing " (dsp-package-identifier package) " ..."))
      (remove-package-files! db package)
      (delete-file (database-package-info-pathname db package))
      (database-update-item! db
                             package
                             (lambda (item)
                               (if (null? (item-sources item))
                                   #f
                                   (item-with-state item 'available))))
      #t)))

(define (remove-package-files! db package)
  (let ((file-table (database-file-table db))
        (destination (database-destination db))
        (maybe-remove (make-hashtable pathname-hash pathname=?)))
    (define (delete-inventory category inventory path)
      (iterate! (for cursor (in-inventory inventory))
        (let ((pathname
               (destination-pathname destination
                                     package
                                     category
                                     (make-pathname #f
                                                    path
                                                    (inventory-name cursor)))))
          (cond ((inventory-container? cursor)
                 (delete-inventory category
                                   cursor
                                   (append path
                                           (list (inventory-name cursor))))
                 (maybe-remove-directory pathname (null? path)))
                (else
                 (log/db 'debug (cat "deleting " (dsp-pathname pathname)))
                 (delete-file pathname)
                 (hashtable-delete! file-table pathname)
                 (when (null? path)
                   (hashtable-set! maybe-remove
                                   (pathname-container pathname)
                                   #t)))))))
    (define (maybe-remove-directory pathname include-ancestors?)
      (when (not (pathname=? pathname (destination-prefix destination)))
        (let ((packages
               (remp (lambda (providing-package)
                       (package=? providing-package
                                  package))
                     (hashtable-ref file-table pathname '()))))
          (cond ((null? packages)
                 (guard (c ((i/o-error? c)
                            ;; This condition should only occur when
                            ;; the user has placed something into the
                            ;; directory
                            #f))
                   (delete-file pathname)
                   (log/db 'debug (cat "removed directory "
                                       (dsp-pathname pathname))))
                 (hashtable-delete! file-table pathname)
                 (when include-ancestors?
                   (maybe-remove-directory (pathname-container pathname) #t)))
                (else
                 (hashtable-set! file-table pathname packages))))))
    (iterate! (for inventory (in-list (package-inventories package)))
      (delete-inventory (inventory-name inventory) inventory '()))
    (iterate! (for directory (in-vector (hashtable-keys maybe-remove)))
      (maybe-remove-directory directory #t))))

;;@ Run installation hook for a package.
(define (database-setup! db package-name)
  (define who 'database-setup!)
  (guarantee-open-database who db)
  (let ((item (find item-installed?
                    (hashtable-ref (database-pkg-table db) package-name '()))))
    (cond ((not item)
           (assertion-violation who "package not installed" package-name))
          ((eq? 'unpacked (item-state item))
           (let ((installed-item (setup-item db item)))
             (save-item-info db installed-item)
             (database-update-item! db
                                    (item-package item)
                                    (lambda (item) installed-item))))
          (else
           (assertion-violation who "package already setup" package-name)))))

(define (setup-item db item)
  (define (conflict a-cursor b-cursor)
    ;;++ could use better diagnostics
    (file-conflict item #f #f))
  (let ((package (item-package item)))
    (log/db 'info (cat "Setting up "
                       (dsp-package-identifier package) " ..."))
    (make-item (run-destination-hooks (database-destination db)
                                      (run-installation-hook db item conflict)
                                      conflict)
               'installed
               (item-sources item)
               (item-bundle item))))

(define (make-source-unpacker db item)
  (lambda ()
    (let* ((bundle (open-bundle! 'make-source-unpacker db item))
           (tmp-dir (create-temp-directory)))
      (extract-bundle-package bundle (item-package item) tmp-dir)
      tmp-dir)))

(define (extract-bundle-package bundle package directory)
  (define (dest-pathname relative-path file)
    (pathname-join directory (make-pathname #f relative-path file)))
  (let ((package-path (find-package-path bundle package)))
    (loop ((for pathname extractor
                (in-bundle-inventory bundle (bundle-inventory bundle))))
      (cond ((list-prefix package-path (pathname-directory pathname) string=?)
             => (lambda (relative-path)
                  (let ((dest (dest-pathname relative-path
                                             (pathname-file pathname))))
                    (log/db 'debug (cat "extracting to " (dsp-pathname dest)))
                    (create-directory* (pathname-with-file dest #f))
                    (call-with-port (open-file-output-port (->namestring dest))
                      (lambda (port)
                        (extractor port))))))))))

;; Run the installation hook of @var{item} and return the
;; updated package.
(define (run-installation-hook db item conflict)
  (let ((package (item-package item)))
    (cond ((package-property package 'installation-hook #f)
           => (lambda (installation-hook)
                (package-extend-inventories
                 package
                 (run-hook (database-get-hook-runner db)
                           package
                           `(installation-hook . ,installation-hook)
                           (make-source-unpacker db item))
                 conflict)))
          (else
           package))))

;; Run the destination hooks for @var{destination} and return the
;; updated package.
(define (run-destination-hooks destination package conflict)
  (iterate-values ((package package))
      (for hook (in-list (destination-hooks destination)))
    (package-extend-inventories package (hook destination package) conflict)))

(define (find-package-path bundle package)
  (exists (lambda (entry)
            (match entry
              ((path . packages)
               (and (find (lambda (bundle-pkg)
                            (package=? bundle-pkg package))
                          packages)
                    path))))
          (bundle-package-map bundle)))

(define (database-clear-cache! db)
  (rm-rf (database-cache-dir db))
  (create-directory* (database-cache-dir db)))


;;; Logging

(define logger:dorodango.db (make-logger logger:dorodango 'db))
(define log/db (make-fmt-log logger:dorodango.db))

(define (dsp-package-identifier package)
  (cat (package-name package)
       " (" (package-version->string (package-version package)) ")"))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
