;;; database.sls --- Dorodango package database

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

(library (dorodango database)
  (export database?
          open-database
          close-database
          call-with-database
          
          database-add-bundle!
          database-add-bundles!

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

          logger:dorodango.db)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) append-reverse filter-map lset-adjoin)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13) string-map string-suffix?)
          (srfi :67 compare-procedures)
          (wak foof-loop)
          (wak foof-loop nested)
          (spells alist)
          (spells pathname)
          (spells filesys)
          (wak fmt)
          (spells match)
          (only (spells record-types)
                define-functional-fields)
          (spells logging)
          (spells tracing)
          (dorodango private utils)
          (dorodango inventory)
          (dorodango bundle)
          (dorodango package)
          (dorodango repository)
          (dorodango destination)
          (dorodango hooks))

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


;;; Database loading

;;@ Open a package database.
(define open-database
  (case-lambda
    ((directory destination repositories implementation cache-dir)
     (log/db 'debug "opening database in " (dsp-pathname directory) " ...")
     (let* ((directory (pathname-as-directory directory))
            (status-directory (status-subdirectory directory)))
       (unless (file-exists? status-directory)
         (setup-destination destination `((implementation . ,implementation)))
         (create-directory* status-directory))
       (lock-database-directory directory)
       (receive (pkg-table file-table)
                (load-installed-packages status-directory destination)
         (let ((db (make-database directory
                                  destination
                                  repositories
                                  cache-dir
                                  file-table
                                  pkg-table
                                  #f ;closed?
                                  #f ;hook-runner
                                  )))
           (iterate! (for repository (in-list repositories))
             (create-directory* (database-cache-directory db repository)))
           (load-available-files! db repositories)
           db))))
    ((directory destination repositories implementation)
     (let ((directory (pathname-as-directory directory)))
       (open-database directory
                      destination
                      repositories
                      implementation
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
  (unlock-database-directory (database-directory db))
  (database-closed?-set! db #t)
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

(define (load-installed-packages directory destination)
  (define (lose filename msg . irritants)
    (apply database-corruption
           'load-installed-packages
           (pathname-with-file directory filename)
           msg
           irritants))
  (let ((pkg-table (make-eq-hashtable))
        (file-table (make-hashtable pathname-hash pathname=?)))
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
                              destination
                              (item-package item)))))))

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
        (for-each
         (lambda (real-pathname)
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
                            real-pathname)))
         (destination-pathnames destination package category pathname)))))
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

;;@ Add the bundle located at @var{pathname} as a source of packages
;; to the database @var{db}.
(define (database-add-bundle! db pathname)
  (guarantee-open-database 'database-add-bundle! db)
  (let ((bundle (open-input-bundle pathname (bundle-options no-inventory))))
    (loop ((for package (in-list (bundle-packages bundle))))
      (add-package-source! db
                           package
                           (make-source null-repository pathname)))
    (close-bundle bundle)))

;;@ Add, as with @xref{dorodango database
;; database-add-bundle!,database-add-bundle!}, each element of the
;; list @var{pathnames} to the database.
(define (database-add-bundles! db pathnames)
  (loop ((for pathname (in-list pathnames)))
    (database-add-bundle! db pathname)))

;; Later-added sources override earlier ones
(define (add-package-source! db package source)
  (define (update-items items)
    (loop continue ((for item (in-list items))
                    (with result '() (cons item result))
                    (with inserted? #f))
      => (reverse
          (if inserted?
              result
              (cons (make-item package 'available (list source) #f) result)))
      (if3 (package-version-compare (package-version package)
                                    (package-version (item-package item)))
           (continue)
           (continue (=> inserted? #t)
                     (=> result
                         (cons (item-modify-sources
                                item
                                (lambda (sources)
                                  (append (list source) sources)))
                               result)))
           (if inserted?
               (continue)
               (continue
                (=> inserted? #t)
                (=> result
                    (cons* item
                           (make-item package 'available (list source) #f)
                           result)))))))
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
    (let* ((bundle (checked-open-bundle! who db desired-item))
           (package (bundle-package-ref bundle package)))
      (log/db 'info (cat "unpacking " (dsp-package-identifier package) " ..."))
      (update-file-table! (database-file-table db)
                          (database-destination db)
                          package)
      (extract-package bundle package (database-destination db))
      (let ((unpacked-item (item-with-package
                            (item-with-state desired-item 'unpacked)
                            package)))
        (save-item-info (database-package-info-pathname db package)
                        unpacked-item)
        (database-update-item! db package (lambda (item) unpacked-item)))
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

(define (checked-open-bundle! who db item)
  (define (lose msg package)
    (raise (condition
            (make-who-condition who)
            (make-database-unavailable-package-error package)
            (make-message-condition msg))))
  (or (open-bundle! db item)
      (lose "could not obtain bundle for package" (item-package item))))

(define (open-bundle! db item)
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
      (assertion-violation 'open-bundle!
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

(define (save-item-info pathname item)
  (let ((package (item-package item)))
    (call-with-output-file/atomic (->namestring pathname)
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
      (loop continue ((for cursor (in-inventory inventory)))
        (loop ((for pathname
                    (in-list (destination-pathnames
                              destination
                              package
                              category
                              (make-pathname #f path (inventory-name cursor))))))
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
                                   #t))))
          (continue))))
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
    (loop ((for category (in-list (package-categories package))))
      (let ((package-inventory (package-category-inventory package category)))
        (delete-inventory category package-inventory '())))
    (loop ((for directory (in-vector (hashtable-keys maybe-remove))))
      (maybe-remove-directory directory #t))))

;;@ Run installation hook for a package.
(define (database-setup! db package-name)
  (define who 'database-setup!)
  (define (setup-item item)
    (define (conflict a-cursor b-cursor)
      ;;++ could use better diagnostics
      (file-conflict item #f #f))
    (let* ((package (item-package item))
           (installation-hook (package-property package
                                                'installation-hook
                                                #f)))
      (log/db 'info (cat "Setting up "
                         (dsp-package-identifier package) " ..."))
      (let* ((installed-files
              (if installation-hook
                  (run-hook (database-get-hook-runner db)
                            package
                            `(installation-hook . ,installation-hook)
                            (make-source-unpacker db item))
                  '()))
             (installed-item
              (item-with-state (item-add-files item installed-files conflict)
                               'installed)))
        (save-item-info
         (database-package-info-pathname db package)
         installed-item)
        (database-update-item! db package (lambda (item) installed-item)))))
  (guarantee-open-database who db)
  (let ((item (find item-installed?
                    (hashtable-ref (database-pkg-table db) package-name '()))))
    (cond ((not item)
           (assertion-violation who "package not installed" package-name))
          ((eq? 'unpacked (item-state item))
           (setup-item item))
          (else
           (assertion-violation who "package already setup" package-name)))))

(define (make-source-unpacker db item)
  (lambda ()
    (let* ((bundle (checked-open-bundle! 'make-source-unpacker db item))
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

(define (find-package-path bundle package)
  (exists (lambda (entry)
            (match entry
              ((path . packages)
               (and (find (lambda (bundle-pkg)
                            (package=? bundle-pkg package))
                          packages)
                    path))))
          (bundle-package-map bundle)))

(define (item-add-files item inventories conflict)
  (item-modify-package
   item
   (lambda (package)
     (package-modify-inventories
      package
      (lambda (existing)
        (merge-category-inventories existing inventories conflict))))))

(define (merge-category-inventories a-inventories b-inventories conflict)
  (define (same-name-as inventory)
    (let ((name (inventory-name inventory)))
      (lambda (other)
        (eq? name (inventory-name other)))))
  (loop continue ((for a-entry (in-list a-inventories))
                  (with result '()))
    => (append-reverse result
                       (collect-list (for b-entry (in-list b-inventories))
                           (if (not (find (same-name-as b-entry) a-inventories)))
                         b-entry))
    (cond ((find (same-name-as a-entry) b-inventories)
           => (lambda (b-entry)
                (continue
                 (=> result (cons (merge-inventories a-entry b-entry conflict)
                                  result)))))
          (else
           (continue (=> result (cons a-entry result)))))))

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
