;;; bundles.scm --- Dealing with bundle files

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (guildhall bundle)
  (export bundle?
          open-input-bundle
          close-bundle
          call-with-input-bundle

          bundle-options
          
          bundle-packages
          bundle-package-ref
          bundle-package-map

          bundle-inventory
          in-bundle-inventory)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) concatenate filter-map last)
          (srfi :8 receive)
          (only (srfi :13) string-join string-suffix?)
          (guildhall ext foof-loop)
          (guildhall ext fmt)
          (only (guile) assq-ref or-map)
          (only (spells record-types)
                define-functional-fields)
          (ice-9 match)
          (spells ports)
          (spells pathname)
          (spells filesys)
          (spells operations)
          (spells logging)
          (spells tracing)
          (guildhall private utils)
          (guildhall private zip)
          (guildhall package)
          (guildhall inventory)
          (guildhall inventory mapping))

(define-record-type bundle
  (fields inventory package-map ops))

(define-operation (bundle/identifier object))
(define-operation (bundle/extract-entry object entry dest-port))
(define-operation (bundle/close object)
  (values))

(define (close-bundle bundle)
  (bundle/close (bundle-ops bundle)))

;;@ foof-loop iterator for bundles.
;;
;; @lisp
;; (loop ((for pathname extractor
;;             (in-bundle-inventory bundle (bundle-inventory bundle)))
;; @end lisp
(define-syntax in-bundle-inventory
  (syntax-rules ()
    ((_ (pathname-var extractor-var) (bundle-expr inventory-expr) cont . env)
     (cont
      (((bundle) bundle-expr))                   ;Outer bindings
      ((cursor                                   ;Loop variables
        (inventory-cursor inventory-expr)
        (inventory-cursor-next cursor)))
      ()                                         ;Entry bindings
      ((not cursor))                             ;Termination conditions
      (((pathname-var)                           ;Body bindings
        (inventory-cursor->pathname cursor))
       ((extractor-var)
        (lambda (port)
          (bundle/extract-entry (bundle-ops bundle)
                                (inventory-cursor-data cursor)
                                  port))))
      ()                                         ;Final bindings
      . env))))

(define (inventory-cursor->pathname cursor)
  (make-pathname #f
                 (reverse (inventory-cursor-path cursor))
                 (inventory-cursor-name cursor)))

(define-enumeration bundle-option
  (no-inventory)
  bundle-options)

(define (bundle-package-ref bundle package)
  (find (lambda (bundle-package)
          (package=? bundle-package package))
        (bundle-packages bundle)))



;;++ register bundle with GC
(define open-input-bundle
  (case-lambda
    ((pathname options)
     (cond ((file-directory? pathname)
            (open-directory-input-bundle pathname options))
           (else
            (open-zip-input-bundle pathname options))))
    ((pathname)
     (open-input-bundle pathname (bundle-options)))))

(define call-with-input-bundle
  (case-lambda
    ((pathname options proc)
     (let ((bundle (open-input-bundle pathname options)))
       (receive results (proc bundle)
         (close-bundle bundle)
         (apply values results))))
    ((pathname proc)
     (call-with-input-bundle pathname (bundle-options) proc))))


;;; Filesystem bundles

(define (make-fs-bundle-ops directory)
  (object #f
    ((bundle/identifier ops)
     (->namestring directory))
    ((bundle/extract-entry ops entry dest-port)
     (call-with-port (open-file-input-port
                      (->namestring
                       (pathname-join directory entry)))
       (lambda (source-port)
         (copy-port source-port dest-port))))))

(define (open-directory-input-bundle pathname options)
  (let* ((directory (pathname-as-directory pathname))
         (inventory? (not (enum-set-member? 'no-inventory options)))
         (inventory (directory->inventory directory
                                          (make-pathname-filter inventory?)))
         (ops (make-fs-bundle-ops directory)))
    (make-bundle (and inventory? inventory)
                 (construct-package-map ops inventory inventory?)
                 ops)))

(define (directory->inventory pathname accept?)
  (define (fill-inventory inventory pathname relative-dir)
    (let ((dir (pathname-as-directory pathname)))
      (loop continue ((for filename (in-directory dir))
                      (with cursor (inventory-open inventory)))
        => (inventory-leave cursor)
        (let ((pathname (pathname-with-file dir filename))
              (relative-pathname (pathname-with-file relative-dir filename)))
          (let ((is-directory? (file-directory? pathname)))
            (if (accept? relative-pathname is-directory?)
                (let* ((relative-pathname
                        (if is-directory?
                            (pathname-as-directory relative-pathname)
                            relative-pathname))
                       (new-cursor (inventory-insert cursor
                                                     filename
                                                     is-directory?
                                                     relative-pathname)))
                  (continue
                   (=> cursor
                       (if is-directory?
                           (inventory-previous
                            (fill-inventory (inventory-next new-cursor)
                                            pathname
                                            relative-pathname))
                           new-cursor))))
                (continue)))))))
  (let ((base-directory (pathname-as-directory pathname)))
    (fill-inventory (make-inventory 'root base-directory)
                    base-directory
                    (make-pathname #f '() #f))))

(define (default-pathname-filter pathname is-directory?)
  (not (and is-directory?
            (member (file-namestring pathname)
                    '(".git" ".bzr" ".svn" ".hg" "_darcs")))))

(define (make-pathname-filter inventory?)
  (if inventory?
      default-pathname-filter
      (let ((pkgs-filename (last pkgs-path))
            (pkgs-depth (- (length pkgs-path) 1)))
        (lambda (pathname is-directory?)
          (and (default-pathname-filter pathname is-directory?)
               (if is-directory?
                   (< (length (pathname-directory pathname))
                      (+ pkgs-depth 1))
                   (string=? (file-namestring pathname)
                             pkgs-filename)))))))

;;; ZIP bundles

(define (make-zip-bundle-ops pathname zip-port)
  (object #f
    ((bundle/identifier ops)
     (->namestring pathname))
    ((bundle/extract-entry ops entry dest-port)
     (extract-zip-entry zip-port entry dest-port))))

(define (open-zip-input-bundle pathname options)
  (let* ((port (open-file-input-port (->namestring pathname)))
         (inventory? (not (enum-set-member? 'no-inventory options)))
         (inventory (zip-port->inventory port))
         (ops (make-zip-bundle-ops pathname port)))
    (make-bundle (and inventory? inventory)
                 (construct-package-map ops inventory inventory?)
                 ops)))


;;; Package handling

(define pkgs-path '("pkg-list.scm"))

(define (inventory-pkg-lists inventory)
  (cond ((inventory-ref-data inventory pkgs-path #f)
         => (lambda (entry)
              (list (cons '() entry))))
        (else
         (loop ((for cursor (in-inventory inventory))
                (for result (listing
                             (inventory-ref-data cursor pkgs-path #f)
                             => (lambda (info)
                                  (cons (list (inventory-name cursor))
                                        info)))))
           => result))))

(define (bundle-packages bundle)
  (concatenate (map cdr (bundle-package-map bundle))))

(define (construct-package-map ops inventory inventory?)
  (define (snarf-packages elt)
    (match elt
      ((path . entry)
       (let ((pkg-inventory (and inventory?
                                 (inventory-ref inventory path))))
         (cons path (bundle-entry->packages ops entry path pkg-inventory))))))
  (loop ((for elt (in-list (inventory-pkg-lists inventory)))
         (for result (listing (snarf-packages elt))))
    => result))

(define (bundle-entry->packages ops entry path inventory)
  (define who 'bundle-entry->packages)
  (let ((entry-port
         (open-string-input-port
          (utf8->string
           (call-with-bytevector-output-port
             (lambda (port)
               (bundle/extract-entry ops entry port)))))))
    (loop continue ((for form (in-port entry-port read))
                    (with result '())
                    (with inventory inventory))
      => (reverse result)
      (cond ((parse-package-form form)             
             => (lambda (package)
                  (receive (inventories uncategorized)
                           (if inventory
                               (categorize-inventory
                                (package-properties package)
                                inventory)
                               (values '() (make-inventory 'uncategorized #f)))
                    (continue
                     (=> result (cons (package-with-inventories
                                       package
                                       inventories)
                                      result))
                     (=> inventory uncategorized)))))
            (else
             (log/bundle 'warning
                         "unrecognized form in package description file"
                         form
                         (string-append (bundle/identifier ops)
                                        ":"
                                        (path->string
                                         (append path pkgs-path))))
             (continue))))))

;;; Mappers

(define make-mapper make-inventory-mapper)

;; Predefined mappers

(define (make-library-mapper pkgs-path)
  (make-mapper (lambda (filename)
                 (and (or (null? pkgs-path)
                          (not (null? (cdr pkgs-path)))
                          (not (string=? filename (car pkgs-path))))
                      (or-map (lambda (suffix)
                                (string-suffix? suffix filename))
                              '(".sls" ".scm"))
                      (list filename)))
               (lambda (filename)
                 (values (list filename)
                         (make-library-mapper
                          (if (null? pkgs-path)
                              '()
                              (cdr pkgs-path)))))))

(define default-library-mapper
  (make-library-mapper pkgs-path))


(define default-documentation-mapper
  (make-mapper (lambda (filename)
                 (and (member filename '("README"
                                         "COPYING"
                                         "AUTHORS"
                                         "NEWS"
                                         "ChangeLog"))
                   (list filename)))
               (lambda (filename)
                 (values (list filename) default-documentation-mapper))))

(define lookup-mapper
  (let ((mapper-alist `((* . ,(make-recursive-inventory-mapper list))
                        (sls . ,default-library-mapper)
                        (scm . ,default-library-mapper))))
    (lambda (name)
      (or (assq-ref mapper-alist name)
          (error 'lookup-mapper "unknown mapper" name)))))


;;; Categorization

(define-record-type category
  (fields name default-mapper))

(define categories
  (list
   (make-category 'libraries default-library-mapper)
   (make-category 'programs null-inventory-mapper) ; no sensible default here
   (make-category 'documentation default-documentation-mapper)))

(define (categorize-inventory properties inventory)
  (loop continue ((for category (in-list categories))
                  (with uncategorized inventory)
                  (with result '()))
    => (values (reverse result) uncategorized)
    (let ((mapper (cond ((assq-ref properties (category-name category))
                         => (lambda (rules)
                              (evaluate-inventory-mapping-rules
                               rules
                               lookup-mapper)))
                        (else
                         (category-default-mapper category)))))
      (receive (category-inventory remainder)
               (apply-categorization uncategorized category mapper)
        (continue (=> uncategorized remainder)
                  (=> result (cons category-inventory result)))))))

(define (apply-categorization inventory category mapper)
  (log/categorizer 'debug (cat "categorizing " (category-name category)))
  (apply-inventory-mapper mapper
                          (make-inventory (category-name category) 'category)
                          inventory))


;;; Utilities

(define (path->string path)
  (string-join path "/"))

(define logger:dorodango.bundle (make-logger logger:dorodango 'bundle))
(define log/bundle (make-fmt-log logger:dorodango.bundle))

(define logger:dorodango.categorizer (make-logger logger:dorodango 'categorizer))
(define log/categorizer (make-fmt-log logger:dorodango.categorizer))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (object 1) (match 1))
;; End:
