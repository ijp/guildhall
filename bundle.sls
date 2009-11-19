;;; bundles.sls --- Dealing with bundle files

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

(library (dorodango bundle)
  (export bundle?
          open-input-bundle
          close-bundle
          call-with-input-bundle

          bundle-options
          
          bundle-packages
          bundle-package-ref

          in-bundle-inventory)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) filter-map last)
          (srfi :8 receive)
          (only (srfi :13) string-join string-suffix? string-null?)
          (spells alist)
          (spells misc)
          (only (spells record-types)
                define-functional-fields)
          (spells string-utils)
          (spells match)
          (spells ports)
          (spells pathname)
          (spells filesys)
          (spells foof-loop)
          (spells operations)
          (spells fmt)
          (spells logging)
          (spells tracing)
          (only (spells assert) cout)
          (prefix (weinholt compression zip) zip:)
          (dorodango private utils)
          (dorodango package)
          (dorodango inventory))

(define-record-type bundle
  (fields inventory packages ops))

(define-operation (bundle/identifier object))
(define-operation (bundle/extract-entry object entry dest-port))
(define-operation (bundle/close object)
  (unspecific))

(define (close-bundle bundle)
  (bundle/close (bundle-ops bundle)))

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
         (apply values results))))))


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
                 (list-bundle-packages ops inventory inventory?)
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

(define (extract-zip-entry zip-port zip-central dest-port)
  (let ((zip-local (zip:central-directory->file-record zip-port zip-central)))
    (zip:extract-to-port zip-port zip-local zip-central dest-port)))

(define (open-zip-input-bundle pathname options)
  (let* ((port (open-file-input-port (->namestring pathname)))
         (zip-dir (zip:get-central-directory port))
         (inventory? (not (enum-set-member? 'no-inventory options)))
         (inventory (zip-dir->inventory zip-dir))
         (ops (make-zip-bundle-ops pathname port)))
    (make-bundle (and inventory? inventory)
                 (list-bundle-packages ops inventory inventory?)
                 ops)))

;; This should be made more robust; if there are inconsistencies in
;; the central directory file names (such as duplicates or "files
;; inside a file"), `inventory-update' will error out; this should be
;; caught and appropriately reported.
(define (zip-dir->inventory zip-dir)
  (loop continue ((for entry (in-list zip-dir))
                  (with inventory (make-inventory 'root #f)))
    => inventory
    (if (zip:central-directory? entry)
        (receive (path container?)
                 (filename->path (zip:central-directory-filename entry))
          (if (null? path)
              (continue)  ;bad zipfile, probably should error out here
              (continue
               (=> inventory
                   (inventory-leave-n
                    (inventory-update inventory path container? entry)
                    (length path))))))
        (continue))))

(define (filename->path filename)
  (let* ((path (string-split filename #\/))
         (relative-path (if (and (not (null? path))
                                 (string-null? (car path)))
                            (cdr path)
                            path))
         (reversed-path (reverse relative-path)))
    (if (and (not (null? reversed-path))
             (string-null? (car reversed-path)))
        (values (reverse (cdr reversed-path)) #t)
        (values relative-path #f))))


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

(define (list-bundle-packages ops inventory inventory?)
  (loop ((for elt (in-list (inventory-pkg-lists inventory)))
         (for result
              (appending-reverse
               (match elt
                 ((path . entry)
                  (let ((pkg-inventory (and inventory?
                                            (inventory-ref inventory path))))
                    (bundle-entry->packages ops entry path pkg-inventory)))))))
    => (reverse result)))

(define (bundle-entry->packages ops entry path pkg-inventory)
  (define who 'bundle-entry->packages)
  (let ((entry-port
         (open-string-input-port
          (utf8->string
           (call-with-bytevector-output-port
             (lambda (port)
               (bundle/extract-entry ops entry port)))))))
    (loop continue ((for form (in-port entry-port read))
                    (with result '()))
      => (reverse result)
      (cond ((parse-package-form
              form
              (lambda (properties)
                (if pkg-inventory
                    (categorize-inventory properties pkg-inventory)
                    '())))
             => (lambda (package)
                  (continue (=> result (cons package result)))))
            (else
             (warn who
                   "unrecognized form in package description file"
                   form
                   (string-append (bundle/identifier ops)
                                  ":"
                                  (path->string (append path pkgs-path))))
             (continue))))))

;;; Mappers

(define make-mapper make-inventory-mapper)

(define null-mapper
  (make-mapper (lambda (path)
                 #f)
               (lambda (path)
                 (values #f #f))))


;; Predefined mappers

(define (make-library-mapper pkgs-path)
  (make-mapper (lambda (filename)
                 (and (or (null? pkgs-path)
                          (not (null? (cdr pkgs-path)))
                          (not (string=? filename (car pkgs-path))))
                      (or-map (lambda (suffix)
                                (string-suffix? suffix filename))
                              '(".sls" ".ss" ".scm"))
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

(define star-mapper
  (make-mapper (lambda (filename)
                 (list filename))
               (lambda (filename)
                 (values (list filename) #f))))

(define (mapper-with-destination mapper destination)
  (let ((map-leaf (inventory-mapper-map-leaf mapper))
        (map-container (inventory-mapper-map-container mapper)))
    (make-mapper (lambda (filename)
                   (and=> (map-leaf filename)
                          (lambda (path)
                            (append destination path))))
                 (lambda (filename)
                   (receive (path submapper) (map-container filename)
                     (if path
                         (values (append destination path) submapper)
                         (values #f #f)))))))

;; Mapping rule evaluation

(define-record-type mapper-rule
  (fields path submapper destination))

(define-functional-fields mapper-rule path submapper destination)

(define (evaluate-mapping-rules rules)
  (mapper-rules->mapper (map evaluate-mapping-rule rules)))

(define (mapper-rules->mapper rules)
  (let ((catch-all (and=> (memp (lambda (rule)
                                (null? (mapper-rule-path rule)))
                                rules)
                          car)))
    (if catch-all
        (mapper-with-destination (or (mapper-rule-submapper catch-all)
                                     star-mapper)
                                 (mapper-rule-destination catch-all))
        (make-mapper
         (lambda (filename)
           (let ((rule
                  (find (lambda (rule)
                          (let ((path (mapper-rule-path rule)))
                            (and (not (null? path))
                                 (null? (cdr path))
                                 (string=? filename (car path)))))
                        rules)))
             (and rule (mapper-rule-destination rule))))
         (lambda (filename)
           (let ((mapper
                  (exists
                   (lambda (rule)
                     (let ((path (mapper-rule-path rule)))
                       (cond ((null? path)
                              (mapper-with-destination
                               (or (mapper-rule-submapper rule) star-mapper)
                               (mapper-rule-destination rule)))
                             ((string=? filename (car path))
                              (mapper-rules->mapper
                               (list
                                (mapper-rule-with-path rule (cdr path)))))
                             (else
                              #f))))
                   rules)))
             (if mapper
                 (values '() mapper)
                 (values #f #f))))))))

(define lookup-mapper
  (let ((mapper-alist `((* . ,star-mapper)
                        (libs . ,default-library-mapper))))
    (lambda (name)
      (or (assq-ref mapper-alist name)
          (error 'lookup-mapper "unknown mapper" name)))))

;; Returns a path and a mapper
(define (parse-mapping-expr expr)
  (cond ((string? expr)
         (values (list expr) #f))
        ((symbol? expr)
         (values '() (lookup-mapper expr)))
        ((null? expr)
         (values '() #f))
        ((pair? expr)
         (let next ((lst expr)
                    (path '()))
           (cond ((pair? lst)
                  (next (cdr lst) (cons (car lst) path)))
                 ((null? lst)
                  (values (reverse path) #f))
                 (else
                  (values (reverse path) (lookup-mapper lst))))))
        (else
         (error 'parse-mapping-expr "invalid expression" expr))))

(define (parse-path path)
  (match path
    (((? string? components) ___) components)
    ((? string? path)             (list path))
    (else                         (error 'parse-path "invalid path" path))))

(define (evaluate-mapping-rule rule)
  (define who 'evaluate-mapping-rule)
  (match rule
    ((source '-> dest)
     (receive (path submapper)
              (parse-mapping-expr source)
       (make-mapper-rule path submapper (parse-path dest))))
    (source
     (receive (path submapper)
              (parse-mapping-expr source)
       (make-mapper-rule path submapper path)))))


;;; Categorization

(define-record-type category
  (fields name default-mapper))

(define categories
  (list
   (make-category 'libraries default-library-mapper)
   (make-category 'programs null-mapper) ; no sensible default here
   (make-category 'documentation default-documentation-mapper)))

(define (categorize-inventory properties inventory)
  (loop continue ((for category (in-list categories))
                  (with uncategorized
                        (inventory-relabel inventory 'uncategorized 'category))
                  (with result '()))
    => (reverse (cons uncategorized result))
    (let ((mapper (cond ((assq-ref properties (category-name category))
                         => evaluate-mapping-rules)
                        (else
                         (category-default-mapper category)))))
      (receive (category-inventory remainder)
               (apply-categorization uncategorized category mapper)
        (continue (=> uncategorized remainder)
                  (=> result (cons category-inventory result)))))))

(define (apply-categorization inventory category mapper)
  (log/categorizer 'debug (cat "categorizing " (category-name category)))
  (apply-inventory-mapper (make-inventory (category-name category) 'category)
                          inventory
                          mapper))


;;; Utilities

(define (path->string path)
  (string-join path "/"))

(define logger:dorodango.categorizer (make-logger logger:dorodango 'categorizer))
(define log/categorizer (make-fmt-log logger:dorodango.categorizer))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (object 1) (match 1))
;; End:
