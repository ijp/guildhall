;;; destination.scm --- 

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

;; Currently, FHS destinations install a wrapper shell script for each
;; R6RS program. This wrapper executes a destination-specific
;; `r6rs-script', which in turn sets the implementation-specific
;; library search path based on the destination. Once SRFI 103 (and
;; hence R6RS_LIBRARY_PATH) is widely implemented, r6rs-script may be
;; changed to a symlink or dropped in favor of `scheme-script', as
;; recommended by R6RS Appendix D.2. One level of indirection seems
;; inevitable to facilitate setting the library search path
;; environment variable appropriatly for the destination.

;;; Code:
#!r6rs

(library (guildhall destination)
  (export destination?
          destination-name
          destination-prefix
          destination-hooks
          destination-pathname
          setup-destination
          destination-install
          destination-without-categories
          
          make-fhs-destination)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) split-at append-reverse)
          (only (guile) effective-version)
          (srfi :8 receive)
          (only (srfi :13)
                string-concatenate
                string-concatenate-reverse
                string-contains) 
          (srfi :45 lazy)
          (only (guile) assq-ref and=>)
          (guildhall ext fmt)
          (guildhall ext irregex)
          (guildhall ext foof-loop)
          (guildhall ext foof-loop nested)
          (guildhall spells record-types)
          (guildhall spells pathname)
          (guildhall spells filesys)
          (guildhall spells logging)
          (guildhall spells process) ;just needed for `chmod'
          (guildhall spells sysutils) ;ditto
          (guildhall private utils)
          (guildhall build-info)
          (guildhall inventory)
          (guildhall package))

(define-record-type* destination
  (make-destination name prefix setup categories hooks)
  ())

(define-functional-fields destination
  name prefix setup categories hooks)

(define (destination-without-categories destination categories)
  (destination-modify-categories
   destination
   (lambda (categories)
     (remp (lambda (entry)
             (memq (car entry) categories))
           categories))))

(define (invoke-destination-handler destination category getter . args)
  (cond ((assq-ref (destination-categories destination) category)
         => (lambda (handler)
              (apply (getter handler) args)))
        (else
         (error 'invoke-destination-handler "no handler for category"
                category))))

(define (destination-pathname destination package category pathname)
  (invoke-destination-handler destination category handler-mapper package pathname))

(define (setup-destination destination options)
  ((destination-setup destination) destination options))

(define (destination-open-file destination package category pathname)
  (invoke-destination-handler destination category handler-opener package pathname))

(define (destination-install destination package category pathname extractor)
  (call-with-port (destination-open-file destination package category pathname)
    extractor))

(define-record-type handler
  (fields mapper opener))


;;; FHS destination

(define fhs-libraries-template
  (list "share" "guile" "site" (effective-version)))
(define fhs-ccache-template
  (list "lib" "guile" (effective-version) "site-ccache"))
(define fhs-doc-template
  (list "share" "doc" "guildhall" (effective-version) 'name))
(define fhs-programs-template
  (list "share" "guildhall" (effective-version) 'name "programs"))
(define fhs-executables-template
  (list "lib" "guildhall" (effective-version) 'name "bin"))
(define fhs-auxiliaries-template
  (list "share" "guildhall" (effective-version) 'name "aux"))

(define (make-fhs-destination name prefix)
  (let ((prefix (pathname-as-directory prefix)))
    (make-destination
     name
     prefix
     fhs-destination-setup
     `((libraries
        . ,(make-simple-handler prefix fhs-libraries-template))
       (ccache
        . ,(make-simple-handler prefix fhs-ccache-template))
       (library-auxiliaries
        . ,(make-simple-handler prefix fhs-libraries-template))
       (documentation
        . ,(make-simple-handler prefix fhs-doc-template))
       (programs
        . ,(make-simple-handler prefix fhs-programs-template))
       (package-programs
        . ,(make-simple-handler prefix fhs-programs-template))
       (executables
        . ,(make-executable-handler prefix fhs-executables-template))
       (package-executables
        . ,(make-executable-handler prefix fhs-executables-template))
       (man
        . ,(make-man-page-handler prefix))
       (package-auxiliaries
        . ,(make-simple-handler prefix fhs-auxiliaries-template)))
     (list sh-wrapper-hook))))

(define (string-replace s replacements)
  (loop next-replacement ((for replacement (in-list replacements))
                          (with s s))
    => s
    (let ((key (car replacement))
          (value (cdr replacement)))
      (loop continue ((with i 0)
                      (with result '()))
        (cond ((string-contains s key i)
               => (lambda (pos)
                    (continue (=> result (append-reverse (list (substring s i pos)
                                                               value)
                                                         result))
                              (=> i (+ pos (string-length key))))))
              (else
               (next-replacement
                (=> s (string-concatenate-reverse
                       (cons (substring s i (string-length s))
                             result))))))))))

(define (get-lines/replace port substitutions)
  (loop ((for line (in-port port get-line))
         (for result (listing (string-replace line substitutions))))
    => result))

(define (fhs-destination-setup destination options)
  #;
  (let ((implementation (or (assq-ref options 'implementation)
                            (scheme-implementation)))
        (script-directory (dorodango-script-directory))
        (destination-script-directory
         (pathname-join (destination-prefix destination)
                        script-directory-pathname)))
    (define (implementation-pathname pathname)
      (pathname-add-type pathname (symbol->string implementation)))
    (log/fhs 'info
             (cat "initializing destination `" (destination-name destination)
                  "' in " (dsp-pathname (destination-prefix destination))
                  " for " implementation))
    (create-directory* destination-script-directory)
    ;; Put the data scripts into their place
    (iterate! (for entry (in-directory script-directory))
      (let ((destination-script
             (pathname-with-file destination-script-directory entry))
            (source-script (pathname-with-file script-directory entry)))
        (call-with-output-file/atomic destination-script
          (lambda (port)
            (fmt port (rewrite-script source-script destination))))
        (chmod "+x" destination-script)))
    ;; Create implementation-choosing symlinks
    (iterate! (for script (in-list '("run" "compile")))
      (let ((script-pathname (pathname-with-file
                              destination-script-directory
                              script)))
        (delete-file script-pathname) ;++ make the link creation atomic
        (when (file-exists? (implementation-pathname script-pathname))
          (create-symbolic-link
           (implementation-pathname (make-pathname #f '() script))
           script-pathname)))))
  ; Nothing to do!
  #t)

(define (make-simple-handler prefix template)
  (make-handler
   (lambda (package pathname)
     (build-pathname prefix template package pathname))
   (lambda (package pathname)
     (do-open-file (build-pathname prefix template package pathname)))))

(define (make-man-page-handler prefix)
  (define (man-page-pathname pathname)
    (let ((section (filename->man-section (file-namestring pathname))))
      (pathname-join
       prefix
       `(("share" "man" ,(string-append "man" (number->string section))))
       pathname)))
  (make-handler
   (lambda (package pathname)
     (man-page-pathname pathname))
   (lambda (package pathname)
     (do-open-file (man-page-pathname pathname)))))

(define (make-executable-handler prefix template)
  (define (executable-pathname package pathname)
    (build-pathname prefix template package pathname))
  (make-handler
   executable-pathname
   (lambda (package pathname)
     (let* ((real-pathname (executable-pathname package pathname))
            (port (do-open-file real-pathname)))
       (chmod "+x" real-pathname)
       port))))

(define %chmod-path (delay (find-exec-path "chmod")))

(define (chmod mode pathname)
  (cond ((force %chmod-path)
         => (lambda (chmod-path)
              (run-process #f chmod-path "+x" pathname)))
        (else
         (log/fhs 'warning "`chmod' not found in PATH"))))



(define (filename->man-section filename)
  (cond ((irregex-search section-irx filename)
         => (lambda (match)
              (string->number (irregex-match-substring match 1))))
        (else
         1))) ;lame default

(define section-irx (irregex '(: "." ($ (+ (~ numeric))))))

(define (do-open-file dest-pathname)
  (create-directory* (pathname-with-file dest-pathname #f))
  (let ((filename (->namestring dest-pathname)))
    (log/fhs 'debug "opening " filename)
    (open-file-output-port filename)))

(define (sh-wrapper-hook destination package)
  (iterate-values ((result '()))
      (for source.target
           (in-list '((programs . executables)
                      (package-programs . package-executables))))
    (cond ((package-category-inventory package (car source.target))
           => (lambda (programs)
                (cons (create-sh-wrappers destination
                                          package
                                          (car source.target)
                                          (cdr source.target)
                                          programs)
                      result)))
          (else
           result))))

(define (create-sh-wrappers destination
                            package
                            source-category
                            target-category
                            programs)
  (define (create-wrapper pathname)
    (call-with-port
        (transcoded-port (destination-open-file destination
                                                package
                                                target-category
                                                pathname)
                         (native-transcoder))
      (lambda (port)
        (log/fhs 'debug
                 "creating shell wrapper for " pathname
                 " in " (package-name package))
        (fmt port (dsp-sh-wrapper (destination-prefix destination)
                                  package
                                  (destination-pathname destination
                                                        package
                                                        source-category
                                                        pathname))))))
  (iterate! ((for cursor path (in-inventory-leafs programs)))
    (create-wrapper (make-pathname #f (reverse path) (inventory-name cursor))))
  (inventory-relabel programs target-category (inventory-data programs)))

(define (dsp-sh-wrapper prefix package program-pathname)
  (cat "#!/bin/sh\n"
       "# Shell wrapper for package " (package->string package " ") "\n"
       "\n"
       ;; FIXME: --r6rs
       "exec " *script-interpreter* " " (->namestring program-pathname) " \"$@\"\n"))


;;; Utilities

(define (build-pathname prefix template package pathname)
  (merge-pathnames
   pathname
   (pathname-as-directory
    (vals->pathname prefix
                    `((name . ,(symbol->string (package-name package))))
                    template))))
  
(define (resolve-template template vals)
  (cond ((symbol? template)
         (assq-ref vals template))
        ((pair? template)
         (string-concatenate (map (lambda (part) (resolve-template part vals)) template)))
        (else
         template)))

(define (vals->pathname base vals template)
  (receive (dir-parts file-part)
           (split-at (map (lambda (part) (resolve-template part vals)) template)
                     (- (length template) 1))
    (make-pathname (pathname-origin base)
                   (append (pathname-directory base) dir-parts)
                   (if (pair? (car file-part))
                       (string-concatenate (car file-part))
                       (car file-part)))))

(define logger:dorodango.fhs-destination
  (make-logger logger:dorodango 'fhs-destination))
(define log/fhs (make-fmt-log logger:dorodango.fhs-destination))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
