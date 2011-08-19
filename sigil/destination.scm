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

(library (sigil destination)
  (export destination?
          destination-name
          destination-prefix
          destination-pathnames
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
          (sigil ext fmt)
          (sigil ext irregex)
          (sigil ext foof-loop)
          (sigil spells record-types)
          (sigil spells pathname)
          (sigil spells filesys)
          (sigil spells logging)
          (sigil spells process) ;just needed for `chmod'
          (sigil spells sysutils) ;ditto
          (sigil private utils)
          (sigil build-info)
          (sigil package))

(define-record-type* destination
  (make-destination name prefix setup categories)
  ())

(define-functional-fields destination
  name prefix setup categories)

(define (destination-without-categories destination categories)
  (destination-modify-categories
   destination
   (lambda (categories)
     (remp (lambda (entry)
             (memq (car entry) categories))
           categories))))

(define (destination-pathnames destination package category pathname)
  (cond ((assq-ref (destination-categories destination) category)
         => (lambda (handler)
              (handler-map handler package pathname)))
        (else
         '())))

(define (setup-destination destination options)
  ((destination-setup destination) destination options))

(define (destination-install destination package category pathname extractor)
  (and=> (assq-ref (destination-categories destination) category)
         (lambda (handler)
           (handler-install handler package pathname extractor))))

(define-record-type handler
  (fields mapper installer))

(define (handler-map handler package pathname)
  ((handler-mapper handler) package pathname))

(define (handler-install handler package pathname extractor)
  ((handler-installer handler) package pathname extractor))


;;; FHS destination

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
       (documentation
        . ,(make-simple-handler prefix fhs-doc-template))
       (programs
        . ,(make-program-handler
            prefix
            (list "share" "guile" "site-programs" (effective-version))
            '("bin")))
       (man . ,(make-man-page-handler prefix))))))

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
  ; Nothing to do!
  #t)

(define fhs-libraries-template
  (list "share" "guile" "site" (effective-version)))

(define fhs-ccache-template
  (list "lib" "guile" (effective-version) "site-ccache"))

(define fhs-doc-template
  (list "share" "doc" "guile" (effective-version) 'name))

(define (make-simple-handler prefix template)
  (make-handler
   (lambda (package pathname)
     (list (destination-pathname prefix template package pathname)))
   (lambda (package pathname extractor)
     (do-install-file (destination-pathname prefix template package pathname)
                      extractor))))

(define (make-man-page-handler prefix)
  (define (man-page-pathname pathname)
    (let ((section (filename->man-section (file-namestring pathname))))
      (pathname-join
       prefix
       `(("share" "man" ,(string-append "man" (number->string section))))
       pathname)))
  (make-handler
   (lambda (package pathname)
     (list (man-page-pathname pathname)))
   (lambda (package pathname extractor)
     (do-install-file (man-page-pathname pathname) extractor))))

(define (filename->man-section filename)
  (cond ((irregex-search section-irx filename)
         => (lambda (match)
              (string->number (irregex-match-substring match 1))))
        (else
         1))) ;lame default

(define section-irx (irregex '(: "." ($ (+ (~ numeric))))))

(define (do-install-file dest-pathname extractor)
  (create-directory* (pathname-with-file dest-pathname #f))
  (let ((filename (->namestring dest-pathname)))
    (log/fhs 'debug "installing " filename)
    (call-with-port (open-file-output-port filename)
      extractor)))

(define (make-program-handler prefix program-template sh-wrapper-template)
  (make-handler
   (lambda (package pathname)
     (list (destination-pathname prefix sh-wrapper-template package pathname)
           (destination-pathname prefix program-template package pathname)))
   (lambda (package pathname extractor)
     (let ((program-pathname (destination-pathname prefix
                                                   program-template
                                                   package
                                                   pathname))
           (sh-wrapper-pathname (destination-pathname prefix
                                                      sh-wrapper-template
                                                      package
                                                      pathname)))
       (do-install-file program-pathname extractor)
       (create-directory* (pathname-with-file sh-wrapper-pathname #f))
       (let ((filename (->namestring sh-wrapper-pathname)))
         (log/fhs 'debug "creating shell wrapper" filename)
         (call-with-output-file filename
           (lambda (port)
             (fmt port (dsp-sh-wrapper prefix package program-pathname))))
         (chmod "+x" filename))))))

(define %chmod-path (delay (find-exec-path "chmod")))

(define (chmod mode pathname)
  (cond ((force %chmod-path)
         => (lambda (chmod-path)
              (run-process #f chmod-path "+x" pathname)))
        (else
         (log/fhs 'warning "`chmod' not found in PATH"))))

(define (dsp-sh-wrapper prefix package program-pathname)
  (let ((library-path
         (destination-pathname prefix
                               fhs-libraries-template
                               package
                               (make-pathname #f '() #f))))
    (cat "#!/bin/sh\n"
         "# Shell wrapper for package " (package->string package " ") "\n"
         "\n"
         ;; FIXME: --r6rs
         "exec " *script-interpreter* " " (->namestring program-pathname) " \"$@\"\n")))

(define (destination-pathname prefix template package pathname)
  (pathname-join
   (pathname-as-directory
    (vals->pathname prefix
                    `((name . ,(symbol->string (package-name package))))
                    template))
   pathname))
  
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
