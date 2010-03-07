;;; destination.sls --- 

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

;; Currently, FHS destinations install a wrapper shell script for each
;; R6RS program. This wrapper sets the environment variable
;; R6RS_LIBRARY_PATH and executes `r6rs-script', which in turn sets
;; the implementation-specific library search path based on that
;; setting. Once SRFI 103 (and hence R6RS_LIBRARY_PATH) is widely
;; implemented, r6rs-script may be changed to a symlink or dropped in
;; favor of `scheme-script', as recommended by R6RS D.2.

;;; Code:
#!r6rs

(library (dorodango destination)
  (export destination?
          destination-name
          destination-prefix
          destination-pathnames
          setup-destination
          destination-install
          destination-without-categories
          
          make-fhs-destination)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) split-at)
          (srfi :8 receive)
          (only (srfi :13) string-concatenate)
          (spells alist)
          (only (spells misc) and=>)
          (spells lazy)
          (spells fmt)
          (spells record-types)
          (spells pathname)
          (spells filesys)
          (spells logging)
          (spells foof-loop)
          (spells process) ;just needed for `chmod'
          (spells sysutils) ;ditto
          (dorodango private utils)
          (dorodango package))

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
       (documentation
        . ,(make-simple-handler prefix '("share" "doc" ("libr6rs-" name))))
       (programs
        . ,(make-program-handler/sh-wrapper
            prefix
            '("share" ("libr6rs-" name) "programs")
            '("bin")))))))

(define script-interpreter-pathname '(("bin") "r6rs-script"))

(define fhs-script-interpreters
  '((ikarus
     "#!/bin/sh"
     "export IKARUS_LIBRARY_PATH=\"$R6RS_LIBRARY_PATH\""
     "exec ikarus --r6rs-script \"$@\"")
    (ypsilon
     "#!/bin/sh"
     "export YPSILON_SITELIB=\"$R6RS_LIBRARY_PATH\""
     "dir=`echo \"$R6RS_LIBRARY_PATH\" | sed -e 's/^\\([^:]*\\)/\\1/'`"
     "export YPSILON_ACC=\"$HOME/.cache/ypsilon/$dir\""
     "mkdir -p \"$YPSILON_ACC\""
     "exec ypsilon --r6rs -- \"$@\"")))

(define (fhs-destination-setup destination options)
  (define (implementation-pathname pathname implementation)
    (pathname-add-type pathname (symbol->string implementation)))
  (let ((implementation (assq-ref options 'implementation))
        (interpreter-pathname (pathname-join (destination-prefix destination)
                                             script-interpreter-pathname)))
    (log/fhs 'info
             (cat "initializing destination `" (destination-name destination)
                  "' in " (dsp-pathname (destination-prefix destination))
                  " for " implementation))
    (create-directory* (pathname-with-file interpreter-pathname #f))
    ;; Put the scripts into their place
    (loop ((for name.lines (in-list fhs-script-interpreters)))
      (let ((pathname (implementation-pathname interpreter-pathname (car name.lines))))
        (call-with-output-file/atomic pathname
          (lambda (port)
            (fmt port (fmt-join/suffix dsp (cdr name.lines) "\n"))))
        (chmod "+x" pathname)))
    ;; Create implementation-choosing symlink
    (delete-file interpreter-pathname) ;++ make the link creation atomic
    (create-symbolic-link
     (implementation-pathname
      (make-pathname #f '() (pathname-file interpreter-pathname))
      implementation)
     interpreter-pathname)))

(define fhs-libraries-template '("share" "r6rs-libs"))

(define (make-simple-handler prefix template)
  (make-handler
   (lambda (package pathname)
     (list (destination-pathname prefix template package pathname)))
   (lambda (package pathname extractor)
     (let ((dest-pathname (destination-pathname prefix template package pathname)))
       (create-directory* (pathname-with-file dest-pathname #f))
       (let ((filename (->namestring dest-pathname)))
         (log/fhs 'debug "installing " filename)
         (call-with-port (open-file-output-port filename)
           extractor))))))

(define (make-program-handler/sh-wrapper prefix
                                         program-template
                                         sh-wrapper-template)
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
       (create-directory* (pathname-with-file program-pathname #f))
       (let ((filename (->namestring program-pathname)))
         (log/fhs 'debug "installing " filename)
         (call-with-port (open-file-output-port filename)
           extractor))
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
         (log/fhs 'warn "`chmod' not found in PATH"))))

(define (dsp-sh-wrapper prefix package program-pathname)
  (let ((library-path
         (destination-pathname prefix
                               fhs-libraries-template
                               package
                               (make-pathname #f '() #f))))
    (cat "#!/bin/sh\n"
         "# Shell wrapper for package " (package->string package " ") "\n"
         "\n"
         "export R6RS_LIBRARY_PATH=\"" (->namestring library-path) ;++quote
         "${R6RS_LIBRARY_PATH:+:$R6RS_LIBRARY_PATH}\"\n"
         "exec " (->namestring (pathname-join prefix script-interpreter-pathname))
         " " (->namestring program-pathname) " \"$@\"\n")))

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
