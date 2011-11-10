#!r6rs
;;; fhs.sls --- Destination conforming to the FHS

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (guildhall destination fhs)
  (export make-fhs-destination)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) split-at append-reverse)
          (only (guile) effective-version)
          (ice-9 match)
          (srfi :8 receive)
          (only (srfi :13)
                string-concatenate
                string-concatenate-reverse
                string-contains
                string-suffix?)
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
          (guildhall destination)
          (guildhall package)
          (guildhall bundle))

(define make-handler make-destination-handler)


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
     call-with-fhs-support-bundle
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

(define (call-with-fhs-support-bundle destination options receiver)
  (let ((support-bundle-dir (create-support-bundle destination options)))
    (call-with-input-bundle support-bundle-dir
      (lambda (bundle)
        (receive results (receiver bundle)
          ;;++remove temporary directory
          (apply values results))))))

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
        (fmt port (dsp-sh-wrapper destination
                                  package
                                  (destination-pathname destination
                                                        package
                                                        source-category
                                                        pathname))))))
  (iterate! ((for cursor path (in-inventory-leafs programs)))
    (create-wrapper (make-pathname #f (reverse path) (inventory-name cursor))))
  (inventory-relabel programs target-category (inventory-data programs)))

(define (dsp-sh-wrapper destination package program-pathname)
  (cat "#!/bin/sh\n"
       "# Shell wrapper for package " (package->string package " ") "\n"
       "\n"
       "exec " *script-interpreter* " " (->namestring program-pathname) " \"$@\"\n"))


;;; Support bundle creation

(define (create-support-bundle destination options)
  (let ((bundle-directory (create-temp-directory)))
    (copy-directory/substitute (support-bundle-directory)
                               (make-support-file-handler bundle-directory options)
                               (make-destination-substitutions destination))
    bundle-directory))

(define (copy-directory/substitute source-directory
                                   handler
                                   substitutions)
  (let ((source-directory (pathname-as-directory source-directory)))
    (iterate! (for entry (in-directory source-directory))
      (let ((source-pathname (pathname-with-file source-directory entry)))
        (match (handler source-pathname)
          (('recurse target-pathname new-handler)
           (create-directory target-pathname)
           (copy-directory/substitute source-pathname
                                      new-handler
                                      substitutions))
          (('substitute target-pathname)
           (call-with-file-i/o-ports source-pathname target-pathname
               (lambda (port)
                 (transcoded-port port (native-transcoder)))
             (lambda (in out)
               (copy-port/substitute in out substitutions))))
          (('copy target-pathname)
           (call-with-file-i/o-ports source-pathname target-pathname
               values
             copy-port))
          ('ignore #f))))))

(define (make-support-file-handler target-directory options)
  (let ((target-directory (pathname-as-directory target-directory)))
    (lambda (source-pathname)
      (cond ((file-directory? source-pathname)
             (let ((target-directory
                    (pathname-as-directory
                     (pathname-with-file target-directory
                                         (pathname-file source-pathname)))))
               (list 'recurse
                     target-directory
                     (make-support-file-handler target-directory options))))
            (else
             (let* ((entry (file-namestring source-pathname)))
               (if (string-suffix? ".in" entry)
                   'ignore
                   (list 'copy (pathname-with-file target-directory entry)))))))))

(define (call-with-file-i/o-ports in-pathname out-pathname maybe-transcoded receiver)
  (call-with-port (maybe-transcoded
                   (open-file-input-port (->namestring in-pathname)))
    (lambda (in-port)
      (call-with-port (maybe-transcoded
                       (open-file-output-port (->namestring out-pathname)))
        (lambda (out-port)
          (receiver in-port out-port))))))

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

(define (support-bundle-directory)
  (or (find-file (make-pathname #f '("guildhall" "private" "support-bundle") #f)
                 (library-search-paths))
      (error 'support-bundle-directory
             "unable to find dorodango support bundle directory")))


(define (make-destination-substitutions destination)
  `(("@R6RS_LIBRARY_PATH@"
     . ,(->namestring
         (pathname-join (destination-prefix destination)
                        (make-pathname #f fhs-libraries-template #f))))))

(define (copy-port/substitute in out substitutions)
  (loop ((for line (in-port in get-line)))
    (put-string out (string-replace line substitutions))
    (newline out)))


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
;; scheme-indent-styles: (foof-loop as-match)
;; End:
