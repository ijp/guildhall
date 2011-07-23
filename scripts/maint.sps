;;; maint.sps --- Maintainance tool for dorodango

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

;;; Code:
#!r6rs

(import (except (rnrs) delete-file file-exists?)
        (srfi :45 lazy)
        (guild ext foof-loop)
        (guild ext foof-loop nested)
        (guild ext fmt)
        (spells match)
        (spells pathname)
        (spells filesys)
        (spells process)
        (spells sysutils)
        (only (guild private utils) rm-rf)
        (guild package)
        (guild actions))

(define bundle-base (make-pathname '(back) '() #f))

(define transitive-dependencies
  '(srfi
    wak-common
    wak-syn-param
    wak-riastreams
    wak-foof-loop
    wak-fmt
    wak-irregex
    wak-parscheme
    wak-trc-testing
    wak-ssax
    spells
    industria
    ocelotl))

(define (run-dist name-suffix append-version)
  (let* ((package (call-with-input-file "pkg-list.scm"
                    (lambda (port)
                      (parse-package-form (read port)))))
         (renamed-package
          (make-package (string->symbol (string-append
                                         (symbol->string (package-name package))
                                         name-suffix))
                        (append (package-version package) append-version)))
         (renamed-filename (package->string renamed-package "-"))
         (tmp-dir (create-temp-directory '(())))
         (dist-dir (pathname-join tmp-dir `((,renamed-filename)))))
    ;; Create hardlink tree
    (loop ((for directory-basename
                (in-list (cons (package-name package)
                               transitive-dependencies))))
      (let ((directory (pathname-as-directory
                        (pathname-with-file bundle-base directory-basename))))
        (loop ((for pathname (in-list (list-files directory '(())))))
          (let ((dest-pathname (pathname-join dist-dir
                                              `((,directory-basename))
                                              pathname)))
            (create-directory* (pathname-container dest-pathname))
            (create-hard-link (pathname-join directory pathname)
                              dest-pathname)))))
    (run-process #f
                 (force %tar-path)
                 "-C" tmp-dir
                 "-cjf" (string-append renamed-filename ".tar.bz2")
                 renamed-filename)
    (rm-rf tmp-dir)))

(define %tar-path
  (delay (or (find-exec-path "tar")
             (die "`tar' not found in PATH."))))

(define (die formatter)
  (fmt (current-error-port) (cat "tool.sps: " formatter "\n"))
  (exit #f))

(define (main argv)
  (match (cdr argv)
    (("dist")
     (run-dist "-full" '()))
    (("dist" append-version)
     (run-dist "-full" (string->package-version append-version)))
    (args
     (cond ((< (length args) 2)
            (die (cat "need at least one argument.")))
           (else
            (die (cat "invalid invocation: " (fmt-join dsp args " ") ".")))))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
