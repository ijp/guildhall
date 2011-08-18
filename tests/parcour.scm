;;; parcour.sps --- Test parcour for the `doro' CLI

;; Copyright (C) 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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
        (only (srfi :1) last)
        (srfi :8 receive)
        (srfi :19 time)
        (srfi :39 parameters)
        (srfi :98 os-environment-variables)
        (sigil ext foof-loop)
        (sigil ext foof-loop nested)
        (sigil ext fmt)
        (spells pathname)
        (spells filesys)
        (spells process)
        (only (spells misc) scheme-implementation)
        (sigil private utils)
        (sigil ui cmdline))

(define verbose? (make-parameter #t))
(define dry-run? (make-parameter #f))

(define global-args (make-parameter '()))
(define prefix-directory (make-parameter #f))

(define http-temp-directory
  (pathname-join (pathname-as-directory (get-environment-variable "HOME"))
                 '(("public_html" "tmp"))))

(define http-temp-directory-uri
  (string-append "http://localhost/~" (get-environment-variable "USER") "/tmp/"))

(define (prefix-pathname pathname)
  (pathname-join (prefix-directory) pathname))

(define (dsp* object)
  (if (pathname? object)
      (dsp (->namestring object))
      (dsp object)))

(define (->string-list lst)
  (collect-list (for element (in-list lst))
    (cond ((string? element) element)
          ((pathname? element) (->namestring element))
          (else
           (assertion-violation '->string-list
                                "cannot coerce to string" element)))))

(define (run . args)
  (let ((argv (append (list "doro") (global-args) args)))
    (when (or (dry-run?) (verbose?))
      (fmt #t "% " (fmt-join dsp* argv " ") "\n"))
    (unless (dry-run?)
      (run-cmdline-ui (->string-list argv)))))

(define (run-installed . args)
  (apply run/checked
         (prefix-pathname '(("bin") "doro"))
         (append (global-args) args)))

(define (run/checked program . args)
  (when (or (dry-run?) (verbose?))
    (fmt #t "% " (fmt-join dsp* (cons program args) " ") "\n"))
  (unless (dry-run?)
    (receive (status signal) (apply run-process #f program args)
      (unless (eqv? 0 status)
        (error 'run/checked
               "unexpected exit status from program" status program)))))

(define (do-install)
  (run "init" "--implementation" (symbol->string (scheme-implementation)))
  (run "install" "--bundle" ".." "dorodango")
  #;
  (fmt #t "* Compiling libraries used by doro\n")
  #;
  (run/checked
   (prefix-pathname '(("bin") "r6rs-script"))
   "--compile" (prefix-pathname '(("share" "libr6rs-dorodango" "programs")
                                  "doro"))))

;; Basic checks that the installation works
(define (do-installation-test)
  (run-installed "--version")
  (run-installed "list"))

(define package-list
  '(srfi spells industria wak-parscheme ocelotl))

(define (with-repository uri-string thunk)
  (parameterize ((global-args (append (list "-r" uri-string) (global-args))))
    (thunk)))

(define (populate-repository repo-dir append-version)
  (loop ((for package (in-list package-list)))
    (run "create-bundle"
         "--append-version" append-version
         "-o" (pathname-with-file
               repo-dir
               (string-append (symbol->string package) "_" append-version ".zip"))
         (make-pathname '(back) '() (symbol->string package))))
    (with-working-directory repo-dir
      (lambda ()
        (run "scan-bundles" "-o" "available.scm" "."))))

(define (do-upgrade)
  (let ((repo-dir (create-temp-directory http-temp-directory)))
    (populate-repository repo-dir "1")
    (with-repository (string-append "file://" (->namestring repo-dir))
      (lambda ()
        (run "update")
        (run "upgrade")))
    (populate-repository repo-dir "2")
    (with-repository (string-append http-temp-directory-uri 
                                    (last (pathname-directory repo-dir)))
      (lambda ()
        (run "update")
        (run "install" "srfi" "spells" "dorodango")))
    (rm-rf repo-dir)))

(define (do-remove)
  (run "remove" "srfi"))

(define (parcour)
  (let ((prefix (create-temp-directory)))
    (parameterize ((dry-run? #f)
                   (prefix-directory prefix)
                   (global-args (list "--yes" "--no-config" "--prefix" prefix)))
      (do-install)
      (do-installation-test)
      (do-upgrade)
      (do-remove)
      (rm-rf (prefix-directory)))))

(parcour)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
