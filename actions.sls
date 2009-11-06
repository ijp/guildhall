;;; commands.sls --- Dorodango commands

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

(library (dorodango actions)
  (export create-bundle
          list-files
          zip-files)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) last)
          (only (srfi :13) string-suffix?)
          (only (spells misc) unspecific)
          (spells lazy)
          (spells alist)
          (spells match)
          (spells foof-loop)
          (spells fmt)
          (spells pathname)
          (spells filesys)
          (spells process)
          (only (spells sysutils) find-exec-path)
          (dorodango package)
          (dorodango ui cmdline)
          (dorodango private utils))

(define (create-bundle bundle-filename
                       directories
                       packages-list
                       rewrite-pkg-list-files?)
  (let ((filename (cond ((string-suffix? ".zip" bundle-filename)
                         bundle-filename)
                        (else
                         (string-append bundle-filename ".zip"))))
        (n-directories (length directories)))
    (define (run-zip directory toplevel-pathname)
      (let* ((tmp-dir (create-temp-directory))
             (symlink (pathname-join tmp-dir toplevel-pathname)))
        (create-directory* (pathname-container symlink))
        (create-symbolic-link (pathname-join (working-directory) directory)
                              symlink)
        (zip-files filename
                   (pathname-container symlink)
                   (list-files directory toplevel-pathname))
        (rm-rf tmp-dir)))
    (define (make-rewriter toplevel-pathname packages)
      (lambda (tmp-dir)
        (let* ((directory (pathname-as-directory
                           (pathname-join tmp-dir toplevel-pathname)))
               (pathname (pathname-join directory "pkg-list.scm")))
          (create-directory* directory)
          (call-with-output-file (->namestring pathname)
            (lambda (port)
              (fmt port (fmt-join/suffix
                         (lambda (package)
                           (pretty/unshared (package->form package)))
                         packages
                         "\n"))))
          (pathname-join (pathname-as-directory toplevel-pathname)
                         "pkg-list.scm"))))
    (define (run-rewriters rewrite-list)
      (let ((tmp-dir (create-temp-directory)))
        (loop ((for rewriter (in-list rewrite-list))
               (for files (listing (rewriter tmp-dir))))
          => (zip-files filename tmp-dir files))
        (rm-rf tmp-dir)))
    (delete-file filename)
    (message "Creating " filename)
    (loop ((for directory (in-list directories))
           (for packages (in-list packages-list))
           (let-values (zip-directory toplevel-pathname)
             (match packages
               ((package)
                (values directory (package-identifier package)))
               (_
                (let ((directory-part (pathname-directory directory)))
                  (cond ((null? directory-part)
                         (when (> n-directories 1)
                           (die (cat "unable to compute top-level directory for"
                                     " bundle component `"
                                     (dsp-pathname directory) "'.")))
                         (values (make-pathname #f '() #f) (make-pathname #f '() #f)))
                        (else
                         (values (pathname-container directory)
                                 (->pathname `((,(last directory-part)))))))))))
           (for rewrite-list (listing (make-rewriter toplevel-pathname packages)
                                      (if rewrite-pkg-list-files?))))
      => (run-rewriters rewrite-list)
      (run-zip zip-directory toplevel-pathname))))

(define (zip-files zip-filename directory pathnames)
  (let ((zip-path (force %zip-path)))
    (unless zip-path
      (die "`zip' executable not found in PATH."))
    (let ((zip-filename (pathname-join (working-directory) zip-filename)))
      (with-working-directory directory
        (lambda ()
          (call-with-values
              (lambda ()
                (call-with-process-input #f (list zip-path "-q" "-@" zip-filename)
                  (lambda (port)
                    (loop ((for pathname (in-list pathnames)))
                      (put-string port (->namestring pathname))
                      (put-string port "\n")))))
            (process-status-checker zip-path 0)))))))

(define (process-status-checker program-path expected-status)
  (lambda (status signal . results)
    (cond (signal
           (die (cat "`zip' was terminated by signal " signal)))
          ((= status 0)
           (if (null? results)
               (unspecific)
               (apply values results)))
          (else
           (die (cat "`zip' returned with unexpected status " status))))))

(define %zip-path (delay (find-exec-path "zip")))

(define %git-path (delay (find-exec-path "git")))
(define %bzr-path (delay (find-exec-path "bzr")))
(define %darcs-path (delay (find-exec-path "darcs")))

(define (list-files directory prefix)
  (let ((directory (pathname-as-directory directory))
        (prefix (pathname-as-directory prefix)))
    (define (run-rcs-lister argv)
      (with-working-directory directory
        (lambda ()
          (map (lambda (line)
                 (pathname-join prefix (->pathname line)))
               (call-with-values
                 (lambda () (apply run-process/lines #f argv))
                 (process-status-checker (car argv) 0))))))
    (define (builtin-lister)
      (loop ((for filename (in-directory directory))
             (for result
                  (appending-reverse
                   (let ((pathname (pathname-with-file directory filename)))
                     (if (file-directory? pathname)
                         (if (member filename '(".git" ".bzr" ".hg" "_darcs" ".svn"))
                             '()
                             (list-files pathname
                                         (pathname-join prefix `((,filename)))))
                         (list (pathname-with-file prefix filename)))))))
        => (reverse result)))
    (loop continue
        ((for rcs (in-list `((".git" ,%git-path "ls-files")
                             (".bzr" ,%bzr-path "ls"
                              "--recursive" "--versioned"
                              "--kind=file")
                             ("_darcs" ,%darcs-path "query" "files"
                              "--no-directories")))))
      => (builtin-lister)
      (match rcs
        ((dir program-path-promise . arguments)
         (if (file-directory? (pathname-with-file directory dir))
             (cond ((force program-path-promise)
                    => (lambda (program)
                         (run-rcs-lister (cons program arguments))))
                   (else
                    (builtin-lister)))
             (continue)))))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
