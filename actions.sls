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
  (export create-bundle)
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

(define (create-bundle bundle-filename directories)
  (let ((filename (cond ((string-suffix? ".zip" bundle-filename)
                         bundle-filename)
                        (else
                         (string-append bundle-filename ".zip")))))
    (delete-file filename)
    (message "Creating " filename)
    (match directories
      ((directory)
       (zip-files filename
                  directory
                  (list-files directory (make-pathname #f '() #f))))
      (_
       (loop ((for directory (in-list directories)))
         (zip-files filename
                    (pathname-container directory)
                    (list-files directory
                                (->pathname
                                 `((,(last (pathname-directory directory))))))))))))

(define (zip-files zip-filename directory pathnames)
  (let ((zip-path (force %zip-path)))
    (unless zip-path
      (die "`zip' executable not found in PATH."))
    (with-working-directory directory
      (lambda ()
        (call-with-values
          (lambda ()
            (call-with-process-input #f (list zip-path "-q" "-@" zip-filename)
              (lambda (port)
                (loop ((for pathname (in-list pathnames)))
                  (put-string port (->namestring pathname))
                  (put-string port "\n")))))
            (process-status-checker zip-path 0))))))

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
  (let ((directory (pathname-as-directory directory)))
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
                             (".bzr" ,%bzr-path "ls" "--recursive" "--versioned")
                             ("_darcs" ,%darcs-path "query" "files")))))
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
