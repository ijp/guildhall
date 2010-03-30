;;; actions.sls --- Dorodango actions

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

;; This library provides some of the primitives upon which the
;; command-line UI is built.

;;; Code:
#!r6rs

(library (dorodango actions)
  (export read-package-lists
          create-bundle
          list-files
          zip-files
          find-pkg-list-files
          scan-bundles-in-directory
          symlink-bundle)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) make-list last unfold)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (only (srfi :13) string-suffix?)
          (only (spells misc) unspecific)
          (spells lazy)
          (spells alist)
          (spells match)
          (spells foof-loop)
          (spells nested-foof-loop)
          (spells fmt)
          (spells pathname)
          (spells filesys)
          (spells process)
          (only (spells sysutils) find-exec-path)
          (dorodango inventory)
          (dorodango inventory mapping)
          (dorodango package)
          (dorodango bundle)
          (dorodango ui)
          (dorodango private utils))

(define (read-package-lists pkg-list-files append-version)
  (collect-list (for pathname (in-list pkg-list-files))
    (let ((packages (call-with-input-file (->namestring pathname)
                      read-pkg-list)))
      (if (null? append-version)
          packages
          (collect-list (for package (in-list packages))
            (package-modify-version package
                                    (lambda (version)
                                      (append version append-version))))))))

(define (read-pkg-list port)
  (unfold eof-object?
          parse-package-form
          (lambda (seed) (read port))
          (read port)))

(define (create-bundle bundle-pathname
                       directories
                       packages-list
                       rewrite-pkg-list-files?)
  (let* ((bundle-pathname (->pathname bundle-pathname))
         (bundle-directory (make-pathname
                            #f
                            (list (file-name (pathname-file bundle-pathname)))
                            #f))
         (pathname (cond ((pathname-has-file-type? bundle-pathname "zip")
                          bundle-pathname)
                        (else
                         (pathname-add-type bundle-pathname "zip"))))
        (n-directories (length directories)))
    (define (run-zip directory graft-point)
      (let* ((tmp-dir (create-temp-directory))
             (graft-dir (pathname-as-directory graft-point))
             (symlink (pathname-join tmp-dir graft-dir))
             (symlink-container (pathname-container symlink)))
        (create-directory* symlink-container)
        (create-symbolic-link (pathname-join (working-directory) directory)
                              ;; ensure `fileness' of target argument
                              (pathname-with-file
                               symlink-container
                               (last (pathname-directory symlink))))
        (zip-files pathname
                   (pathname-join symlink
                                  (make-pathname
                                   (make-list (length (pathname-directory
                                                       graft-dir))
                                              'back)
                                   '()
                                   #f))
                   (list-files directory graft-point))
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
          => (unless (null? files)
               (zip-files pathname tmp-dir files)))
        (rm-rf tmp-dir)))
    (define (component-graft-point directory packages)
      (cond ((> n-directories 1)
             (let ((directory-part (pathname-directory directory)))
               (when (null? directory-part)
                 (fatal (cat "unable to compute top-level directory for"
                             " bundle component `"
                             (dsp-pathname directory) "'.")))
               (pathname-with-file bundle-directory (last directory-part))))
            (else
             bundle-directory)))
    (delete-file pathname)
    (message "Creating " (->namestring pathname))
    (loop ((for directory (in-list directories))
           (for packages (in-list packages-list))
           (let graft-point (component-graft-point directory packages))
           (for rewrite-list (listing (make-rewriter graft-point packages)
                                      (if rewrite-pkg-list-files?))))
      => (run-rewriters rewrite-list)
      (run-zip directory graft-point))))

(define (zip-files zip-filename directory pathnames)
  (let ((zip-path (force %zip-path)))
    (unless zip-path
      (fatal "`zip' executable not found in PATH."))
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
           (fatal (cat "`zip' was terminated by signal " signal)))
          ((= status 0)
           (if (null? results)
               (unspecific)
               (apply values results)))
          (else
           (fatal (cat "`zip' returned with unexpected status " status))))))

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

(define (find-pkg-list-files directories)
  (define (subdirectory-pkg-list-files directory)
    (loop ((for filename (in-directory directory))
           (let pathname
               (pathname-join directory
                              (make-pathname #f (list filename) "pkg-list.scm")))
           (for result (listing pathname (if (file-exists? pathname)))))
      => result))
  (loop ((for directory (in-list directories))
         (for result
              (appending-reverse
               (let ((pathname (pathname-with-file directory "pkg-list.scm")))
                 (if (file-exists? pathname)
                     (list pathname)
                     (subdirectory-pkg-list-files directory))))))
    => (reverse result)))

(define (scan-bundles-in-directory directory base)
  (let ((directory (pathname-as-directory directory))
        (base (pathname-as-directory base)))
    (define (scan-entry filename)
      (let ((pathname (pathname-with-file directory filename)))
        (cond ((file-directory? pathname)
               (scan-bundles-in-directory pathname
                                          (pathname-with-file base filename)))
              ((and (file-regular? pathname)
                    (string-suffix? ".zip" (file-namestring pathname)))
               (call-with-input-bundle pathname (bundle-options no-inventory)
                 (lambda (bundle)
                   (collect-list ((for package (in-list (bundle-packages bundle))))
                     (cons package (pathname-with-file base filename)))))))))
    (loop ((for filename (in-directory directory))
           (for result (appending-reverse (scan-entry filename))))
      => result)))


;;; Bundle symlinking

(define (symlink-bundle bundle-directory
                        target-directory
                        force?
                        deep?
                        consider-package?)
  (define (assert-directory pathname)
    (unless (file-directory? pathname)
      (fatal (cat "not a directory: " (->namestring pathname)))))
  (let ((target-directory (merge-pathnames (pathname-as-directory target-directory)
                                           (working-directory)))
        (bundle-directory (merge-pathnames (pathname-as-directory bundle-directory)
                                           (working-directory))))
    (assert-directory bundle-directory)
    (when (file-exists? target-directory)
      (if force?
          (assert-directory target-directory)
          (fatal (cat "target directory `" (->namestring target-directory)
                      "' must not exist."))))
    (let ((bundle (open-input-bundle bundle-directory)))
      (loop continue ((for package (in-list (bundle-packages bundle)))
                      (with target-inventory (make-inventory 'target #f)))
        => (create-inventory-symlinks target-inventory
                                      (bundle-inventory bundle)
                                      bundle-directory
                                      target-directory
                                      deep?)
        (if (consider-package? package)
            (let ((libraries (package-category-inventory package 'libraries)))
              (receive (target source)
                       (apply-inventory-mapper identity-inventory-mapper
                                               target-inventory
                                               libraries)
                (continue (=> target-inventory target))))
            (continue))))))

(define (create-inventory-symlinks inventory
                                   original
                                   base-directory
                                   target-directory
                                   deep?)
  (define (symlink inventory-pathname real-pathname)
    (let ((inventory-pathname (merge-pathnames inventory-pathname
                                               target-directory))
          (real-pathname (merge-pathnames real-pathname
                                          base-directory)))
      (create-directory* (pathname-with-file inventory-pathname #f))
      (create-symbolic-link (enough-pathname real-pathname
                                             inventory-pathname)
                            inventory-pathname)))
  (define (symlink-tree-shallowly inventory directory)
    (loop ((for cursor (in-inventory inventory)))
      (if (inventory-leaf? cursor)
          (symlink (pathname-with-file directory (inventory-name cursor))
                   (inventory-data cursor))
          (let* ((prefix (inventory-prefix cursor))
                 (original-cursor (and prefix (inventory-ref original prefix)))
                 (pathname (pathname-with-file directory
                                               (inventory-name cursor))))
            (if (and original-cursor
                     (inventory-container? original-cursor)
                     (isomorphic-inventories? cursor original-cursor))
                (symlink pathname (make-pathname #f prefix #f))
                (symlink-tree-shallowly cursor
                                        (pathname-as-directory pathname)))))))
  (define (inventory-prefix inventory)
    (loop continue ((for cursor (in-inventory inventory))
                    (with result #f))
      => result
      (define (iterate prefix)
        (and-let* ((prefix (if result
                               (common-prefix string=? prefix result)
                               prefix)))
          (continue (=> result prefix))))
      (cond ((inventory-data cursor)
             => (lambda (pathname)
                  (iterate (pathname-directory pathname))))
            (else
             (and-let* ((prefix (inventory-prefix cursor)))
               (iterate prefix))))))
  (cond (deep?
         (loop ((with cursor
                      (inventory-cursor inventory)
                      (inventory-cursor-next cursor))
                (while cursor))
           (symlink (make-pathname
                     #f
                     (reverse (inventory-cursor-path cursor))
                     (inventory-cursor-name cursor))
                    (inventory-cursor-data cursor))))
        (else
         (symlink-tree-shallowly inventory (make-pathname #f '() #f)))))

(define (common-prefix =? xs ys)
  (let loop ((xs xs) (ys ys) (prefix '()))
    (cond ((or (null? xs) (null? ys))
           (reverse prefix))
          ((=? (car xs) (car ys))
           (loop (cdr xs) (cdr ys) (cons (car xs) prefix)))
          (else
           (reverse prefix)))))

(define (inventory->sorted-alist inventory)
  (list-sort (lambda (item-1 item-2)
               (string<? (car item-1) (car item-2)))
             (collect-list-reverse (for cursor (in-inventory inventory))
               (cons (inventory-name cursor)
                     (if (inventory-leaf? cursor)
                         (inventory-data cursor)
                         cursor)))))

(define (isomorphic-inventories? inventory-1 inventory-2)
  (loop continue
      ((for name.data-1 lst-1 (in-list (inventory->sorted-alist inventory-1)))
       (for name.data-2 lst-2 (in-list (inventory->sorted-alist inventory-2))))
    => (and (null? lst-1) (null? lst-2))
    (let ((data-1 (cdr name.data-1))
          (data-2 (cdr name.data-2)))
      (and (string=? (car name.data-1) (car name.data-2))
           (cond ((and (pathname? data-1) (pathname? data-2))
                  (pathname=? data-1 data-2))
                 ((and (inventory? data-1) (inventory? data-2))
                  (isomorphic-inventories? data-1 data-2))
                 (else
                  #f))
           (continue)))))


)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
