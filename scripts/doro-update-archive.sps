#!r6rs
;;; doro-update-archive.sps --- Archive maintainence script

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(import (except (rnrs) delete-file file-exists?)
        (only (srfi :13)
              string-suffix?)
        (srfi :8 receive)
        (srfi :39 parameters)
        (srfi :45 lazy)
        (srfi :67 compare-procedures)
        (wak fmt)
        (wak foof-loop)
        (wak foof-loop nested)
        (only (spells misc) and=>)
        (spells tracing) ;debug
        (spells process)
        (spells pathname)
        (spells filesys)
        (spells sysutils)
        (spells match)
        (spells args-fold)
        (spells record-types)
        (only (dorodango private utils)
              in-hashtable
              location->pathname
              pathname->location
              pathname-add-type)
        (dorodango bundle)
        (dorodango package))


;;; Archive

(define-record-type* archive
  (make-archive directory)
  ((bundle-refcounts (make-hashtable pathname-hash pathname=?))
   (packages (make-eq-hashtable))))

(define (archive-add-package! archive package)
  (let ((bundle-refcounts (archive-bundle-refcounts archive)))
    (define (package-removed package)
      (hashtable-update!
       bundle-refcounts
       (package-location package)
       (lambda (refcount)
         (let ((new-refcount (- refcount 1)))
           (or (>= new-refcount 0)
               (assertion-violation 'archive-add-package!
                                    "bundle reference count inconsistency"
                                    package))
           new-refcount))
       0))
    (define (update-package old-package)
      (cond ((or (not old-package)
                 (package-version<? (package-version old-package)
                                    (package-version package)))
             (hashtable-update! bundle-refcounts
                                (package-location package)
                                (lambda (count) (+ 1 count))
                                0)
             (when old-package
               (package-removed old-package))
             package)
            (else
             (hashtable-update! bundle-refcounts
                                (package-location package)
                                values
                                0)
             old-package)))
    (hashtable-update! (archive-packages archive)
                       (package-name package)
                       update-package
                       #f)))

(define (open-archive directory)
  (let* ((directory (pathname-as-directory directory))
         (archive (make-archive directory)))
    (guard (c ((i/o-filename-error? c)
               archive))
      (call-with-input-file (->namestring
                             (pathname-with-file directory "available.scm"))
        (lambda (port)
          (loop ((for form (in-port port read)))
            => archive
            (let ((package (parse-package-form form)))
              (archive-add-package! archive package))))))))

(define (prune-archive archive pruner)
  (let* ((bundle-refcounts (archive-bundle-refcounts archive))
         (stale-pathnames
          (collect-list
              (for pathname refcount (in-hashtable bundle-refcounts))
              (if (= refcount 0))
            pathname)))
    (loop ((for pathname (in-list (list-sort pathname<? stale-pathnames))))
      (pruner (merge-pathnames pathname (archive-directory archive)))
      (hashtable-delete! bundle-refcounts pathname))))

(define (write-archive-index archive)
  (let* ((package-table (archive-packages archive))
         (package-names (vector-sort symbol<? (hashtable-keys package-table))))
    (call-with-output-file/atomic
        (pathname-with-file (archive-directory archive) "available.scm")
      (lambda (port)
        (loop ((for package-name (in-vector package-names)))
          (let ((package (hashtable-ref package-table package-name #f)))
            (fmt port (pretty/unshared (package->form package)))))))))


;;; Upload area treatment

(define dry-run? (make-parameter #f))

(define (scan-upload-directory directory)
  (let ((directory (pathname-as-directory directory)))
    (loop continue
        ((for filename (in-directory directory))
         (with uploaded-bundles '()))
      => uploaded-bundles
      (if (string-suffix? ".zip.sig" filename)
          (let ((signature-file (pathname-with-file directory filename))
                (bundle-file (pathname-with-file
                              directory
                              (substring filename
                                         0
                                         (- (string-length filename) 4)))))
            (cond ((not (file-regular? bundle-file))
                   (warn "lone signature file: " (->namestring signature-file))
                   (continue))
                  ((valid-signature-file? signature-file bundle-file)
                   (continue (=> uploaded-bundles (cons bundle-file
                                                        uploaded-bundles))))
                  (else
                   (warn "invalid signature file: "
                         (->namestring signature-file))
                   (continue))))
          (continue)))))

(define (install-uploaded-bundles archive upload-directory)
  (loop ((for bundle-pathname (in-list (scan-upload-directory upload-directory))))
    (let* ((install-pathname (make-pathname #f '() (pathname-file bundle-pathname)))
           (archive-pathname (merge-pathnames install-pathname
                                              (archive-directory archive))))
      (cond ((file-exists? archive-pathname)
             (warn "ignoring uploaded bundle due to filename conflict: "
                   (->namestring bundle-pathname)))
            (else
             (let ((packages (call-with-input-bundle bundle-pathname
                                 (bundle-options no-inventory)
                               bundle-packages)))
               (loop ((for package (in-list packages)))
                 (archive-add-package! archive (package-with-location
                                                package
                                                install-pathname))))
             (message "installing " (->namestring bundle-pathname)
                      " into " (->namestring archive-pathname))
             (unless (dry-run?)
               (rename-file bundle-pathname archive-pathname)
               (rename-file (pathname-add-type bundle-pathname "sig")
                            (pathname-add-type archive-pathname "sig"))))))))

(define %gpgv-path (delay (find-exec-path "gpgv")))

(define archive-keyring (make-parameter #f))

(define (valid-signature-file? signature-file data-file)
  (let ((gpgv-path (or (force %gpgv-path)
                       (fatal "`gpgv' not found in PATH"))))
    (receive (status signal)
             (apply run-process #f (append (list gpgv-path)
                                           (or (and=> (archive-keyring) list)
                                               '())
                                           (list signature-file data-file)))
      (eqv? status 0))))


;;; Utilities

(define symbol<? (<? symbol-compare))

(define (package-location package)
  (and=> (package-property package 'location #f)
         (lambda (location)
           (location->pathname (car location)))))

(define (package-with-location package pathname)
  (package-with-property package
                         'location
                         (list (pathname->location pathname))))

(define (show-error-message . formats)
  (fmt (current-error-port) "update-archive: " (apply-cat formats) "\n"))

(define (message . formats)
  (fmt #t (apply-cat formats) "\n"))

(define (warn . formats)
  (apply show-error-message formats))

(define (fatal . formats)
  (apply show-error-message formats)
  (exit #f))


;;; Entry point

(define (update-archive archive-directory upload-directory)
  (let ((archive (open-archive archive-directory)))
    (install-uploaded-bundles archive upload-directory)
    (prune-archive archive
                   (lambda (pathname)
                     (message "removing stale bundle " (->namestring pathname))
                     (unless (dry-run?)
                       (delete-file pathname))))
    (unless (dry-run?)
      (write-archive-index archive))))

(define (option-handler parameter convert)
  (lambda (option name arg operands)
    (parameter (convert arg))
    operands))

(define command-line-options
  (list (option '("keyring") 'keyring
                (option-handler archive-keyring values))
        (option '("dry-run") #f
                (option-handler dry-run? (lambda (arg) #t)))))

(define (main argv)
  (define (unrecognized-option option name arg operands)
    (fatal "unrecognized command-line option: " name))
  (let ((args (reverse (args-fold (cdr argv)
                                  command-line-options
                                  unrecognized-option
                                  cons
                                  '()))))
    (match args
      ((archive-directory upload-directory)
       (update-archive archive-directory upload-directory))
      (_
       (fatal "two command-line arguments expected")))))

(main (command-line))

;; Local Variables:
;; scheme-indent-styles: (foof-loop as-match)
;; End:
