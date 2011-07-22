;;; hooks.scm --- Hook runner frontend

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

;; The installation hook mechanism works by launching an helper script
;; inside the target environment (where the package is to be installed
;; to), and sending the code of the hook there. The code is evaluated
;; by the helper script, and passed an "agent" which it can use to
;; communicate with dorodango. This way, the hook's code can rely on
;; the package and all dependencies to do its job.

;;; Code:
#!r6rs

(library (guildhall hooks)
  (export hook-runner?
          spawn-hook-runner
          close-hook-runner
          run-hook
          
          hook-runner-error?
          hook-runner-exception?
          hook-runner-exception-value
          dsp-hook-runner-exception
          
          logger:dorodango.hooks)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) append-reverse)
          (srfi :8 receive)
          (guildhall ext foof-loop)
          (guildhall ext fmt)
          (only (guile) assq-ref)
          (spells pathname)
          (spells filesys)
          (spells process)
          (spells match)
          (spells ports)
          (only (spells misc) unspecific and=>)
          (spells logging)
          (guildhall private utils)
          (guildhall build-info)
          (guildhall inventory)
          (guildhall package)
          (guildhall destination))

(define-record-type hook-runner
  (fields destination process input output))

(define (hook-runner-send runner datum)
  (let ((port (hook-runner-input runner)))
    (log/debug (cat "sending to hook runner: " datum))
    (write datum port)
    (newline port)
    (flush-output-port port)))

(define (hook-runner-receive runner)
  (let ((line (get-line (hook-runner-output runner))))
    (when (eof-object? line)
      (raise-hook-runner-error "received unexpected EOF"))
    (let ((result (read (open-string-input-port line))))
      (log/debug (cat "received from hook runner: " result))
      result)))

(define null-package (make-package 'null '((0))))

(define (spawn-hook-runner destination)
  (let ((r6rs-script *script-interpreter*)
        (hook-runner (find-file (make-pathname #f
                                               '("guildhall" "private")
                                               "hook-runner.sps")
                                (library-search-paths))))
    (let ((process (spawn-process #f #f #f (standard-error-port)
                                  r6rs-script hook-runner)))
      (make-hook-runner destination
                        process
                        (transcoded-port (process-input process)
                                         (make-transcoder (utf-8-codec)))
                        (transcoded-port (process-output process)
                                         (make-transcoder (utf-8-codec)))))))

(define (close-hook-runner runner)
  (close-port (hook-runner-input runner))
  (close-port (hook-runner-output runner))
  (receive (status signal)
           (wait-for-process (hook-runner-process runner))
    (unless (eqv? status 0)
      (raise-hook-runner-error
       "hook runner process returned unexpected exit status" status))))

;;@ Run @var{hook-form} as hook for the package @var{package}. Returns
;; an alist of inventories, which list files installed by the hook.
(define (run-hook runner package hook-form unpack-source)
  (receive (kind options libraries hook-proc-expr)
           (parse-hook-form hook-form)
    (let ((source-pathname (if (assq-ref options 'needs-source?)
                               (unpack-source)
                               #f)))
      (define (cleanup)
        (when source-pathname
          (rm-rf source-pathname)))
      (hook-runner-send runner
                        `(run-hook ,kind ,options ,libraries ,hook-proc-expr))
      (loop continue ((with message
                            (hook-runner-receive runner)
                            (hook-runner-receive runner))
                      (with inventories '()))
        (match message
          (('install-file (? symbol? category)
                          (? string? dest-filename)
                          (? string? src-filename))
           (destination-install (hook-runner-destination runner)
                                package
                                category
                                dest-filename
                                (make-file-extractor src-filename))
           (hook-runner-send runner #t)
           (continue
            (=> inventories (update-inventories inventories category dest-filename))))
          (('package-name)
           (hook-runner-send runner (package-name package))
           (continue))
          (('unpacked-source)
           (hook-runner-send runner (and=> source-pathname ->namestring))
           (continue))
          (('hook-done)
           (cleanup)
           inventories)
          (('exception exception)
           (cleanup)
           (raise-hook-runner-exception exception))
          (_
           (cleanup)
           (raise-hook-runner-error "invalid message from hook runner"
                                    message)))))))

(define (make-file-extractor filename)
  (lambda (dest-port)
    (call-with-port (open-file-input-port filename)
      (lambda (src-port)
        (copy-port src-port dest-port)))))

(define (update-inventories inventories category pathname)
  (let ((pathname (->pathname pathname)))
    (define (updated-inventory inventory)
      (let ((path (pathname-directory (pathname-as-directory pathname))))
        (inventory-leave-n (inventory-update inventory path #f #t)
                           (length path))))
    (define (finish processed inventory rest)
      (append-reverse processed (cons (updated-inventory inventory) rest)))
    (loop continue ((for entry entry-rest (in-list inventories))
                    (with processed '()))
      => (finish processed (make-inventory category 'category) '())
      (cond ((eq? category (inventory-name entry))
             (finish processed entry (cdr entry-rest)))
            (else
             (continue (=> processed (cons entry processed))))))))

(define (parse-hook-form form)
  (match form
    (('installation-hook (options ___)
       ('import libraries ___)
       hook-proc-expr)
     (values 'installation options libraries hook-proc-expr))
    (_
     (raise-hook-runner-error "malformed hook form" form))))

(define-condition-type &hook-runner-error &error
  make-hook-runner-error hook-runner-error?)

(define-condition-type &hook-runner-exception &error
  make-hook-runner-exception hook-runner-exception?
  (value hook-runner-exception-value))

(define (raise-hook-runner-error message . irritants)
  (raise (condition (make-hook-runner-error)
                    (make-message-condition message)
                    (make-irritants-condition irritants))))

(define (raise-hook-runner-exception exception)
  (raise (condition (make-hook-runner-exception exception))))

(define (dsp-hook-runner-exception c)
  (define (dsp-hook-exception-part part)
    (match part
      ((name fields ___)
       (cat name
            (case (length fields)
              ((0) nl)
              ((1) (cat ": " (wrt/unshared (cdar fields)) nl))
              (else
               (cat ":\n"
                    (fmt-join/suffix (lambda (field)
                                       (cat "    " (car field) ": " (cdr field)))
                                     fields
                                     nl))))))))
  (cat "Exception in installation hook:\n"
       (fmt-indented "  " (fmt-join/suffix dsp-hook-exception-part
                                           (hook-runner-exception-value c)))))


(define logger:dorodango.hooks
  (make-logger logger:dorodango 'hooks))
(define log/debug (make-fmt-log logger:dorodango.hooks 'debug))

)

;; Local Variables:
;; scheme-indent-styles: (as-match foof-loop (installation-hook 1))
;; End:
