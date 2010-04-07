;;; hooks.scm --- Test suite for the hook mechanism

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
#!r6rs

(import (except (rnrs) delete-file file-exists?)
        (spells pathname)
        (spells filesys)
        (spells testing)
        (spells logging)
        (dorodango private utils)
        (dorodango inventory)
        (dorodango package)
        (dorodango destination)
        (dorodango hooks))

(define debug-output? #f)

(define test-dir (pathname-join (working-directory) '((",hooks-test.tmp"))))

(define (setup-stage)
  (when (file-exists? test-dir)
    (test-failure "working stage not clear" test-dir))
  (create-directory* test-dir))

(define (clear-stage)
  (rm-rf test-dir))

(define-test-suite hooks-tests
  "Hook mechanism")

(define package:null (make-package 'null '((0))))

(define-test-case hooks-tests simple ((setup (setup-stage))
                                      (teardown (clear-stage)))
  (let ((destination (make-fhs-destination 'test test-dir)))
    (setup-destination destination '())
    (let ((runner (spawn-hook-runner destination))
          (dest-pathname (car (destination-pathnames destination
                                                     package:null
                                                     'libraries
                                                     "test-library.sls")))
          (test-datum '(mic check (1 2 3)))
          (test-pathname (pathname-join test-dir ",test.tmp")))
      (test-eqv #t (hook-runner? runner))
      (call-with-port (open-output-file (->namestring test-pathname))
        (lambda (port)
          (write test-datum port)))
      (test-equal '((libraries . (libraries "test-library.sls")))
       (map (lambda (entry)
              (cons (car entry) (inventory->tree (cdr entry))))
            (run-hook runner
                      package:null
                      `(installation-hook ()
                         (import (rnrs base))
                         (lambda (agent)
                           (agent 'install-file
                                  'libraries
                                  "test-library.sls"
                                  ,(->namestring test-pathname)))))))
      (test-eqv #t (file-regular? dest-pathname))
      (test-equal test-datum
        (call-with-port (open-input-file (->namestring dest-pathname))
          read))
      (close-hook-runner runner))))

(when debug-output?
  (set-logger-properties!
   root-logger
   `((threshold trace)
     (handlers ,(lambda (entry)
                  (default-log-formatter entry (current-output-port)))))))

(run-test-suite hooks-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing (installation-hook 1))
;; End:
