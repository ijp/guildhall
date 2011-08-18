;;; hooks.scm --- Test suite for the hook mechanism

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
        (sigil spells pathname)
        (sigil spells filesys)
        (sigil ext trc-testing)
        (sigil spells logging)
        (sigil private utils)
        (sigil inventory)
        (sigil package)
        (sigil destination)
        (sigil hooks))

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
                                                     "test-library.scm")))
          (test-datum '(mic check (1 2 3)))
          (test-pathname (pathname-join test-dir ",test.tmp")))
      (test-eqv #t (hook-runner? runner))
      (call-with-port (open-output-file (->namestring test-pathname))
        (lambda (port)
          (write test-datum port)))
      (test-equal '((libraries "test-library.scm"))
        (map inventory->tree
             (run-hook runner
                       package:null
                       `(installation-hook ()
                          (import (rnrs base))
                          (lambda (agent)
                            (agent 'install-file
                                   'libraries
                                   "test-library.scm"
                                   ,(->namestring test-pathname))))
                       (lambda ()
                         (assertion-violation 'hooks-tests
                                              "unexpected unpack call")))))
      (test-eqv #t (file-regular? dest-pathname))
      (test-equal test-datum
        (call-with-port (open-input-file (->namestring dest-pathname))
          read))
      (close-hook-runner runner))))

(define-test-case hooks-tests exception ((setup (setup-stage))
                                         (teardown (clear-stage)))
  (let ((destination (make-fhs-destination 'test test-dir)))
    (setup-destination destination '())
    (let ((runner (spawn-hook-runner destination))
          (test-pathname (make-pathname #f '("foo" "bar") "test.scm"))
          (exception-cookie (list 'cookie)))
      (test-eq exception-cookie
        (guard (c ((hook-runner-exception? c)
                   exception-cookie))
          (run-hook runner
                    package:null
                    `(installation-hook ()
                       (import (rnrs base))
                       (lambda (agent)
                         (error 'hook "Test error")))
                    (lambda ()
                      test-pathname))))
      (close-hook-runner runner))))

(define-test-case hooks-tests unpacked-source-option ((setup (setup-stage))
                                                      (teardown (clear-stage)))
  (let ((destination (make-fhs-destination 'test test-dir)))
    (setup-destination destination '())
    (let ((runner (spawn-hook-runner destination))
          (test-pathname (make-pathname #f '("foo" "bar") "test.scm")))
      (test-equal '()
        (run-hook runner
                  package:null
                  `(installation-hook ()
                     (import (rnrs base))
                     (lambda (agent)
                       (let ((unpacked-source (agent 'unpacked-source)))
                         (or (eqv? #f unpacked-source)
                             (error 'hook
                                    "Unexpected result of `unpacked-source"
                                    unpacked-source)))))
                  (lambda ()
                    test-pathname)))
      (test-equal '()
        (run-hook runner
                  package:null
                  `(installation-hook ((needs-source? . #t))
                     (import (rnrs base))
                     (lambda (agent)
                       (let ((unpacked-source (agent 'unpacked-source)))
                         (or (string=? unpacked-source
                                       ,(->namestring test-pathname))
                             (error 'hook
                                    "Unexpected result of `unpacked-source"
                                    unpacked-source)))))
                  (lambda ()
                    test-pathname)))
      (close-hook-runner runner))))

(when debug-output?
  (set-logger-properties!
   root-logger
   `((threshold trace)
     (handlers ,(lambda (entry)
                  (default-log-formatter entry (current-output-port)))))))

(exit (run-test-suite hooks-tests))

;; Local Variables:
;; scheme-indent-styles: (trc-testing (installation-hook 1))
;; End:
