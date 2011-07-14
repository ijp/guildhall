;;; run.scm --- Utilities for running testcases

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Utilities for running testcases.
(library (spells test-runner)
  (export run-tests
          run-tests-in-directory
          run-tests-in-file
          main)
  (import (except (rnrs base) string-copy string->list string-for-each)
          (rnrs eval)
          (rnrs control)
          (rnrs io simple)
          (rnrs lists)
          (rnrs exceptions)
          (rnrs programs)
          (only (srfi :1 lists) span)
          (srfi :8 receive)
          (spells misc)
          (srfi :39 parameters)
          (spells match)
          (spells filesys)
          (spells pathname)
          (spells condition)
          (only (wak trc-testing)
                test-verbosity with-test-verbosity
                test-debug-errors? with-test-debug-errors?)
          (spells test-runner environment))

  ;;@ Run specified tests.
  ;;
  ;; @1 must be a list whose elements can be either directories or
  ;; single file names: @code{run-tests-in-directory} or
  ;; @code{run-tests-in-file} is applied accordingly. If empty is
  ;; provided, the current directory is used.
  (define (run-tests tests env)
    (for-each (lambda (f)
                (cond ((file-directory? f) (run-tests-in-directory f env))
                      ((file-regular? f) (run-tests-in-file f env))
                      (else
                       (display (list "skipping non-existing" f))
                       (newline))))
              tests))

  ;;@ Call @code{run-tests-in-file} for each file in the given
  ;; directory. If the directory contains a file named @file{tests.scm},
  ;; the list of files is read from it.
  (define (run-tests-in-directory dir env)
    (let* ((dir (pathname-as-directory dir))
           (listing (pathname-with-file dir (make-file "tests" "scm"))))
      (cond ((file-regular? listing)
             (eval-test-spec dir (with-input-from-file (->namestring listing)
                                   read)))
            (else
             (for-each (lambda (f)
                         (run-tests-in-file f env))
                       (directory-fold dir cons '()))))))

  (define (run-tests-in-file file env)
    (let ((filename (->namestring file)))
      (define (evaluate-forms forms)
        (cond (env
               (eval `(let () ,@forms) env))
              (else
               (match forms
                 ((('import . imports) . body)
                  (and=> (construct-test-environment #f imports)
                         (lambda (env)
                           (parameterize ((test-environment env))
                             (eval `(let () ,@body) env)))))
                 (_
                  (error 'run-tests-in-file
                         "Invalid test file, expected leading import form"
                         filename (car forms)))))))
      (display "\n;")
      (display (list "Loading " filename "... "))
      (newline)
      (let ((forms (call-with-input-file filename
                     (lambda (port)
                       (let loop ((forms '()))
                         (let ((form (read port)))
                           (if (eof-object? form)
                               (reverse forms)
                               (loop (cons form forms)))))))))
        (receive results (evaluate-forms forms)
          (display ";")
          (display (list "..." filename "done"))
          (newline)
          (apply values results)))))

  (define (construct-test-environment defaults? imports)
    (guard (c (#t (display ";#(Error constructing environment: ")
                  (newline)
                  (display-condition c)
                  (display ")")
                  (newline)
                  #f))
      (apply environment
             (append
              (if defaults?
                  '((except (rnrs base)
                            error string-copy string-for-each string->list)
                    (rnrs io simple)
                    (wak trc-testing)
                    (spells test-runner environment))
                  '())
              imports))))

  ;; test spec grammar:
  ;;
  ;; <test spec> -> (<clause> ...)
  ;; <clause> -> (files <file spec> ...)
  ;; <file spec> -> <filename>
  ;;                (<filename> [<options+imports>])
  ;;                ((code <scheme expr> ...) <filename> [<options+imports>])
  ;; <options+imports> -> ['<option> ...] <library-name> ...
  (define (eval-test-spec pathname test-spec)
    (for-each
     (lambda (spec)
       (case (car spec)
         ((files)
          (for-each
           (lambda (clause)
             (eval-test-clause clause pathname (test-verbosity) (test-debug-errors?)))
           (cdr spec)))))
     test-spec))

  (define (parse-options options+rest)
    (receive (options rest)
             (span (match-lambda
                    (('quote option) #t)
                    (_               #f))
                   options+rest)
      (values (map cadr options)
              (if (null? rest) #f rest))))

  (define (with-test-options verbosity debug-errors? thunk)
    (with-test-verbosity verbosity
      (lambda ()
        (with-test-debug-errors? debug-errors?
          thunk))))
  
  (define (eval-test-clause clause directory verbosity debug-errors?)
    (define (run-test code filename options+imports)
      (receive (options imports) (parse-options options+imports)
        (let ((default-imports? (not (memq 'no-default-imports options)))
              (debug-errors? (or (and (memq 'debug-errors options) #t)
                                 debug-errors?))
              (test-pathnames (list (pathname-with-file directory filename))))
          (with-test-options verbosity debug-errors?
            (lambda ()
              (cond (imports
                     (and=> (construct-test-environment default-imports? imports)
                            (lambda (env)
                              (parameterize ((test-environment env))
                                (when code
                                  (eval `(let () ,@code) env))
                                (run-tests test-pathnames env)))))
                    (code
                     (error 'eval-test-clause
                            "Invalid clause; specifies code, but no imports given"
                            clause))
                    (else
                     (run-tests test-pathnames #f))))))))
    (match clause
      ((('code . code) filename . options+imports)
       (run-test code filename options+imports))
      ((filename . options+imports)
       (run-test #f filename options+imports))
      (filename
       (run-test #f filename '()))))
  
  (define (main args)
    (define (process-tests-files files)
      (for-each
       (lambda (tests-file)
         (call-with-input-file tests-file
           (lambda (port)
             (let* ((test-spec (read port))
                  (pathname (->pathname tests-file))
                  (directory (pathname-with-file pathname #f)))
               (parameterize ((this-directory (->namestring directory)))
                 (eval-test-spec directory test-spec))))))
       files))
    (define (parse-options args run)
      (match args
        (("--debug" . rest)
         (parse-options rest (lambda (args)
                               (with-test-debug-errors? #t
                                 (lambda () (run args))))))
        (("--verbosity" verbosity . rest)
         (parse-options rest (lambda (args)
                               (with-test-verbosity (string->symbol verbosity)
                                 (lambda () (run args))))))
        (_
         (run args))))
    (with-test-options 'quiet #f
      (lambda ()
        (parse-options (cdr args) (lambda (files) (process-tests-files files)))))))

;; Local Variables:
;; scheme-indent-styles: ((match 1) (parameterize 1))
;; End:
