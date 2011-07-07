;;; args-fold.sls --- Slightly extended variant of SRFI 37

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (c) 2002 Anthony Carrico

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This is a slight extension of SRFI-37, which is intended to be
;; backwards compatible, and offers the following additional features:
;;
;; - Support for additional `description' field in the option record,
;;   to faciliate providing an `--help' option.
;;
;; - `option' accepts a symbol as what is the `required-arg?' argument
;;   in SRFI-37, for the same reason; this value is available via
;;   `option-argument'. This is allowed also to faciliate `--help'.
;;
;; - An option may terminate command-line processing
;;   (`option-terminator?').
;;
;; - An additional procedure `args-fold*' is provided, which has one
;;   additional argument `stop-early?'. When this argument is true,
;;   scanning for arguments stops after the first non-option
;;   argument. When `stop-early' is #f, `args-fold*' behaves the same
;;   as `args-fold'.
;;
;; - The argument list may carry non-strings, which are considered as
;;   operands (or option arguments, if preceded by an option requiring
;;   an argument).  This is allowed to increase flexibility for
;;   applications that want to do pre-processing on the actual
;;   argument list.
;; 

;;; Code:
#!r6rs

(library (spells args-fold)
  (export option
          args-fold
          option-names
          option-required-arg?
          option-optional-arg?
          option-processor

          ;; extensions to SRFI-37
          args-fold*
          option-argument
          option-terminator?
          option-description)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (srfi :9 records))

(define-record-type :option
  (make-option names argument optional-arg? terminator? description processor)
  option?
  (names option-names)
  (argument option-argument)
  (optional-arg? option-optional-arg?)
  (terminator? option-terminator?)
  (description option-description)
  (processor option-processor))

(define option
  (case-lambda
    ((names argument processor)
     (make-option names argument #f #f #f processor))
    ((names argument optional? processor) ; SRFI-37 backwards-compatible case
     (make-option names argument optional? #f #f processor))
    ((names argument optional? terminator? processor)
     (make-option names argument optional? terminator? #f processor))
    ((names argument optional? terminator? description processor)
     (make-option names
                  argument
                  optional?
                  terminator?
                  description
                  processor))))

(define (option-required-arg? option)
  (let ((arg (option-argument option)))
    (if (symbol? arg)
        (not (option-optional-arg? option))
        arg)))

(define (args-fold args options unrecognized-option-proc operand-proc . seeds)
  (apply args-fold*
         args
         options
         #f
         unrecognized-option-proc
         operand-proc
         seeds))

(define (args-fold* args
                    options
                    stop-early?
                    unrecognized-option-proc
                    operand-proc
                    . seeds)
  (define (find-option name)
    (find (lambda (option)
            (find (lambda (test-name)
                    (equal? name test-name))
                  (option-names option)))
          options))
  (define (scan-short-options index shorts args seeds)
    (if (= index (string-length shorts))
        (scan-args args seeds)
        (let* ((name (string-ref shorts index))
               (option (or (find-option name)
                           (option (list name)
                                   #f
                                   #f
                                   unrecognized-option-proc))))
          (cond ((and (< (+ index 1) (string-length shorts))
                      (or (option-required-arg? option)
                          (option-optional-arg? option)))
                 (process-arg+iterate option
                                      name
                                      (substring shorts (+ index 1) (string-length shorts))
                                      args
                                      seeds))
                ((and (option-required-arg? option)
                      (pair? args))
                 (process-arg+iterate option name (car args) (cdr args) seeds))
                (else
                 (let-values ((seeds (apply (option-processor option)
                                            option
                                            name
                                            #f
                                            seeds)))
                   (if (option-terminator? option)
                       (scan-operands args seeds)
                       (scan-short-options (+ index 1)
                                           shorts
                                           args
                                           seeds))))))))
  (define (scan-operands operands seeds)
    (if (null? operands)
        (apply values seeds)
        (let-values ((seeds (apply operand-proc
                                   (car operands)
                                   seeds)))
          (scan-operands (cdr operands) seeds))))
  (define (process-arg+iterate option name arg args seeds)
    (let-values ((seeds (apply (option-processor option)
                               option
                               name
                               arg
                               seeds)))
      (if (option-terminator? option)
          (scan-operands args seeds)
          (scan-args args seeds))))
  (define (scan-args args seeds)
    (if (null? args)
        (apply values seeds)
        (let ((arg (car args))
              (args (cdr args)))
          ;; NOTE: This string matching code would be simpler
          ;; using a regular expression matcher.
          (cond
           ( ;; (rx bos "--" eos)
            (and (string? arg) (string=? "--" arg))
            ;; End option scanning:
            (scan-operands args seeds))
           ((looking-at-long-option-with-arg arg)
            => (lambda (=-index)
                 (let* ((name (substring arg 2 =-index))
                        (option-arg (substring arg
                                               (+ =-index 1)
                                               (string-length arg)))
                        (option (or (find-option name)
                                    (option (list name)
                                            #t
                                            #f
                                            unrecognized-option-proc))))
                   (process-arg+iterate option name option-arg args seeds))))
           ( ;;(rx bos "--" (submatch (+ any)))
            (and (string? arg)
                 (> (string-length arg) 3)
                 (char=? #\- (string-ref arg 0))
                 (char=? #\- (string-ref arg 1)))
            ;; Found long option:
            (let* ((name (substring arg 2 (string-length arg)))
                   (option (or (find-option name)
                               (option (list name)
                                       #f
                                       #f
                                       unrecognized-option-proc))))
              (if (and (option-required-arg? option)
                       (pair? args))
                  (process-arg+iterate option name (car args) (cdr args) seeds)
                  (process-arg+iterate option name #f args seeds))))
           ( ;; (rx bos "-" (submatch (+ any)))
            (and (string? arg)
                 (> (string-length arg) 1)
                 (char=? #\- (string-ref arg 0)))
            ;; Found short options
            (let ((shorts (substring arg 1 (string-length arg))))
              (scan-short-options 0 shorts args seeds)))
           (else
            (let-values ((seeds (apply operand-proc arg seeds)))
              (if stop-early?
                  (scan-operands args seeds)
                  (scan-args args seeds))))))))
  (scan-args args seeds))

(define (looking-at-long-option-with-arg arg)
  ;;(rx bos
  ;;    "--"
  ;;    (submatch (+ (~ "=")))
  ;;    "="
  ;;    (submatch (* any)))
  (and (string? arg)
       (> (string-length arg) 4)
       (char=? #\- (string-ref arg 0))
       (char=? #\- (string-ref arg 1))
       (not (char=? #\= (string-ref arg 2)))
       (let loop ((index 3))
         (cond ((= index (string-length arg))
                #f)
               ((char=? #\= (string-ref arg index))
                index)
               (else
                (loop (+ 1 index)))))))
)
