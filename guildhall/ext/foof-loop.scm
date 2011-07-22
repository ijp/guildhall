;;; foof-loop.scm --- 

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (guildhall ext foof-loop)
  (export
    loop
    lazy-loop

    =>
    for
    with
    until
    let
    let-values
    while

    listing
    listing-reverse
    appending
    appending-reverse
    listing!
    listing-into!
    summing
    multiplying
    maximizing
    minimizing

    initial

    up-from
    down-from

    to
    by

    in-list
    in-lists
    in-vector
    in-vector-reverse
    in-string
    in-string-reverse
    in-port
    in-file
    in-stream
    )
  (import
    (rnrs)
    (only (rnrs mutable-pairs) set-cdr!)
    (only (guile) include-from-path)
    (srfi :8 receive)
    (srfi :45 lazy)
    (ice-9 streams)
    (guildhall ext private include))

  (define-syntax define-aux
    (syntax-rules ()
      ((_ id ...)
	(begin
	  (define-syntax id
	    (lambda (x)
	      (syntax-violation #f "invalid use of auxiliary keyword" x 'id)))
	  ...))))

  (define-aux
    for
    with
    to
    by
    until
    while
    initial
    )

  (include-from-path "guildhall/ext/foof-loop/private/syn-param")
  (include-from-path "guildhall/ext/foof-loop/private/foof-loop")

  (define-syntax in-stream
    (syntax-rules ()
      ((_ (elt-var stream-var) (stream-expr) cont . env)
       (cont
        ()                                    ;Outer bindings
        ((stream-var stream-expr              ;Loop variables
                     (stream-cdr stream-var)))
        ()                                    ;Entry bindings
        ((stream-null? stream-var))           ;Termination conditions
        (((elt-var) (stream-car stream-var))) ;Body bindings
        ()                                    ;Final bindings
        . env))
      ;; Optional stream variable is optional
      ((_ (elt-var) (stream-expr) cont . env)
       (in-stream (elt-var stream) (stream-expr) cont . env))))
)
