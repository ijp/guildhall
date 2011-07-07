;;; -*- Mode: Scheme; scheme48-package: (config) -*-

;;;; Parsing Tools
;;;; Interface Definitions

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-interface parser-combinators-interface
  (export
    parse-stream
    (define-parser :syntax)
    (*parser :syntax)
    parser:at-least
    parser:at-least-until
    parser:at-most
    parser:at-most-until
    parser:backtrackable
    parser:between
    parser:between-until
    parser:bracketed
    parser:bracketed*
    parser:bracketed-noise
    parser:bracketed-list
    parser:call-with-context
    parser:choice
    parser:complete
    parser:context
    parser:deep-choice
    parser:delayed
    parser:end
    parser:epsilon
    parser:error
    parser:eqv-token
    parser:exactly
    parser:extend
    parser:label
    parser:list:at-least
    parser:list:at-least-until
    parser:list:at-most
    parser:list:at-most-until
    parser:list:between
    parser:list:between-until
    parser:list:exactly
    parser:list:repeated
    parser:list:repeated-until
    parser:map
    parser:match
    parser:match->list
    parser:match->ignore
    parser:modify-context
    parser:noise:at-least
    parser:noise:at-least-until
    parser:noise:at-most
    parser:noise:at-most-until
    parser:noise:between
    parser:noise:between-until
    parser:noise:exactly
    parser:noise:repeated
    parser:noise:repeated-until
    parser:on-failure
    parser:optional
    parser:optional-noise
    parser:peek
    parser:refuse
    parser:repeated
    parser:repeated-until
    parser:return
    parser:sequence
    parser:set-context
    parser:token
    parser:token*
    parser:token-if
    ))

(define-interface matcher-combinators-interface
  (export
    match
    (define-matcher :syntax)

    ;; Matcher combinators
    matcher:at-least
    matcher:at-least-until
    matcher:at-most
    matcher:at-most-until
    matcher:between
    matcher:between-until
    matcher:bracketed
    matcher:bracketed*
    matcher:choice
    matcher:comparison
    matcher:deep-choice
    matcher:end
    matcher:epsilon
    matcher:error
    matcher:exactly
    matcher:if
    matcher:left-comparison
    matcher:optional
    matcher:peek
    matcher:repeated
    matcher:repeated-until
    matcher:right-comparison
    matcher:sequence
    matcher:token
    matcher:token-if

    ;; Higher-order matcher combinators
    comparator-matcher
    left-comparator-matcher
    right-comparator-matcher
    guarded-matcher
    ))

(define-interface text-parser-combinators-interface
  (export
    parse-file
    parse-input-chars
    parse-string
    parser:bracketed-string
    parser:char
    parser:char=
    parser:char/=
    parser:char-ci=
    parser:char-ci/=
    parser:char-in-set
    parser:char-not-in-set
    parser:list->string
    parser:match->string
    parser:reverse-list->string
    parser:string=
    parser:string-ci=
    parser:string:at-least
    parser:string:at-least-until
    parser:string:at-most
    parser:string:at-most-until
    parser:string:between
    parser:string:between-until
    parser:string:exactly
    parser:string:repeated
    parser:string:repeated-until
    ))

(define-interface text-matcher-combinators-interface
  (export
    match-string
    match-string?
    matcher:char
    matcher:char=
    matcher:char/=
    matcher:char-ci=
    matcher:char-ci/=
    matcher:char-in-set
    matcher:char-not-in-set
    ))

(define-interface parse-errors-interface
  (export
    parse-error?
    parse-error/position
    parse-error/messages
    merge-parse-errors
    parse-error-with-position
    make-parse-error
    make-parse-error:trailing-garbage
    make-parse-error:unknown
    make-parse-error:unexpected-end-of-input
    make-parse-error:unexpected-token
    ))

(define-interface parsing-tests-interface
  (export
    parsing-tests
    test-parsing
    ))

(define-interface lazy-streams-interface
  (export
    (stream-cons :syntax)
    stream-nil
    stream-null?
    stream-pair?
    stream-car
    stream-cdr
    stream->list
    list->stream
    string->stream
    stream-difference
    ))

(define-interface laziness-interface
  (export
    eager
    (lazy :syntax)
    (delay :syntax)
    force
    ))
