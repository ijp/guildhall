;;; -*- Mode: Scheme; scheme48-package: text-matcher-combinators -*-

;;;; Parsing Tools
;;;; Combinators for Matching Text

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (match-string matcher string)
  (match matcher (string->stream string)))

(define (match-string? matcher string)
  (and (match-string matcher string) #t))

(define (matcher:char)
  (matcher:token-if char?))

(define (matcher:char-test procedure argument)
  (matcher:token-if (lambda (token)
                      (and (char? token)
                           (procedure argument token)))))

(define (char/=? a b) (not (char=? a b)))
(define (char-ci/=? a b) (not (char-ci=? a b)))

(define (matcher:char= char) (matcher:char-test char=? char))
(define (matcher:char/= char) (matcher:char-test char/=? char))
(define (matcher:char-ci= char) (matcher:char-test char-ci=? char))
(define (matcher:char-ci/= char) (matcher:char-test char-ci/=? char))

(define (matcher:char-in-set char-set)
  (matcher:char-test char-set-contains? char-set))

(define (matcher:char-not-in-set char-set)
  (matcher:char-test (lambda (char-set char)
                       (not (char-set-contains? char-set char)))
                     char-set))
