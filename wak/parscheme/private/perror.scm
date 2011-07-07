;;; -*- Mode: Scheme; scheme48-package: parse-errors -*-

;;;; Parse Error Abstraction

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <parse-error>
    (make-parse-error position messages)
    parse-error?
  (position parse-error/position)
  (messages parse-error/messages))

(define (merge-parse-errors a b)
  (make-parse-error (parse-error/position a)
                    (append (parse-error/messages a)
                            (parse-error/messages b))))

(define (parse-error-with-position perror position)
  (make-parse-error position
                    (parse-error/messages perror)))

(define (make-parse-error:unknown position)
  (make-parse-error position '()))

(define (make-parse-error:unexpected-token token position)
  (make-parse-error position (list `("Unexpected token:" ,token))))

(define (make-parse-error:unexpected-end-of-input position)
  (make-parse-error position '("Unexpected end of input")))

(define (make-parse-error:trailing-garbage position)
  (make-parse-error position '("Trailing garbage")))
