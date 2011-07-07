;;; -*- Mode: Scheme; scheme48-package: laziness -*-

;;;; Laziness Primitives
;;;; (Substitutes for R5RS's DELAY & FORCE)

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (eager value)
  (make-promise #t value))

(define-syntax lazy
  (syntax-rules ()
    ((LAZY expression)
     (MAKE-PROMISE #F (LAMBDA () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((DELAY expression)
     (LAZY (EAGER expression)))))

(define (force promise)
  (if (promise-forced? promise)
      (promise-value promise)
      (let ((promise* ((promise-thunk promise))))
        (if (not (promise-forced? promise))
            (clobber-promise! promise promise*))
        (force promise))))

(define (clobber-promise! promise promise*)
  (let ((pair (promise-pair promise))
        (pair* (promise-pair promise*)))
    (set-car! pair (car pair*))
    (set-cdr! pair (cdr pair*))
    (set-promise-pair! promise* pair)))

(define-record-type <promise>
    (%make-promise pair)
    promise?
  (pair promise-pair set-promise-pair!))

(define (make-promise forced? value-then-thunk)
  (%make-promise (cons forced? value-then-thunk)))

(define (promise-forced? promise) (car (promise-pair promise)))
(define (promise-thunk promise) (cdr (promise-pair promise)))
(define (promise-value promise) (cdr (promise-pair promise)))
