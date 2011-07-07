;;; -*- Mode: Scheme; scheme48-package: lazy-streams -*-

;;;; Lazy Streams

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-syntax stream-cons
  (syntax-rules ()
    ((STREAM-CONS a d)
     (DELAY (CONS a d)))))

(define stream-nil (delay '()))

(define-syntax define-stream-unop
  (syntax-rules ()
    ((DEFINE-STREAM-UNOP stream-op op)
     (DEFINE (stream-op STREAM) (op (FORCE STREAM))))))

(define-stream-unop stream-null?        null?)
(define-stream-unop stream-pair?        pair?)
(define-stream-unop stream-car          car)
(define-stream-unop stream-cdr          cdr)

(define (stream->list stream)
  (let ((datum (force stream)))
    (if (pair? datum)
        (cons (car datum)
              (stream->list (cdr datum)))
        datum)))

(define (list->stream list)
  (lazy (if (pair? list)
            (stream-cons (car list)
                         (list->stream (cdr list)))
            (eager list))))

(define (string->stream string)
  (let recur ((index 0))
    (lazy (if (= index (string-length string))
              stream-nil
              (stream-cons (string-ref string index)
                           (recur (+ index 1)))))))

;** Be careful!  This operation is potentially dangerous.

(define (stream-difference earlier later)
  (lazy (if (eq? earlier later)
            stream-nil
            (stream-cons (stream-car earlier)
                         (stream-difference (stream-cdr earlier)
                                            later)))))
