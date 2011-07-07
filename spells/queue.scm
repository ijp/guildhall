;;; queue.sls --- Simple, imperative queue.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

;;@ Imperative queues.
(library (spells queue)
  (export queue?
          make-empty-queue
          queue-empty?
          enqueue!
          dequeue!)

  (import (rnrs base)
          (rnrs control)
          (rnrs mutable-pairs)
          (rnrs records syntactic))
  
  (define-record-type queue
    (fields (mutable front)
            (mutable back)))

  (define (make-empty-queue)
    (make-queue '() '()))
  
  ;;@ Return @code{#t} if the queue @1 is empty, @code{#f} otherwise.
  (define (queue-empty? q)
    (null? (queue-front q)))
  
  ;;@ Insert the element @2 into the queue @1.
  (define (enqueue! q x)
    (let ((pr (cons x '()))
          (back (queue-back q)))
      (unless (null? back)
        (set-cdr! back pr))
      (queue-back-set! q pr)
      (when (null? (queue-front q))
        (queue-front-set! q pr))))

  ;;@ Dequeue an element from @1.
  (define (dequeue! q)
    (let ((front (queue-front q)))
      (when (null? front)
        (error 'dequeue! "queue is empty"))
      (queue-front-set! q (cdr front))
      (when (null? (queue-front q))
        (queue-back-set! q '()))
      (car front)))

)
