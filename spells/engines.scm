;;; engines.sls --- Nestable "engines"

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This is a variation of the nestable engines from the paper "Engines
;; from Continuations", by R. Kent Dybvig and Robert Hieb, 1988.

;;; Code:
#!r6rs
(library (spells engines)
  (export make-engine)
  (import (rnrs base)
          (rnrs control)
          (spells engines timer))

(define make-engine
  (let ((stack '()))
    
    (define (new-engine proc id)
      (lambda (ticks return expire)
        ((call/cc
           (lambda (k)
             (run proc
                  (stop-timer)
                  ticks
                  (lambda (value ticks engine-maker)
                    (k (lambda () (return value ticks engine-maker))))
                  (lambda (engine)
                    (k (lambda () (expire engine))))
                  id))))))
    
    (define (run resume parent child return expire id)
      (let ((ticks (if (and (active?) (< parent child)) parent child)))
        (push (- parent ticks) (- child ticks) return expire id)
        (resume ticks)))
    
    (define (go ticks)
     (when (active?)
       (if (= ticks 0)
           (timer-handler)
           (start-timer ticks timer-handler))))
    
    (define (do-return proc value ticks id1)
      (pop
       (lambda (parent child return expire id2)
         (cond ((eq? id1 id2)
                (go (+ parent ticks))
                (return value
                        (+ child ticks)
                        (lambda (value)
                          (new-engine (proc value) id1))))
               (else
                (do-return
                 (lambda (value)
                   (lambda (new-ticks)
                     (run (proc value) new-ticks (+ child ticks) return expire id2)))
                 value
                 (+ parent ticks)
                 id1))))))
    
    (define (do-expire resume)
      (pop (lambda (parent child return expire id)
             (cond ((> child 0)
                    (do-expire (lambda (ticks)
                                 (run resume ticks child return expire id))))
                   (else
                    (go parent)
                    (expire (new-engine resume id)))))))
    
    (define (timer-handler)
      (go (call/cc do-expire)))
    
    (define (push . l)
      (set! stack (cons l stack)))
    (define (pop handler)
      (if (null? stack)
          (error 'engine "attempt to return from inactive engine")
          (let ((top (car stack)))
            (set! stack (cdr stack))
            (apply handler top))))
    (define (active?)
      (not (null? stack)))
    
    (lambda (proc)
      (letrec ((engine-return
                (lambda (value)
                  (call/cc
                    (lambda (k)
                      (do-return (lambda (value)
                                   (lambda (ticks)
                                     (go ticks)
                                     (k value)))
                                 value
                                 (stop-timer)
                                 engine-return))))))
        (new-engine (lambda (ticks)
                      (go ticks)
                      (proc engine-return)
                      (error 'engine "invalid completion"))
                    engine-return)))))

)
