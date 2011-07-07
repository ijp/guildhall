;;; timer.sls --- Engine timer interface

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells engines timer)
  (export start-timer stop-timer decrement-timer)
  (import (rnrs base)
          (rnrs control)
          (srfi :39 parameters))

(define current-clock (make-parameter 0))
(define current-handler (make-parameter #f))

(define (start-timer ticks new-handler)
  (current-handler new-handler)
  (current-clock ticks))

(define (stop-timer)
  (let ((time-left (current-clock)))
    (current-clock 0)
    time-left))

(define (decrement-timer)
  (let ((new-clock (- (current-clock) 1)))
    (when (>= new-clock 0)
      (current-clock new-clock)
      (when (= new-clock 0)
        ((current-handler))))))

)
