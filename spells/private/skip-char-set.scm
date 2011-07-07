;;; skip-char-set.scm --- Skip characters from a set in a port.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(define (skip-char-set skip-chars . maybe-port)
  (let* ((port (:optional maybe-port (current-input-port)))
         (cset (->char-set skip-chars)))

    (if (not (input-port? port))
      (error "Illegal value -- not an input port." port))
    
    ;; Mighty slow -- we read each char twice (peek first, then read).
    (let lp ((i 0))
      (let ((c (lookahead-char port)))
        (cond ((and (char? c) (char-set-contains? cset c))
               (get-char port)
               (lp (+ i 1)))
              (else i))))))
