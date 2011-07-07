;;; compat.ikarus.sls --- GC-interacting procedures for PLT Scheme

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:
#!r6rs

(library (spells gc compat)
  (export make-weak-cell weak-cell-ref weak-cell?
          make-reaper
          collect)
  (import (rnrs base)
          (rnrs control)
          (rename (only (scheme)
                        make-weak-box
                        weak-box-value
                        weak-box?
                                
                        collect-garbage

                        make-will-executor
                        will-register
                        will-try-execute)
                  
                  (make-weak-box make-weak-cell)
                  (weak-box-value weak-cell-ref)
                  (weak-box? weak-cell?)
                                
                  (collect-garbage collect)))

  (define (make-reaper proc)
    (let ((executor (make-will-executor)))
      (case-lambda
        ((object)
         (will-register executor object proc))
        (()
         (will-try-execute executor)))))

)
