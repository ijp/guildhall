;;; ui.sls --- dorodango UI operations

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (dorodango ui)
  (export current-ui
          message
          y-or-n
          prompt
          
          ui/message
          ui/y-or-n
          ui/prompt)
  (import (rnrs)
          (srfi :39 parameters)
          (spells operations))

(define-operation (ui/message ui . formats))
(define-operation (ui/y-or-n ui default message))
(define-operation (ui/prompt ui message choices))

(define current-ui (make-parameter #f))

(define (make-ui-wrapper operation)
  (lambda args
    (apply operation (current-ui) args)))

(define message (make-ui-wrapper ui/message))
(define prompt  (make-ui-wrapper ui/prompt))
(define y-or-n  (make-ui-wrapper ui/y-or-n))

)
