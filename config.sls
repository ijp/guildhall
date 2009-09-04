;;; config.sls --- 

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

(library (dorodango config)
  (export config?
          make-config
          read-config
          
          config-default-destination
          config-destination)
  (import (rnrs)
          (spells alist)
          (spells match)
          (spells foof-loop)
          (dorodango destination))

(define-record-type (config %make-config config?)
  (fields default-destination
          destinations))

(define (make-config default destinations)
  (%make-config/validate 'make-config default destinations))

(define (%make-config/validate who default destinations)
  (if default
      (cond ((find-destination destinations default)
             => (lambda (default-destination)
                  (%make-config default-destination destinations)))
            (else
             (error who "default configuration undefined" default)))
      (if (null? destinations)
          (error who "no destinations defined")
          (make-config (car destinations)
                       destinations))))

(define (find-destination destinations name)
  (find (lambda (destination)
          (eq? (destination-name destination) name))
        destinations))

(define (config-destination config name)
  (find-destination (config-destinations config) name))

(define (read-config port)
  (define who 'read-config)
  (loop continue ((for form (in-port port read))
                  (with default #f)
                  (with destinations '()))
    => (%make-config/validate who default destinations)
    (match form
      (('default-destination (? symbol? name))
       (continue (=> default name)))
      (('destination ((? symbol? name))
                     dest-spec)
       (continue (=> destinations
                     (cons (cons name (dest-spec->destination who dest-spec))
                           destinations))))
      (else
       (error who "invalid configuration form" form)))))

(define destination-kinds
  `((fhs . ,make-fhs-destination)))

(define (dest-spec->destination who spec)
  (match spec
    (((? symbol? kind) . construct-args)
     (cond ((assq-ref kind destination-kinds)
            => (lambda (constructor)
                 (apply constructor construct-args)))
           (else
            (error who "unkown destination kind" kind))))
    (else
     (error who "invalid destination specification" spec))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
