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
          
          config-destination
          config-database-location
          config-default-destination
          config-default-database-location)
  (import (rnrs)
          (spells alist)
          (spells match)
          (spells foof-loop)
          (dorodango destination))

(define-record-type (config %make-config config?)
  (fields default-destination
          destinations
          database-locations))

(define (make-config default destinations database-locations)
  (%make-config/validate 'make-config default destinations database-locations))

(define (%make-config/validate who default destinations database-locations)
  (loop ((for destination (in-list destinations)))
    (let ((name (destination-name destination)))
      (unless (assq-ref database-locations name)
        (lose who "no database location for destination" name))))
  (if default
      (cond ((find-destination destinations default)
             => (lambda (default-destination)
                  (%make-config default-destination
                                destinations
                                database-locations)))
            (else
             (lose who "default configuration undefined" default)))
      (if (null? destinations)
          (lose who "no destinations defined")
          (make-config (destination-name (car destinations))
                       destinations
                       database-locations))))

(define (find-destination destinations name)
  (find (lambda (destination)
          (eq? (destination-name destination) name))
        destinations))

(define (config-destination config name)
  (find-destination (config-destinations config) name))

(define (config-database-location config name)
  (assq-ref (config-database-locations config) name))

(define (config-default-database-location config)
  (config-database-location
   config
   (destination-name (config-default-destination config))))

(define (read-config port)
  (define who 'read-config)
  (loop continue ((for form (in-port port read))
                  (with default #f)
                  (with destinations '())
                  (with database-locations '()))
    => (%make-config/validate who default destinations database-locations)
    (match form
      (('default-destination (? symbol? name))
       (continue (=> default name)))
      (('destination (? symbol? name)
                     dest-spec
                     ('database location))
       (continue (=> destinations
                     (cons (dest-spec->destination who name dest-spec)
                           destinations))
                 (=> database-locations
                     (cons (cons name location) database-locations))))
      (else
       (lose who "invalid configuration form" form)))))

(define destination-kinds
  `((fhs . ,make-fhs-destination)))

(define (dest-spec->destination who name spec)
  (match spec
    (((? symbol? kind) . construct-args)
     (cond ((assq-ref destination-kinds kind)
            => (lambda (constructor)
                 (apply constructor name construct-args)))
           (else
            (lose who "unkown destination kind" kind))))
    (else
     (lose who "invalid destination specification" spec))))

(define-condition-type &config &error
  make-config-error config-error?)

(define (lose who message . irritants)
  (raise (condition (make-config-error)
                    (make-message-condition message)
                    (make-irritants-condition irritants))))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
