;;; mapping.sls --- Applying mappings to inventories

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

;; TODO: Use conditions instead of `error'

;;; Code:
#!r6rs

(library (dorodango inventory mapping)
  (export make-inventory-mapper
          inventory-mapper?
          inventory-mapper-map-leaf
          inventory-mapper-map-container

          apply-inventory-mapper
          evaluate-inventory-mapping-rules

          identity-inventory-mapper
          make-recursive-inventory-mapper
          null-inventory-mapper)
  (import (rnrs)
          (only (srfi :1) append-map filter-map)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (wak foof-loop)
          (wak irregex)
          (only (spells misc) and=> or-map)
          (spells record-types)
          (spells match)
          (spells tracing) ;debug
          (dorodango inventory))


(define-record-type inventory-mapper
  (fields map-leaf map-container))

(define (map-leaf mapper reverse-path)
  ((inventory-mapper-map-leaf mapper) reverse-path))

(define (map-container mapper reverse-path)
  ((inventory-mapper-map-container mapper) reverse-path))

(define (apply-inventory-mapper mapper dest source)
  (loop continue
      ((for from (in-inventory source (result source-result)))
       (with to dest))
    => (values to source-result)
    (let ((name (inventory-name from)))
      (define (move+iterate path)
        (let ((dest (inventory-leave-n
                     (inventory-update to path from)
                     (length path))))
          (receive (empty? cursor) (inventory-delete from)
            (if empty?
                (values dest (inventory-leave cursor))
                (continue (=> from cursor)
                          (=> to dest))))))
      (define (handle-container path sub-mapper)
        (if sub-mapper
            (let ((to-descendant
                   (if (null? path)
                       to
                       (inventory-update to
                                         path
                                         #t
                                         (inventory-data from)))))
              (receive (dest source)
                       (apply-inventory-mapper sub-mapper to-descendant from)
                (let ((dest (if (inventory-empty? dest)
                                to
                                (inventory-leave-n dest (length path))))
                      (next (inventory-next source)))
                  (if next
                      (continue (=> from next)
                                (=> to dest))
                      (values dest (inventory-leave source))))))
            (move+iterate path)))
      (if (inventory-leaf? from)
          (cond ((map-leaf mapper name)  => move+iterate)
                (else
                 (continue)))
          (receive (path sub-mapper)
                   (map-container mapper name)
            (if path
                (handle-container path sub-mapper)
                (continue)))))))



(define make-mapper make-inventory-mapper)

(define null-inventory-mapper
  (make-mapper (lambda (path)
                 #f)
               (lambda (path)
                 (values #f #f))))

(define identity-inventory-mapper
  (make-mapper (lambda (filename)
                 (list filename))
               (lambda (filename)
                 (values (list filename) #f))))

(define (make-recursive-inventory-mapper transform)
  (letrec ((mapper (make-inventory-mapper
                    (lambda (filename)
                      (transform filename))
                    (lambda (filename)
                      (values (transform filename)
                              mapper)))))
    mapper))

;; Mapping rule evaluation

(define-record-type mapper-rule
  (fields path submapper destination))

(define-functional-fields mapper-rule path submapper destination)

(define (evaluate-inventory-mapping-rules rules lookup-mapper)
  (mapper-rules->mapper
   (append-map (lambda (rule)
                 (evaluate-mapping-rule rule lookup-mapper))
               rules)))

(define (mapper-rules->mapper rules)
  (make-mapper (mapper-rules->map-leaf rules)
               (mapper-rules->map-container rules)))


(define (mapper-rules->map-leaf rules)
  (lambda (filename)
    (loop continue ((for rule (in-list rules)))
      => #f
      (let ((path (mapper-rule-path rule)))
        (cond ((null? path)
               (let ((submapper (or (mapper-rule-submapper rule)
                                    identity-inventory-mapper)))
                 (cond ((map-leaf submapper filename)
                        => (lambda (path)
                             (append (mapper-rule-destination rule) path)))
                       (else
                        (continue)))))
              ((and (null? (cdr path))
                    (string=? filename (car path)))
               (mapper-rule-destination rule))
              (else
               (continue)))))))

(define (mapper-rules->map-container rules)
  (lambda (filename)
    (loop continue ((for rule (in-list rules))
                    (with matching-rules '()))
      => (if (null? matching-rules)
             (values #f #f)
             (values '() (mapper-rules->mapper (reverse matching-rules))))
      (let ((path (mapper-rule-path rule)))
        (cond ((null? path)
               (receive (new-path new-mapper)
                        (map-container (or (mapper-rule-submapper rule)
                                           identity-inventory-mapper)
                                       filename)
                 (cond ((not new-path)
                        (continue))
                       ((not new-mapper)
                        (values (append (mapper-rule-destination rule)
                                        new-path)
                                #f))
                       (else
                        (continue
                         (=> matching-rules
                             (cons (make-mapper-rule
                                    '()
                                    new-mapper
                                    (append (mapper-rule-destination rule)
                                            new-path))
                                   matching-rules)))))))
              ((string=? filename (car path))
               (if (and (null? (cdr path))
                        (not (mapper-rule-submapper rule)))
                   (values (mapper-rule-destination rule) #f)
                   (continue
                    (=> matching-rules
                        (cons (mapper-rule-modify-path rule cdr)
                              matching-rules)))))
              (else
               (continue)))))))

;; Returns a path and a mapper
(define (parse-mapping-expr expr lookup-mapper)
  ;;++ use a dedicated condition type
  (define (lose message . irritants)
    (apply error 'parse-mapping-expr message irritants))
  (define (lookup/lose symbol)
    (or (lookup-mapper symbol)
        (lose "unknown mapper" symbol)))
  (define (maybe-parse-tail thing)
    (cond ((symbol? thing)
           (lookup/lose thing))
          ((and (pair? thing)
                 (eq? ': (car thing)))
            (make-irregex-mapper (irregex thing)))
          (else
           #f)))
  (cond ((string? expr) ;convinience case
         (values (list expr) #f))
        (else
         (let next ((lst expr)
                    (path '()))
           (cond ((null? lst)
                  (values (reverse path) #f))
                 ((maybe-parse-tail lst)
                  => (lambda (mapper)
                       (values (reverse path) mapper)))
                 ((and (pair? lst)
                       (string? (car lst)))
                  (next (cdr lst) (cons (car lst) path)))
                 (else
                  (lose "invalid mapping expression" expr)))))))

(define (make-irregex-mapper irx)
  (make-mapper (lambda (filename)
                 (and (irregex-match irx filename)
                      (list filename)))
               (lambda (filename)
                 (values (if (irregex-match irx filename)
                             (list filename)
                             #f)
                         #f))))

(define (parse-path path)
  (match path
    (((? string? components) ___) components)
    ((? string? path)             (list path))
    (else                         (error 'parse-path "invalid path" path))))

(define (evaluate-mapping-rule rule lookup-mapper)
  (match rule
    ((source '-> dest)
     (receive (path submapper)
              (parse-mapping-expr source lookup-mapper)
       (list (make-mapper-rule path submapper (parse-path dest)))))
    (('exclude . expressions)
     (map (lambda (expr)
            (receive (path submapper) (parse-mapping-expr expr lookup-mapper)
              (make-mapper-rule path submapper #f)))
          expressions))
    (source
     (receive (path submapper)
              (parse-mapping-expr source lookup-mapper)
       (list (make-mapper-rule path submapper path))))))
)

;; Local Variables:
;; scheme-indent-styles: (foof-loop (match 1))
;; End:
