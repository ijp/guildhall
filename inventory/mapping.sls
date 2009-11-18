;;; mapping-rules.sls --- 

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
          null-inventory-mapper)
  (import (rnrs)
          (srfi :8 receive)
          (only (spells misc) and=>)
          (spells record-types)
          (spells match)
          (spells foof-loop)
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
        #;
        (log/categorizer 'debug (cat "moving item " (dsp-path/reverse r-path)
                                     " to " (dsp-path path)))
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

(define (mapper-with-destination mapper destination)
  (let ((map-leaf (inventory-mapper-map-leaf mapper))
        (map-container (inventory-mapper-map-container mapper)))
    (make-mapper (lambda (filename)
                   (and=> (map-leaf filename)
                          (lambda (path)
                            (append destination path))))
                 (lambda (filename)
                   (receive (path submapper) (map-container filename)
                     (if path
                         (values (append destination path) submapper)
                         (values #f #f)))))))

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

;; Mapping rule evaluation

(define-record-type mapper-rule
  (fields path submapper destination))

(define-functional-fields mapper-rule path submapper destination)

(define (evaluate-inventory-mapping-rules rules lookup-mapper)
  (mapper-rules->mapper
   (map (lambda (rule)
          (evaluate-mapping-rule rule lookup-mapper))
        rules)))

(define (mapper-rules->mapper rules)
  (let ((catch-all (and=> (memp (lambda (rule)
                                (null? (mapper-rule-path rule)))
                                rules)
                          car)))
    (if catch-all
        (mapper-with-destination (or (mapper-rule-submapper catch-all)
                                     identity-inventory-mapper)
                                 (mapper-rule-destination catch-all))
        (make-mapper
         (lambda (filename)
           (let ((rule
                  (find (lambda (rule)
                          (let ((path (mapper-rule-path rule)))
                            (and (not (null? path))
                                 (null? (cdr path))
                                 (string=? filename (car path)))))
                        rules)))
             (and rule (mapper-rule-destination rule))))
         (lambda (filename)
           (let ((mapper
                  (exists
                   (lambda (rule)
                     (let ((path (mapper-rule-path rule)))
                       (cond ((null? path)
                              (mapper-with-destination
                               (or (mapper-rule-submapper rule)
                                   identity-inventory-mapper)
                               (mapper-rule-destination rule)))
                             ((string=? filename (car path))
                              (mapper-rules->mapper
                               (list
                                (mapper-rule-with-path rule (cdr path)))))
                             (else
                              #f))))
                   rules)))
             (if mapper
                 (values '() mapper)
                 (values #f #f))))))))

;; Returns a path and a mapper
(define (parse-mapping-expr expr lookup-mapper)
  (cond ((string? expr)
         (values (list expr) #f))
        ((symbol? expr)
         (values '() (lookup-mapper expr)))
        ((null? expr)
         (values '() #f))
        ((pair? expr)
         (let next ((lst expr)
                    (path '()))
           (cond ((pair? lst)
                  (next (cdr lst) (cons (car lst) path)))
                 ((null? lst)
                  (values (reverse path) #f))
                 (else
                  (values (reverse path) (lookup-mapper lst))))))
        (else
         (error 'parse-mapping-expr "invalid expression" expr))))

(define (parse-path path)
  (match path
    (((? string? components) ___) components)
    ((? string? path)             (list path))
    (else                         (error 'parse-path "invalid path" path))))

(define (evaluate-mapping-rule rule lookup-mapper)
  (define who 'evaluate-mapping-rule)
  (match rule
    ((source '-> dest)
     (receive (path submapper)
              (parse-mapping-expr source lookup-mapper)
       (make-mapper-rule path submapper (parse-path dest))))
    (source
     (receive (path submapper)
              (parse-mapping-expr source lookup-mapper)
       (make-mapper-rule path submapper path)))))
)
