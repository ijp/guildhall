;;; inventory.scm --- Tests for the inventory tree data structure

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

;;; Code:
#!r6rs

(import (rnrs)
        (srfi :8 receive)
        (spells misc)
        (guildhall ext foof-loop)
        (guildhall ext trc-testing)
        (guildhall inventory)
        (guildhall inventory mapping))

;;; Utilities

(define (leaf-data inventory path)
  (and=> (inventory-ref inventory path)
         (lambda (entry)
           (and (inventory-leaf? entry)
                (inventory-data entry)))))

(define (->inventory tree)
  (let ((inventory (make-inventory (car tree) 'data)))
    (loop continue ((for item (in-list (reverse (cdr tree))))
                    (with cursor (inventory-open inventory)))
      => (inventory-leave cursor)
      (continue
       (=> cursor (if (pair? item)
                      (inventory-insert cursor (->inventory item))
                      (inventory-insert cursor item #f 'data)))))))

(define (->tree inventory)
  (if (inventory-leaf? inventory)
      (inventory-name inventory)
      (loop ((for cursor (in-inventory inventory))
             (for result (listing (->tree cursor))))
        => (cons (inventory-name inventory) result))))

;;; Tests

(define-test-suite inventory-tests
  "Inventory")

(define-test-case inventory-tests basics ()
  (let* ((i0 (make-inventory 'root 'root))
         (i1 (inventory-top (inventory-update i0 '("foo" "bar.txt") #f 'bar)))
         (i2 (inventory-top (inventory-update i1 '("foo" "baz.txt") #f 'baz))))
    (test-eqv #t
      (and=> (inventory-ref i1 '("foo")) inventory-container?))
    (test-eq 'bar
      (leaf-data i1 '("foo" "bar.txt")))
    (test-eq 'baz
      (leaf-data i2 '("foo" "baz.txt")))))

(define-test-suite (inventory-tests.iterator inventory-tests)
  "foof-loop iterator")

(define foo-bar-inventory
  (let* ((i0 (make-inventory 'root 'root))
         (i1 (inventory-insert (inventory-open i0) 'bar #t 'bar)))
    (inventory-leave (inventory-insert i1 'foo #f 42))))

(define-test-case inventory-tests.iterator list ()
  (test-equal '((foo #f 42)
                (bar #t bar))
    (loop ((for item (in-inventory foo-bar-inventory))
           (for result (listing (list (inventory-name item)
                                      (inventory-container? item)
                                      (inventory-data item)))))
      => result)))

(define-test-case inventory-tests.iterator insert ()
  (test-equal '(foo bar frotz)
    (loop continue ((for item (in-inventory foo-bar-inventory))
                    (for i (up-from 0))
                    (for result (listing (inventory-name item))))
      => result
      (if (= i 1)
          (continue (=> item (inventory-next
                              (inventory-insert item 'frotz #f #f))))
          (continue)))))

(define-test-case inventory-tests.iterator final-expr ()
  (test-equal '(foo bar frotz)
    (loop continue ((for item (in-inventory foo-bar-inventory
                                            (result final-inventory)))
                    (for i (up-from 0)))
      => (loop ((for item-2 (in-inventory final-inventory))
                (for result (listing (inventory-name item-2))))
           => result)
      (if (= i 1)
          (continue (=> item (inventory-next
                              (inventory-insert item 'frotz #f #f))))
          (continue))))
  (test-equal 'root
    (loop ((for item (in-inventory (make-inventory 'root #f)
                                   (result result))))
      => (inventory-name result))))

(define (flatten-tree/inventory-cursor tree)
  (loop ((with cursor
               (inventory-cursor (->inventory tree))
               (inventory-cursor-next cursor))
         (while cursor)
         (for result (listing (cons (inventory-cursor-name cursor)
                                    (inventory-cursor-path cursor)))))
    => result))

(define-test-case inventory-tests cursor ()
  (test-equal '((1) (2) (3 b) (4 c b) (5 c b) (6 c b) (7))
    (flatten-tree/inventory-cursor '(a 1 2 (b 3 (c 4 5 6)) 7)))
  (test-equal '((1 c b) (2))
    (flatten-tree/inventory-cursor '(a (b (c 1)) 2))))

(define-test-suite (inventory-tests.merge inventory-tests)
  "Merging")

(define (raise-conflict to from)
  (raise (list 'conflict (->tree to) (->tree from))))

(define-test-case inventory-tests.merge basics ()
  (test-equal '(a "foo" "bar" "baz")
    (->tree (merge-inventories (->inventory '(a "bar" "baz"))
                               (->inventory '(b "foo"))
                               raise-conflict)))
  (test-equal '(a "foo" ("bar" "x" "y") "baz")
    (->tree (merge-inventories (->inventory '(a ("bar") "baz"))
                               (->inventory '(b "foo" ("bar" "y" "x")))
                               raise-conflict)))
  (test-equal '(conflict "foo" ("foo" "x" "y"))
    (guard (e ((list? e) e))
      (merge-inventories (->inventory '(a "foo"))
                         (->inventory '(b ("foo" "x" "y")))
                         raise-conflict))))


;;; Mapping

(define-syntax test-mapper
  (syntax-rules ()
    ((_ expected-dest expected-src mapper dest-tree src-tree)
     (receive (dest source)
              (apply-inventory-mapper mapper
                                      (->inventory dest-tree)
                                      (->inventory src-tree))
       (test-equal (list expected-dest expected-src)
         (list (->tree dest) (->tree source)))))))

(define-test-suite (inventory-tests.map inventory-tests)
  "Mapping between inventories")

(define-test-case inventory-tests.map null ()
  (test-mapper '(a) '(b "foo" ("bar" "x" "y") "baz")
    null-inventory-mapper '(a) '(b "foo" ("bar" "x" "y") "baz")))

(define-test-case inventory-tests.map identity ()
  (test-mapper
      '(a "baz" ("bar" "x" "y") "foo")
      '(b)
    identity-inventory-mapper
    '(a)
    '(b "foo" ("bar" "x" "y") "baz")))

(define-test-case inventory-tests.map rule-1 ()
  (test-mapper
      '(a ("new-bar" "x" "y") "new-foo")
      '(b ("bar") "baz")
    (evaluate-inventory-mapping-rules
     '(("foo" -> "new-foo")
       (("bar" "x") -> ("new-bar" "x"))
       (("bar" "y") -> ("new-bar" "y")))
     (lambda (name) #f))
    '(a)
    '(b "foo" ("bar" "y" "x") "baz")))

(define-test-case inventory-tests.map rule-2 ()
  (test-mapper
      '(a "texinfo.scm"
          ("texinfo" "x.scm"))
      '(b ("scheme" "y.scm") "other")
    (evaluate-inventory-mapping-rules
     '((("scheme" "texinfo.scm") -> "texinfo.scm")
       (("scheme" "texinfo") -> "texinfo"))
     (lambda (name) #f))
    '(a)
    '(b ("scheme"
         ("texinfo" "x.scm")
         "texinfo.scm"
         "y.scm")
        "other")))

(define-test-case inventory-tests.map rule-3 ()
  (test-mapper
      '(a ("bar" "x" "y"))
      '(b)
    (evaluate-inventory-mapping-rules
     '(("foo" -> "bar"))
     (lambda (name) #f))
    '(a)
    '(b ("foo" "x" "y"))))

(define-test-case inventory-tests.map rule-star ()
  (test-mapper
      '(a ("star"
           ("foo" "z.scm" "y.scm")
           ("net" ("private" "foo.scm"))
           "x.scm")
          "top.scm")
      '(b ("private") ("foo"))
    (evaluate-inventory-mapping-rules
     '((* -> "star")
       (("private" "net") -> ("star" "net" "private")))
     (lambda (name)
       (and (eq? name '*) (make-recursive-inventory-mapper list))))
    '(a "top.scm")
    '(b "x.scm"
        ("private" ("net" "foo.scm"))
        ("foo" "y.scm" "z.scm"))))

(define-test-case inventory-tests.map rule-foo ()
  (test-mapper
      '(a ("bar" "foo.scm")
          ("the-x" "a.scm"))
      '(b ("bar" ("c" "d")) "other" ("baz" "y"))
    (evaluate-inventory-mapping-rules
     '((foo -> ())
       ("x" -> "the-x"))
     (letrec ((foo-mapper
               (make-inventory-mapper
                (lambda (filename)
                  (and (string=? filename "bar.scm")
                       '("foo.scm")))
                (lambda (filename)
                  (values (list filename) foo-mapper)))))
       (lambda (name)
         (and (eq? name 'foo) foo-mapper))))
    '(a)
    '(b ("x" "a.scm")
        ("bar" "bar.scm" ("c" "d"))
        "other"
        ("baz" "y"))))

(define-test-case inventory-tests.map regex-rule-1 ()
  (test-mapper
      '(a ("srfi"
           "%3a1.scm"
           ("private" "include.scm")
           ("%3a1" "lists.scm" "srfi-1.scm")))
      '(b "pkg-list.scm")
    (evaluate-inventory-mapping-rules
     '(((: "%3a" (+ digit) (* any)) -> "srfi")
       ("private" -> ("srfi" "private")))
     (lambda (name) #f))
    '(a)
    '(b "pkg-list.scm"
        ("%3a1" "lists.scm" "srfi-1.scm")
        ("private" "include.scm")
        "%3a1.scm")))

(define-test-case inventory-tests.map exclude ()
  (test-mapper
    '(a "awk.scm")
    '(b ("spells"
         ("foreign" "compat.ypsilon.scm")
         "foreign.scm"))
    (evaluate-inventory-mapping-rules
     '((exclude ("spells" "foreign")
                ("spells" "foreign.scm"))
       (("spells" "awk.scm") -> "awk.scm"))
     (lambda (name) #f))
    '(a)
    '(b ("spells"
         "awk.scm"
         ("foreign" "compat.ypsilon.scm")
         "foreign.scm"))))

(exit (run-test-suite inventory-tests))

;; Local Variables:
;; scheme-indent-styles: (trc-testing foof-loop (test-mapper 2))
;; End:
