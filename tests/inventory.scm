;;; Utilities

(define (leaf-data inventory path)
  (and=> (inventory-ref inventory path)
         (lambda (entry)
           (and (inventory-leaf? entry)
                (inventory-data entry)))))

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

(define-test-suite (inventory-tests.merge inventory-tests)
  "merging")

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

(run-test-suite inventory-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing foof-loop)
;; End:
