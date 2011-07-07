#!r6rs
(library (spells test-runner environment)
  (export this-directory
          test-environment)
  (import (rnrs)
          (srfi :39 parameters)
          (spells pathname))
  
  (define this-directory
    (make-parameter (->namestring
                     (pathname-with-file (->pathname (car (command-line))) #f))))

  (define test-environment (make-parameter #f)))
