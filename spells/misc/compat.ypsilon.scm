(library (spells misc compat)
  (export sleep-seconds scheme-implementation)
  (import (rnrs base)
          (only (core primitives) usleep))

  (define (sleep-seconds t)
    (usleep (+ (* (exact (truncate t)) #e1e+6)
               (mod (exact (round (* t #e1e+6))) #e1e+6))))

  (define (scheme-implementation)
    'ypsilon))
