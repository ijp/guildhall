(library (spells misc compat)
  (export sleep-seconds scheme-implementation)
  (import (rnrs base)
          (only (ikarus) nanosleep))

  (define (sleep-seconds t)
    (nanosleep (exact (truncate t)) (mod (exact (round (* t #e1e+9))) #e1e+9)))

  (define (scheme-implementation)
    'ikarus))
