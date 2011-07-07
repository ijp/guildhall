(library (spells misc compat)
  (export sleep-seconds scheme-implementation)
  (import (rnrs base))

  ;;@ Sleep @1 seconds.
  (define (sleep-seconds t)
    (error "please implement SLEEP-SECONDS for this implementation"))

  ;;@ Return a symbol indicating the scheme implementation
  (define (scheme-implementation)
    'unknown))
