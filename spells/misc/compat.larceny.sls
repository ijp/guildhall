(library (spells misc compat)
  (export sleep-seconds exit scheme-implementation)
  (import (rnrs base))

  (define (sleep-seconds t)
    (error 'sleep-seconds "please implement SLEEP-SECONDS for this implementation"))

  (define (exit status)
    (error 'exit "please implement EXIT for this implementation"))
  
  (define (scheme-implementation)
    'larceny))
