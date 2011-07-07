#!r6rs

(library (spells assert)
  (export assert cerr cout)
  (import (except (rnrs base) assert)
          (rnrs io ports)
          (rnrs io simple)
          (spells include))

  (include-file ((spells private) assert)))
