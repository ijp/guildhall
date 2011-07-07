;;; -*- Mode: Scheme; scheme48-package: matcher-combinators -*-

;;;; Parsing Tools
;;;; Matcher Combinators

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define (match matcher stream)
  (run 'MATCH matcher stream))

(define *match-trace?* #f)

(define (enable-match-trace) (set! *match-trace?* #t))
(define (disable-match-trace) (set! *match-trace?* #f))

(define (with-match-trace thunk)
  (if *match-trace?*
      (thunk)
      (dynamic-wind enable-match-trace
                    thunk
                    disable-match-trace)))

(define (run marker matcher stream)
  (if *match-trace?*
      (begin
        (write `(RUN ,marker))
        (newline)))
  (matcher stream))

(define-syntax define-matcher
  (syntax-rules ()
    ((DEFINE-MATCHER (name . bvl) matcher)
     (DEFINE (name . bvl) matcher))
    ((DEFINE-MATCHER name matcher)
     (DEFINE name
       (LET ((PROMISE (DELAY matcher)))
         (LAMBDA (STREAM)
           (RUN 'name (FORCE PROMISE) STREAM)))))))

(define (matcher:epsilon)
  (lambda (stream)
    stream))

(define (matcher:end)
  (lambda (stream)
    (stream-null? stream)))

(define (matcher:error . messages)
  messages                              ;ignore
  (lambda (stream)
    stream                              ;ignore
    #f))

(define (matcher:binary-choice left-matcher right-matcher)
  (lambda (stream)
    (or (run '(MATCHER:CHOICE LEFT) left-matcher stream)
        (run '(MATCHER:CHOICE RIGHT) right-matcher stream))))

(define (matcher:choice . matchers)
  (reduce-right matcher:binary-choice
                (matcher:error)
                matchers))

(define (matcher:deep-choice . matchers)
  (reduce-right matcher:binary-choice
                (matcher:error)
                matchers))

(define (matcher:binary-sequence first-matcher second-matcher)
  (lambda (stream)
    (cond ((run 'MATCHER:SEQUENCE first-matcher stream)
           => (lambda (stream*)
                (run 'MATCHER:SEQUENCE second-matcher stream*)))
          (else #f))))

(define (matcher:sequence . matchers)
  (reduce-right matcher:binary-sequence
                (matcher:epsilon)
                matchers))

(define (matcher:token)
  (lambda (stream)
    (and (stream-pair? stream)
         (stream-cdr stream))))

(define (matcher:token-if predicate)
  (lambda (stream)
    (and (stream-pair? stream)
         (predicate (stream-car stream))
         (stream-cdr stream))))

(define (matcher:peek matcher)
  (lambda (stream)
    (and (run 'MATCHER:PEEK matcher stream)
         stream)))

(define (matcher:if examiner consumer)
  (lambda (stream)
    (and (examiner stream)
         (consumer stream))))

(define (matcher:optional matcher)
  (matcher:choice matcher (matcher:epsilon)))

;;;; Repetition

(define (matcher:repeated matcher)
  (lambda (stream)
    (let loop ((stream stream))
      (cond ((run 'MATCHER:REPEATED matcher stream) => loop)
            (else stream)))))

(define (matcher:repeated-until terminal-matcher matcher)
  (lambda (stream)
    (let loop ((stream stream))
      (or (run '(MATCHER:REPEATED-UNTIL TERMINAL) terminal-matcher stream)
          (cond ((run '(MATCHER:REPEATED-UNTIL REPEATED) matcher stream)
                 => loop)
                (else #f))))))

(define (matcher:at-most n matcher)
  (lambda (stream)
    (let loop ((stream stream) (i 0))
      (and (<= i n)
           (cond ((run 'MATCHER:AT-MOST matcher stream)
                  => (lambda (stream) (loop stream (+ i 1))))
                 (else stream))))))

(define (matcher:at-most-until n terminal-matcher matcher)
  (define (match-terminal stream)
    (run '(MATCHER:AT-MOST-UNTIL TERMINAL) terminal-matcher stream))
  (lambda (stream)
    (let loop ((stream stream) (i 0))
      (cond ((= i n) (match-terminal stream))
            ((run '(MATCHER:AT-MOST-UNTIL REPEATED) matcher stream)
             => (lambda (stream*) (loop stream* (+ i 1))))
            (else (match-terminal stream))))))

(define (matcher:exactly n matcher)
  (lambda (stream)
    (let loop ((stream stream) (i 0))
      (cond ((= i n) stream)
            ((run 'MATCHER:EXACTLY matcher stream)
             => (lambda (stream) (loop stream (+ i 1))))
            (else #f)))))

(define (matcher:at-least n matcher)
  (matcher:sequence (matcher:exactly n matcher)
                    (matcher:repeated matcher)))

(define (matcher:at-least-until n terminal-matcher matcher)
  (matcher:sequence (matcher:exactly n matcher)
                    (matcher:repeated-until terminal-matcher matcher)))

(define (matcher:between n m matcher)
  (matcher:sequence (matcher:at-least n matcher)
                    (matcher:at-most (- m n) matcher)))

(define (matcher:between-until n m terminal-matcher matcher)
  (matcher:sequence (matcher:exactly n matcher)
                    (matcher:at-most-until (- m n) terminal-matcher matcher)))

(define (matcher:bracketed left-bracket right-bracket body-matcher)
  (matcher:sequence left-bracket right-bracket body-matcher))

(define (matcher:bracketed* left-bracket right-bracket matcher)
  (matcher:sequence left-bracket
                    (matcher:repeated-until right-bracket matcher)))

;;;; Token Matchers

(define (left-comparator-matcher comparator)
  (lambda (comparand)
    (matcher:left-comparison comparator comparand)))

(define (matcher:left-comparison comparator comparand)
  (matcher:token-if (lambda (token)
                      (comparator comparand token))))

(define (right-comparator-matcher comparator)
  (lambda (comparand)
    (matcher:right-comparison comparator comparand)))

(define (matcher:right-comparison comparator comparand)
  (matcher:token-if (lambda (token)
                      (comparator token comparand))))

(define comparator-matcher left-comparator-matcher)
(define matcher:comparison matcher:left-comparison)

(define (guarded-matcher predicate make-consumer)
  (lambda parameters
    (matcher:if (matcher:token-if predicate)
                (apply make-consumer parameters))))


