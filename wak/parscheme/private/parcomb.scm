;;; -*- Mode: Scheme; scheme48-package: parser-combinators -*-

;;;; Parsing Tools
;;;; Parser Combinators

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-record-type <parse-state>
    (make-parse-state stream position advancer stack context)
    parse-state?
  (stream parse-state/stream)
  (position parse-state/position)
  (advancer parse-state/advancer)
  (stack parse-state/stack)
  (context parse-state/context))

(define (parse-stream parser stream position advancer context win lose)
  (run 'PARSE parser
       (initial-parse-state stream position advancer context lose)
       (lambda (pstate value perror)
         perror                         ;ignore
         (win value
              (parse-state/context pstate)
              (parse-state/stream pstate)))))

(define (initial-parse-state stream position advancer context lose)
  (make-parse-state stream position advancer (luser lose) context))

;;; Convert a user's loser (a luser) into a loser.  This is silly.

(define (luser lose)
  (lambda (pstate perror)
    (lose perror
          (parse-state/context pstate)
          (parse-state/stream pstate))))

(define (parse-state-with-context pstate context)
  (make-parse-state (parse-state/stream pstate)
                    (parse-state/position pstate)
                    (parse-state/advancer pstate)
                    (parse-state/stack pstate)
                    context))

(define (parse-state-with-stack pstate stack)
  (make-parse-state (parse-state/stream pstate)
                    (parse-state/position pstate)
                    (parse-state/advancer pstate)
                    stack
                    (parse-state/context pstate)))

(define (parse-state/end? pstate)
  (stream-null? (parse-state/stream pstate)))

(define (parse-state/next-token pstate)
  (stream-car (parse-state/stream pstate)))

(define (parse-state/advance pstate)
  (let ((context (parse-state/context pstate))
        (stream (parse-state/stream pstate))
        (position (parse-state/position pstate))
        (advancer (parse-state/advancer pstate))
        (stack (parse-state/stack pstate)))
    (make-parse-state (stream-cdr stream)
                      (advancer position (stream-car stream))
                      advancer
                      (flush-parse-stack stack)
                      context)))

;;;; Parse Stack Management

;;; The parse stack is a list terminated by a final losing continuation
;;; or a parse state to which to backtrack.  The elements of the stack
;;; are losing continuations.
;;;
;;; This page was substantially more complex until I decided to use a
;;; flat continuation-per-frame representation of the stack, rather
;;; than a list of lists of continuations representing choices.  I
;;; originally tried the more complex format in the hope that it would
;;; benefit PARSER:CHOICE to to as little preprocessing as possible,
;;; but then I realized (1) it has to do preprocessing anyway, and (2)
;;; this thing is not going to be fast unless it runs in a fast Scheme
;;; that properly integrates procedures.

(define (parse-state/push pstate continuation)
  (parse-state-with-stack pstate
                          (cons continuation (parse-state/stack pstate))))

(define (parse-state/pop pstate)
  (let ((stack (parse-state/stack pstate)))
    (cond ((pair? stack)                ;+++ Common case first.
           (parse-state/*pop pstate stack))
          ((parse-state? stack)         ;Preserved parse state.
           (parse-state/*pop stack (parse-state/stack stack)))
          (else                         ;Final continuation.
           (values stack pstate)))))

(define (parse-state/*pop pstate stack)
  (values (car stack)
          (parse-state-with-stack pstate (cdr stack))))

(define (parse-state/preserve pstate)
  (if (pair? (parse-state/stack pstate))
      (parse-state-with-stack pstate pstate)
      pstate))

(define (parse-state/flush pstate)
  (parse-state-with-stack pstate (flush-parse-stack pstate)))

(define (flush-parse-stack stack)
  (let loop ((stack stack))
    (if (pair? stack)
        (loop (cdr stack))
        stack)))

;;;; Parser Drivers

;;; This little abstraction for driving the parser exists for the
;;; purpose of carefully controlling tracing behaviour.  If the
;;; conditionals on *PARSE-TRACE?* are eliminated, all of these
;;; procedures will integrate nicely to straight tail calls with no
;;; indirection through a top-level reference.  (Scheme48 is too stupid
;;; to eliminate the conditionals even when it can *prove* that
;;; *PARSE-TRACE?* will be false, if ENABLE-PARSE-TRACE and
;;; DISABLE-PARSE-TRACe are commented out so that there is no
;;; assignment to it.  Blah.)
;;;
;;; The tracing is not especially well-engineered at the moment; it is
;;; mostly useful for me to scan to get an idea of whether the parser
;;; is being remotely sane.  However, the way I have written the code
;;; below makes it easy for a better-engineered mechanism than what is
;;; on this page to be substituted trivially.

(define (run marker parser pstate win)
  (if *parse-trace?*
      (begin
        (write `(RUN ,marker))
        (newline)))
  (parser pstate win))

(define (success* marker win pstate value)
  (success marker win pstate value
           (make-parse-error:unknown (parse-state/position pstate))))

(define (success marker win pstate value perror)
  (if *parse-trace?*
      (begin
        (write `(SUCCESS ,marker -> ,value
                         ,(parse-error/position perror)
                         ,@(parse-error/messages perror)))
        (newline)))
  (win pstate value perror))

(define (failure marker pstate perror)
  (if *parse-trace?*
      (begin
        (write `(FAILURE ,marker
                         ,(parse-error/position perror)
                         ,@(parse-error/messages perror)))
        (newline)))
  (receive (lose pstate) (parse-state/pop pstate)
    (lose pstate perror)))

(define *parse-trace?* #f)

(define (enable-parse-trace) (set! *parse-trace?* #t))
(define (disable-parse-trace) (set! *parse-trace?* #f))

(define (with-parse-trace thunk)
  (if *parse-trace?*
      (thunk)
      (dynamic-wind enable-parse-trace
                    thunk
                    disable-parse-trace)))

;;;; Primitive Parser Combinators

(define (parser:epsilon thunk)
  (lambda (pstate win)
    (success* 'PARSER:EPSILON win pstate (thunk))))

(define (parser:error . messages)
  (lambda (pstate win)
    win                                 ;ignore
    (failure 'PARSER:ERROR
             pstate
             (make-parse-error (parse-state/position pstate)
                               messages))))

(define (parser:on-failure lose parser)
  (lambda (pstate win)
    (run 'PARSER:ON-FAILURE parser
         (parse-state/push pstate (luser lose))
         win)))

(define (parser:extend parser extender)
  (lambda (pstate win)
    (run 'PARSER:EXTEND parser pstate
         (lambda (pstate value perror)
           (run '(PARSER:EXTEND EXTENSION) (extender value)
                (parse-state/push pstate
                  (lambda (pstate* perror*)
                    (failure '(PARSER:EXTEND EXTENSION)
                             pstate*
                             (merge-parse-errors perror perror*))))
                win)))))

(define (parser:binary-choice left-parser right-parser)
  (lambda (pstate win)
    (run '(PARSER:BINARY-CHOICE LEFT) left-parser
         (parse-state/push pstate
           (lambda (pstate perror)
             (run '(PARSER:BINARY-CHOICE RIGHT) right-parser
                  (parse-state/push pstate
                    (lambda (pstate* perror*)
                      (failure 'PARSER:CHOICE
                               pstate
                               (merge-parse-errors perror perror*))))
                  win)))
         win)))

(define (parser:backtrackable parser)
  (lambda (pstate win)
    (run 'PARSER:BACKTRACKABLE parser
         (parse-state/preserve
          (parse-state/push pstate
            (lambda (pstate perror)
              (failure 'PARSER:BACKTRACKABLE
                       pstate
                       (parse-error-with-position
                        perror
                        (parse-state/position pstate))))))
         (lambda (pstate* value perror)
           (success 'PARSER:BACKTRACKABLE
                    win
                    (parse-state-with-stack pstate* (parse-state/stack pstate))
                    value
                    perror)))))

;;;;; Context

;;; This can store the the context in context-sensitive parsers.

(define (parser:context)
  (lambda (pstate win)
    (success* 'PARSER:CONTEXT win pstate (parse-state/context pstate))))

(define (parser:set-context context)
  (lambda (pstate win)
    (success* 'PARSER:SET-CONTEXT
              win
              (parse-state-with-context pstate context)
              context)))

(define (parser:call-with-context receiver)
  (lambda (pstate win)
    (success* 'PARSER:CALL-WITH-CONTEXT
              win
              pstate
              (receiver (parse-state/context pstate)))))

(define (parser:modify-context modifier)
  (lambda (pstate win)
    (let ((context* (modifier (parse-state/context pstate))))
      (success* 'PARSER:MODIFY-CONTEXT
                win
                (parse-state-with-context pstate context*)
                context*))))

;;;;; Odds and Ends

;;; This totally loses because most Scheme systems won't integrate
;;; PARSER:TOKEN*.  Phooey.

(define (parser:token* processor)
  (lambda (pstate win)
    (if (parse-state/end? pstate)
        (failure 'PARSER:TOKEN*
                 pstate
                 (make-parse-error:unexpected-end-of-input
                  (parse-state/position pstate)))
        (let ((token (parse-state/next-token pstate)))
          (processor token
                     (lambda (value)
                       (success* 'PARSER:TOKEN*
                                 win
                                 (parse-state/advance pstate)
                                 value))
                     (lambda ()
                       (failure 'PARSER:TOKEN*
                                pstate
                                (make-parse-error:unexpected-token
                                 token
                                 (parse-state/position pstate)))))))))

(define (parser:peek parser)
  (lambda (pstate win)
    (run 'PARSER:PEEK parser
         (parse-state/push pstate
           (lambda (pstate* perror*)
             pstate*                    ;ignore
             (failure 'PARSER:PEEK
                      pstate
                      (parse-error-with-position
                       perror*
                       (parse-state/position pstate)))))
         (lambda (pstate* value perror)
           pstate*                      ;ignore
           (success 'PARSER:PEEK win pstate value perror)))))

(define (parser:end)
  (lambda (pstate win)
    (if (parse-state/end? pstate)
        (success* 'PARSER:END win pstate '())
        (failure 'PARSER:END
                 pstate
                 (make-parse-error:trailing-garbage
                  (parse-state/position pstate))))))

(define (parser:return value)
  (parser:epsilon (lambda () value)))

(define (parser:delayed promise)
  (lambda (pstate win)
    (run 'PARSER:DELAYED (force promise) pstate win)))

;;; This will label only the entrance to the parse state.  To label
;;; exit as well, use PARSER:ON-FAILURE or PARSER:EXTEND.

(define (parser:label name parser)
  (lambda (pstate win)
    (run name parser pstate win)))

;;;; Syntactic Sugar

;;; In order not to surprise users who write parsers as top-level
;;; variables or mutually recursive procedures, we delay the top-level
;;; of *PARSER forms.  Consider, for example,
;;;
;;;   (define-parser mumble-parser:foo
;;;     (*parser
;;;         (frob mumble-parser:frob)
;;;         (mumble-parser:zarquon)
;;;       (parser:return frob)))
;;;
;;;   (define-parser mumble-parser:frob ...),
;;;
;;; which would otherwise have a forward reference at the top level and
;;; cause an error; or
;;;
;;;   (define-parser (mumble-parser:foo bar)
;;;     (*parser
;;;         (frob (mumble-parser:frob bar))
;;;         ((mumble-parser:zarquon))
;;;       (parser:return frob)))
;;;
;;;   (define-parser (mumble-parser:frob bar)
;;;     (parser:choice (parser:sequence mumble-parser:fnord
;;;                                     (mumble-parser:foo bar))
;;;                    ...)),
;;;
;;; which would (and did, for me!) lead to a very confusing infinite
;;; loop that would blow the stack.  Unfortunately, this leads to a
;;; dreadful inefficiency which I am powerless to avoid.  (I'm not sure
;;; precisely what impact this has on performance, though.)
;;;
;;; I am not sure whether the code on the following pages ought to use
;;; DEFINE-PARSER.  (Primarily, it would clutter up traces, but that is
;;; sometimes what I want.  Ah, how nice it would be to have a high-
;;; level mechanism for describing precisely and accurately the traces
;;; that I want to see at exactly the levels I want to see.)

(define-syntax *parser
  (syntax-rules ()
    ((*PARSER tail-parser)
     tail-parser)

    ((*PARSER (variable parser) more ...)
     (PARSER:EXTEND parser (LAMBDA (variable) (*PARSER more ...))))

    ((*PARSER (parser) more ...)
     (*PARSER (IGNORED parser) more ...))))

(define-syntax define-parser
  (syntax-rules ()
    ((DEFINE-PARSER (name . bvl) parser)
     (DEFINE (name . bvl)
       (PARSER:LABEL 'name (PARSER:DELAYED (DELAY parser)))))

    ((DEFINE-PARSER name parser)
     (DEFINE name
       (PARSER:LABEL 'name (PARSER:DELAYED (DELAY parser)))))))

;;;; Higher-Level Utilities

(define (parser:complete parser)
  (*parser (value parser)
           ((parser:end))
    (parser:return value)))

(define (parser:token . mapper-option)
  (parser:token* (if (pair? mapper-option)
                     (let ((mapper (car mapper-option)))
                       (lambda (token win lose)
                         lose           ;ignore
                         (win (mapper token))))
                     (lambda (token win lose)
                       lose             ;ignore
                       (win token)))))

(define (parser:token-if predicate . mapper-option)
  (parser:token* (if (pair? mapper-option)
                     (let ((mapper (car mapper-option)))
                       (lambda (token win lose)
                         (if (predicate token)
                             (win (mapper token))
                             (lose))))
                     (lambda (token win lose)
                       (if (predicate token)
                           (win token)
                           (lose))))))

(define (parser:eqv-token token)
  (parser:token-if (lambda (token*) (eqv? token* token))))

(define (parser:binary-sequence first-parser second-parser)
  (*parser (first-parser) second-parser))

(define (parser:sequence sequent . sequents)
  (reduce-right parser:binary-sequence #f (cons sequent sequents)))

(define (parser:choice alternative . alternatives)
  (reduce-right parser:binary-choice #f (cons alternative alternatives)))

(define (parser:deep-choice alternative . alternatives)
  (let recur ((alternative alternative) (alternatives alternatives))
    (if (pair? alternatives)
        (parser:binary-choice (parser:backtrackable alternative)
                              (recur (car alternatives) (cdr alternatives)))
        alternative)))                  ;** Last one is not backtrackable.

(define (parser:map mapper parser)
  (parser:extend parser
                 (lambda (value)
                   (parser:epsilon (lambda ()
                                     (mapper value))))))

(define (parser:optional default-value parser)
  (parser:choice parser (parser:return default-value)))

(define (parser:optional-noise parser)
  (parser:choice (parser:sequence parser (parser:return #t))
                 (parser:return #f)))

(define (parser:refuse parser error)
  (parser:choice (parser:extend parser error)
                 (parser:return '())))

;;;;; Repetition

;;; It would be nice to be able to generally instantiate these somehow.
;;; Consider, e.g, (DEFINE-REPEATING-PARSERS CONS '() REVERSE (REPEATED
;;; PARSER:LIST:REPEATED) (AT-LEAST PARSER:LIST:AT-LEAST) ...).

(define (parser:repeated combiner seed parser)
  (*parser (result seed)
    (let loop ((result result))
      (parser:choice (*parser (item parser) (loop (combiner item result)))
                     (parser:return result)))))

(define (parser:repeated-until terminal-parser combiner seed parser)
  (*parser (result seed)
    (let loop ((result result))
      (parser:choice (*parser (terminal-parser) (parser:return result))
                     (*parser (item parser) (loop (combiner item result)))))))

(define (parser:at-most n combiner seed parser)
  (*parser (result seed)
    (let loop ((n n) (result result))
      (let ((final (parser:return result)))
        (if (zero? n)
            final
            (parser:choice (*parser (item parser)
                             (loop (- n 1) (combiner item result)))
                           final))))))

(define (parser:at-most-until n terminal-parser combiner seed parser)
  (*parser (result seed)
    (let loop ((n n) (result result))
      (let* ((final (parser:return result))
             (terminal (parser:sequence terminal-parser final)))
        (if (zero? n)
            terminal
            (parser:choice terminal
                           (*parser (item parser)
                             (loop (- n 1) (combiner item result)))))))))

(define (parser:exactly n combiner seed parser)
  (*parser (result seed)
    (let loop ((n n) (result result))
      (if (zero? n)
          (parser:return result)
          (*parser (item parser)
            (loop (- n 1) (combiner item result)))))))

(define (parser:at-least n combiner seed parser)
  (parser:repeated combiner (parser:exactly n combiner seed parser) parser))

(define (parser:at-least-until n terminal-parser combiner seed parser)
  (parser:repeated-until terminal-parser combiner
      (parser:exactly n combiner seed parser)
    parser))

(define (parser:between n m combiner seed parser)
  (parser:at-most (- m n) combiner (parser:exactly n combiner seed parser)
    parser))

(define (parser:between-until n m terminal-parser combiner seed parser)
  (parser:at-most-until (- m n) terminal-parser combiner
      (parser:exactly n combiner seed parser)
    parser))

;;;;;; Specialized Repetitions: Noise and Lists

(define (ignore-noise item result)
  item                                  ;ignore
  result)

(define null-parser (parser:return '()))

(define (parser:noise:repeated parser)
  (parser:repeated ignore-noise null-parser parser))

(define (parser:noise:repeated-until terminal-parser parser)
  (parser:repeated-until terminal-parser ignore-noise null-parser parser))

(define (parser:noise:at-most n parser)
  (parser:at-most n ignore-noise null-parser parser))

(define (parser:noise:at-most-until n terminal-parser parser)
  (parser:at-most-until n terminal-parser ignore-noise null-parser parser))

(define (parser:noise:exactly n parser)
  (parser:exactly n ignore-noise null-parser parser))

(define (parser:noise:at-least n parser)
  (parser:at-least n ignore-noise null-parser parser))

(define (parser:noise:at-least-until n terminal-parser parser)
  (parser:at-least-until n terminal-parser ignore-noise null-parser parser))

(define (parser:noise:between n m parser)
  (parser:between n m ignore-noise null-parser parser))

(define (parser:noise:between-until n m terminal-parser parser)
  (parser:between-until n m terminal-parser ignore-noise null-parser parser))

(define (parser:reverse parser)
  (parser:map reverse parser))

(define (parser:list:repeated parser)
  (parser:reverse (parser:repeated cons null-parser parser)))

(define (parser:list:repeated-until terminal-parser parser)
  (parser:reverse
   (parser:repeated-until terminal-parser cons null-parser parser)))

(define (parser:list:at-most n parser)
  (parser:reverse (parser:at-most n cons null-parser parser)))

(define (parser:list:at-most-until n terminal-parser parser)
  (parser:reverse
   (parser:at-most-until n terminal-parser cons null-parser parser)))

(define (parser:list:exactly n parser)
  (parser:reverse (parser:exactly n cons null-parser parser)))

(define (parser:list:at-least n parser)
  (parser:reverse (parser:at-least n cons null-parser parser)))

(define (parser:list:at-least-until n terminal-parser parser)
  (parser:reverse
   (parser:at-least-until n terminal-parser cons null-parser parser)))

(define (parser:list:between n m parser)
  (parser:reverse
   (parser:between n m cons null-parser parser)))

(define (parser:list:between-until n m terminal-parser parser)
  (parser:reverse
   (parser:between-until n m terminal-parser cons null-parser parser)))

;;;;; Brackets

;;; Did I mean to populate this page with anything else?  It's awfully
;;; bare.

(define (parser:bracketed left-bracket right-bracket body-parser)
  (*parser
      (left-bracket)
      (body body-parser)
      (right-bracket)
    (parser:return body)))

(define (parser:bracketed* left-bracket right-bracket combiner seed parser)
  (parser:sequence left-bracket
                   (parser:repeated-until right-bracket combiner seed parser)))

(define (parser:bracketed-noise left-bracket right-bracket parser)
  (parser:bracketed* left-bracket right-bracket ignore-noise null-parser
    parser))

(define (parser:bracketed-list left-bracket right-bracket parser)
  (parser:reverse
   (parser:bracketed* left-bracket right-bracket cons null-parser parser)))

;;;; Matching Parsers

;;; This is a bit kludgey, because the matcher combinators are
;;; primitive: they don't track source position for us.  Also, this
;;; procedure is three lines too long.

(define (parser:match matcher processor)
  (lambda (pstate win)
    (let ((stream (parse-state/stream pstate))
          (position (parse-state/position pstate))
          (advancer (parse-state/advancer pstate))
          (stack (parse-state/stack pstate))
          (context (parse-state/context pstate)))
      (cond ((match matcher (parse-state/stream pstate))
             => (lambda (stream*)
                  (let ((substream (stream-difference stream stream*)))
                    (success* 'PARSER:MATCH
                              win
                              (make-parse-state
                               stream*
                               (advance-tokens substream position advancer)
                               advancer
                               (if (not (eq? stream stream*))
                                   (flush-parse-stack stack)
                                   stack)
                               context)
                              (processor substream)))))
            (else
             (failure 'PARSER:MATCH
                      pstate
                      (make-parse-error (parse-state/position pstate)
                                        '("Match failed"))))))))

(define (advance-tokens stream position advancer)
  (let loop ((stream stream) (position position))
    (if (stream-null? stream)
        position
        (loop (stream-cdr stream)
              (advancer position
                        (stream-car stream))))))

(define (parser:match->ignore matcher)
  (parser:match matcher
                (lambda (stream)
                  stream                ;ignore
                  '())))

(define (parser:match->list matcher)
  (parser:match matcher stream->list))
