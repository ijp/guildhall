;;; -*- Mode: Scheme; scheme48-package: parsing-tests -*-

;;;; Parsing Tools
;;;; Tests

;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.

(define-test-suite parsing-tests
  "Parscheme: portable, combinator-based parsing tools")

(define (test-parsing)
  (run-test parsing-tests))

(define (ps parser stream . loser)      ;Parse Stream
  (parse-stream parser
                stream
                0                       ;initial position
                (lambda (position token)
                  token                 ;ignore
                  (+ position 1))
                #f                      ;no context
                (lambda (result context stream)
                  context stream        ;ignore
                  result)
                (if (pair? loser) (car loser) parser-lossage)))

(define (pl parser list . loser)        ;Parse List
  (apply ps parser (list->stream list) loser))

(define (pt parser string . loser)      ;Parse Text
  (parse-string parser
                string
                #f                      ;no context
                (lambda (result context stream)
                  context stream        ;ignore
                  result)
                (if (pair? loser) (car loser) parser-lossage)))

(define (pf parser pathname . loser)    ;Parse File
  (parse-file parser
              pathname
              #f                        ;no context
              (lambda (result context stream)
                context stream          ;ignore
                result)
              (if (pair? loser) (car loser) parser-lossage)))

(define (parser-lossage perror stream context)
  stream                                ;ignore
  (error "Parse error:"
         (parse-error/position perror)
         (parse-error/messages perror)))

;;;; Trivial Examples

(define-test-case parsing-tests epsilon ()
  (test-eq 'FOO
    (pl (parser:epsilon (lambda () 'FOO))
        '())))

(define-test-case parsing-tests trivial-failure ()
  (test-eq 'LOSSAGE
    (pl (parser:on-failure (lambda (perror context stream)
                             context stream ;ignore
                             'LOSSAGE)
          (parser:error "Failure!"))
        '())))

(define-test-case parsing-tests monad-identity ()
  (test-eq 'FOO
    (pl (parser:extend (parser:return 'FOO) parser:return)
        '())))

(define-test-case parsing-tests token* ()
  (test-eqv 2
    (pl (parser:token* (lambda (token win lose)
                         (if (and (pair? token)
                                  (number? (car token))
                                  (number? (cdr token)))
                             (win (- (car token) (cdr token)))
                             (lose))))
        '((4 . 2)))))

(define-test-case parsing-tests token-if ()
  (test-eqv 16
    (pl (parser:token-if number? square)
        '(4))))

(define-test-case parsing-tests sequence ()
  (test-eqv #\y
    (pt (parser:sequence (parser:char= #\x) (parser:char= #\y))
        "xy")))

(define-test-case parsing-tests peek ()
  (test-eqv #\x
    (pt (parser:sequence (parser:peek (parser:eqv-token #\x))
                         (parser:eqv-token #\x))
        "xy")))

(define-test-case parsing-tests choice-yes ()
  (test-eqv -4
    (pl test-parser:contrived-choice
        '(4))))

(define-test-case parsing-tests choice-no ()
  (test-eqv 16
    (pl test-parser:contrived-choice
        '(-4))))

(define-parser test-parser:contrived-choice
  (parser:choice (parser:map square (parser:token-if negative?))
                 (parser:map - (parser:token-if integer?))))

(define (square x) (* x x))

(define-test-case parsing-tests backtracking ()
  (test-eqv 'C
    (pl (parser:deep-choice
         (parser:sequence (parser:eqv-token 'A)
                          (parser:eqv-token 'B))
         (parser:sequence (parser:eqv-token 'A)
                          (parser:eqv-token 'C)))
        '(a c))))

(define-test-case parsing-tests nested-backtracking ()
  (test-eqv 'Q
    (pl (parser:deep-choice
         (parser:sequence (parser:eqv-token 'A)
                          (parser:deep-choice
                           (parser:sequence (parser:eqv-token 'B)
                                            (parser:eqv-token 'C))
                           (parser:sequence (parser:eqv-token 'B)
                                            (parser:eqv-token 'D))))
         (parser:sequence (parser:eqv-token 'A)
                          (parser:deep-choice
                           (parser:sequence (parser:eqv-token 'P)
                                            (parser:eqv-token 'Q))
                           (parser:sequence (parser:eqv-token 'P)
                                            (parser:eqv-token 'R)))))
        '(a p q))))

(define-test-case parsing-tests optional-present ()
  (test-equal '(A B C)
    (pl test-parser:optional '(a b c))))

(define-test-case parsing-tests optional-absent ()
  (test-equal '(A #F C)
    (pl test-parser:optional '(a c))))

(define-parser test-parser:optional
  (*parser
      (a (parser:eqv-token 'A))
      (b (parser:optional #f (parser:eqv-token 'B)))
      (c (parser:eqv-token 'C))
    (parser:return (list a b c))))

(define-test-case parsing-tests a^n-b^n-c^n.win ()
  (test-eqv 4
    (pl test-parser:a^n-b^n-c^n
        '(a a a a b b b b c c c c))))

(define-test-case parsing-tests a^n-b^n-c^n.lose ()
  (test-equal '(LOSE 4 ("Unexpected token:" B))
    (pl test-parser:a^n-b^n-c^n
        '(a a b b b c c)
        (lambda (perror context stream)
          context stream                ;ignore
          `(LOSE ,(parse-error/position perror)
                 ,@(parse-error/messages perror))))))

(define-parser test-parser:a^n-b^n-c^n
  (*parser
      (n (parser:repeated (lambda (token count)
                            token       ;ignore
                            (+ count 1))
                          (parser:return 0)
                          (parser:eqv-token 'A)))
      ((parser:noise:exactly n (parser:eqv-token 'B)))
      ((parser:noise:exactly n (parser:eqv-token 'C)))
    (parser:return n)))

;;;; MIME Example

(define-test-suite (parsing-tests.mime parsing-tests)
  "Example parser: MIME headers")

(define-test-case parsing-tests.mime content-disposition ()
  (test-equal '(ATTACHMENT (FILENAME . "mumble.txt"))
    (pl mime-parser:content-disposition
        '("attachment" #\; "filename" #\= "\"mumble.txt\""))))

(define-test-case parsing-tests.mime content-type ()
  (test-equal '(TEXT PLAIN (CHARSET . "UTF-8") (FORMAT . "flowed"))
    (pl mime-parser:content-type
        '("text" #\/ "plain" #\;
          "charset" #\= "\"UTF-8\"" #\;
          "format" #\= "flowed"))))

(define-parser mime-parser:content-disposition
  (*parser
      (type (parser:token-if string? intern))
      (parameters mime-parser:parameters)
    (parser:return (cons type parameters))))

(define-parser mime-parser:content-type
  (*parser
      (type (parser:token-if string? intern))
      ((parser:char= #\/))
      (subtype (parser:token-if string? intern))
      (parameters mime-parser:parameters)
    (parser:return (cons type (cons subtype parameters)))))

(define-parser mime-parser:parameters
  (parser:list:repeated
   (*parser
       ((parser:char= #\;))
       (attribute (parser:token-if string? intern))
       ((parser:char= #\=))
       (value (parser:token-if string? rfc822:unquote-string))
     (parser:return (cons attribute value)))))

;;; Random utilities to run this example.

(define (intern string)
  (string->symbol (string-downcase string)))

(define (string-downcase string)
  (let* ((length (string-length string))
         (result (make-string length)))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (string-set! result i (char-downcase (string-ref string i))))
    result))

(define (rfc822:unquote-string string)
  (let ((length (string-length string)))
    (if (and (> length 2)
             (char=? #\" (string-ref string 0))
             (char=? #\" (string-ref string (- length 1))))
        (substring string 1 (- length 1))
        string)))
