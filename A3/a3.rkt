#lang racket

(require (only-in (file "tokenize.rkt") tokenize))
(define tokens null)
(define temp null)
(define temp_tok null)

(define (consume type)
  (when (empty? tokens)
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first tokens)])
    (when (not (equal? type (first token)))
      ;;(error (~a "expected token of type " type " but actual token was " token)))
      #f)
    (set! tokens (rest tokens))  ; update tokens: remove first token
    token))

(define (check type)
  ;;(display type)
  ;;(display tokens)
  (if (empty? tokens)
      #f
      (begin
        ;;(displayln type)
        ;;(displayln (first (first tokens)))
        (equal? type (first (first tokens))))
      )
  )

(define (atom-pending)
  ;;(displayln (check 'NAME))
  ;;(displayln (or (check 'NAME) (check 'STRING) (check 'INT) (check 'FLOAT)))
  (or (check 'NAME) (check 'STRING) (check 'INT) (check 'FLOAT))
  )
      
(define (invocation-pending)
  (check 'OPAREN)
  )

(define (invocation)
  (begin
    (consume 'OPAREN)
    (set! temp (exprlist))
    (consume 'CPAREN)
    (list 'invocation '(OPAREN #f) temp '(CPAREN #f))
    )
  )

(define (expr)
  ;;(displayln tokens)
  (if (or (equal? #t (atom-pending)) (equal? #t (invocation-pending)))
      (if (equal? #t (atom-pending))
          (if (or (equal? #t (check 'NAME)) (equal? #t (check 'STRING)))
              (begin
                (set! temp_tok (first tokens))
                (consume (first (first tokens)))
                (list 'expr (list 'atom temp_tok))
                )
              (begin
                (set! temp_tok (first tokens)) 
                (consume (first (first tokens)))
                (list 'expr (list 'atom (list 'number temp_tok)))
                )
              )
          (list 'expr (invocation))
          )
      #f
      )
  )
      
(define (optexprlist)
  (if (or (equal? #t (atom-pending)) (equal? #t (invocation-pending)))
      (list 'optExprList (exprlist))
      (list 'optExprList)
      )
  )

(define (exprlist)
  ;;(displayln tokens)
  (list 'exprList (expr) (optexprlist))

 )

(define (parse str)
  (set! tokens null)
  (set! temp null)
  (set! temp_tok null)
  (set! tokens (tokenize str))
  ;;(displayln tokens)
  (list 'program (exprlist))
  )
     