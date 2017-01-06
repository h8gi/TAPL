;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval.scm
;;; evaluator of lambda calculus
;;; implement de bruijn index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use matchable)
(include "lambda-parser.yy.scm")
(include "lexer.scm")

(define (parse-string str)
  (with-input-from-string str
    (lambda ()
      (lambda-parser lexer (lambda x x)))))

(define (eval-lambda expr)
  (match expr
    [('LAMBDA . _) expr]
    [('APPLY t1 t2)
     (match-let ([('LAMBDA (x) body) (eval-lambda t1)]
                 [v2 (eval-lambda t2)])
       (lambda-substitute x v2 body))]
    [else (error "EVAL LAMBDA" expr)]))

(define (lambda-substitute x v body)
  (list x v body))


(define (difference a b)
  (fold (lambda (x acc)
          (delete x acc))
        a b))

(define (union a b)
  (fold (lambda (x acc)
          (if (member x acc) acc
              (cons x acc)))
        a b))

(define (intersection a b)
  (fold (lambda (x acc)
          (if (member x a) (cons x acc)
              acc))
        '() b))

(define (free-variables expr)
  (match expr    
    [('LAMBDA (var) body)
     (difference (free-variables body) (list var))]
    [('APPLY t1 t2)
     (union (free-variables t1)
            (free-variables t2))]
    [var (list var)]))

