;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eval.scm
;;; evaluator of lambda calculus
;;; implement de bruijn index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;; set functions 
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
    [('LAMBDA (x) body)
     (difference (free-variables body) (list x))]
    [('APPLY t1 t2)
     (union (free-variables t1)
            (free-variables t2))]
    [('VAR x) (list x)]))

(define (index-of x ctxt)
  (list-index (cut eq? <> x) ctxt))

;;; 名無し項への変換
(define (remove-names expr #!optional (ctxt '()))
  (match expr
    [('VAR x) (list 'VAR (or (index-of x ctxt)
                             (error "Can't remove name" x)))]
    [('LAMBDA (x) body)
     `(LAMBDA ,(remove-names body (cons x ctxt)))]
    [('APPLY t1 t2)
     `(APPLY ,(remove-names t1 ctxt) ,(remove-names t2 ctxt))]))

(define (restore-names expr #!optional (ctxt '()))
  (match expr
    [('VAR index) (list 'VAR (or (list-ref ctxt index)
                                 (error "Can't restore name" expr index)))]
    [('LAMBDA body)
     (let ([var (gensym)])
       `(LAMBDA (,var) ,(restore-names body (cons var ctxt))))]
    [('APPLY t1 t2)
     `(APPLY ,(restore-names t1 ctxt)
             ,(restore-names t2 ctxt))]))

;;; 名無し項のシフト
(define (shift-index expr d #!optional (c 0))
  (match expr
    [('VAR index) (list 'VAR (if (< index c) index (+ index d)))]
    [('LAMBDA body)
     `(LAMBDA ,(shift-index body d (+ c 1)))]
    [('APPLY t1 t2)
     `(APPLY ,(shift-index t1 d c)
             ,(shift-index t2 d c))]))

(define (lambda-substitute expr j s)
  (match expr
    [('VAR index) (if (= index j) s (list 'VAR index))]
    [('LAMBDA body)
     `(LAMBDA ,(lambda-substitute body (+ j 1)
                                  (shift-index s 1)))]
    [('APPLY t1 t2)
     `(APPLY ,(lambda-substitute t1 j s)
             ,(lambda-substitute t2 j s))]))

(define (subst-test str x v-str ctxt)
  (let ([x-i (index-of x ctxt)])
    (restore-names (lambda-substitute
                    (remove-names (parse-string str) ctxt)
                    x-i
                    (remove-names (parse-string v-str) ctxt))
                   ctxt)))

(subst-test "b ^x.^y.b" 'b "a" '(b a))
(subst-test "b ^x.b" 'b "a ^z.a" '(b a))
(subst-test "^ b . b a" 'b "a" '(b a))
(subst-test "^a. b a" 'b "a" '(b a))
