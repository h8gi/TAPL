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
    [('VAR x) (list x)]
    [else (error "free variable?")]))

(define (index-of x ctxt)
  (list-index (cut eq? <> x) ctxt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; De Bruijn Index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 名無し項への変換
(define (remove-names expr #!optional (ctxt '()))
  (match expr
    [('VAR x) (list 'VAR (or (index-of x ctxt)
                             (error "Can't remove name" x)))]
    [('LAMBDA (x) body)
     `(LAMBDA (,x) ,(remove-names body (cons x ctxt)))]
    [('APPLY t1 t2)
     `(APPLY ,(remove-names t1 ctxt) ,(remove-names t2 ctxt))]
    [else (error "Can't remove name" expr)]))

(define (restore-names expr #!optional (ctxt '()))
  (define (inner-gensym sym ctxt)
    (define (%inner-gensym sym counter ctxt)
      (let ([new-sym (string->symbol (conc sym "_" counter))])
	(if (member new-sym ctxt)
	    (%inner-gensym sym (+ counter 1) ctxt)
	    new-sym)))
    (if (member sym ctxt)
	(%inner-gensym sym 0 ctxt)
	sym))
  (define (inner expr ctxt)
    (match expr
      [('VAR index) (list 'VAR (or (list-ref ctxt index)
				   (error "Can't restore name" expr index)))]
      [('LAMBDA (x) body)
       (let ([var (inner-gensym x ctxt)])
	 `(LAMBDA (,var) ,(inner body (cons var ctxt))))]
      [('APPLY t1 t2)
       `(APPLY ,(inner t1 ctxt)
	       ,(inner t2 ctxt))]
      [else (error "Can't restore name" expr)]))
  (inner expr ctxt))

;;; 名無し項のシフト
(define (shift-index expr d #!optional (c 0))
  (match expr
    [('VAR index) (list 'VAR (if (< index c) index (+ index d)))]
    [('LAMBDA (x) body)
     `(LAMBDA (,x) ,(shift-index body d (+ c 1)))]
    [('APPLY t1 t2)
     `(APPLY ,(shift-index t1 d c)
             ,(shift-index t2 d c))]
    [else (error "Index shift" expr)]))

(define (lambda-substitute expr j s)
  (match expr
    [('VAR index) (if (= index j) s (list 'VAR index))]
    [('LAMBDA (x) body)
     `(LAMBDA (,x) ,(lambda-substitute body (+ j 1)
				     (shift-index s 1)))]
    [('APPLY t1 t2)
     `(APPLY ,(lambda-substitute t1 j s)
             ,(lambda-substitute t2 j s))]
    [else (error "Substitute" expr)]))

(define (subst-test str x v-str ctxt)
  (let ([x-i (index-of x ctxt)])
    (restore-names (lambda-substitute
                    (remove-names (parse-string str) ctxt)
                    x-i
                    (remove-names (parse-string v-str) ctxt))
                   ctxt)))

(define (eval-lambda expr)
  (define (inner expr)
    (match expr
      [('APPLY t1 t2)
       (match-let* ([('LAMBDA (x) body) (inner t1)]
		    [v2 (inner t2)]
		    [v2-sft (shift-index v2 1)]
		    [body-sbst (lambda-substitute body 0 v2-sft)]
		    [body-sft (shift-index body-sbst -1)])
	 (inner body-sft))]
      [else expr]))
  (restore-names (inner (remove-names expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (reshape-lambda expr)
  (match expr
    [('APPLY t1 t2) (list (reshape-lambda t1)
			  (reshape-lambda t2))]
    [('LAMBDA (var) body)
     `(^ (,var) ,(reshape-lambda body))]
    [('VAR x) x]))

(define (repl-error exn)
  (printf "Error: ~A\n" ((condition-property-accessor 'exn 'message) exn)))

(define (lambda-repl)
  (define (inner)
    (display "> ")
    (handle-exceptions exn
	(begin (repl-error exn)
	       (inner))
      (let ([expr (parse-string (read-line))])
	(match expr
	  [('VAR 'quit) (pp "Good bye!")]
	  [else
	   (display "INPUT : ") (pp (reshape-lambda expr))
	   (display "OUTPUT: ") (pp (reshape-lambda (eval-lambda expr)))
	   (inner)]))))
  (inner))
