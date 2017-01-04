(use matchable srfi-18)


(define (numerical? t)
  (match t
    ['zero #t]
    [`(succ ,t1) (numerical? t1)]
    [else #f]))

(define (val? t)
  (match t
    [(or 'true 'false) #t]
    [(? numerical?) #t]
    [else #f]))

(define (smallstep-eval1 t)
  (match t
    [`(if true ,t2 ,t3) t2]
    [`(if false ,t2 ,t3) t3]
    [`(if ,t1 ,t2 ,t3)
     (let ([t1d (smallstep-eval1 t1)])
       `(if ,t1d ,t2 ,t3))]
    [`(succ ,t1)
     (let ([t1d (smallstep-eval1 t1)])
       `(succ ,t1d))]
    ['(pred zero)
     'zero]
    [(and `(pred (succ ,nv1))
	  (? (lambda (pat)
	       (numerical? nv1))))
     nv1]
    [`(pred ,t1)
     (let ([t1d (smallstep-eval1 t1)])
       `(pred ,t1d))]
    ['(iszero zero) 'true]
    [(and `(iszero (succ ,nv1))
	  (? (lambda (pat)
	       (numerical? nv1))))
     'false]
    [`(iszero ,t1)
     (let ([t1d (smallstep-eval1 t1)])
       `(iszero ,t1d))]
    [else
     (raise 'no-rule)]))

(define (smallstep-eval t)
  (handle-exceptions exn
      (cond [(eq? exn 'no-rule) t]
	    [else (error exn)])
    (let ([tid (smallstep-eval1 t)])
      (smallstep-eval tid))))


(define (bigstep-eval t)
  (match t
    [`(if ,t1 ,t2 ,t3)
     (match (bigstep-eval t1)
       ['true (bigstep-eval t2)]
       ['false (bigstep-eval t3)])]
    [`(succ ,t1)
     `(succ ,(bigstep-eval t1))]
    [`(pred ,t1)
     (match (bigstep-eval t1)
       ['zero 'zero]
       [`(succ ,nv) nv])]
    [`(iszero ,t1)
     (match (bigstep-eval t1)
       ['zero 'true]
       [`(succ ,nv) 'false])]
    [else t]))

