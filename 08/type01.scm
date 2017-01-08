(use matchable)
(define (normalize-expr expr)
  (match expr
    ;; lambda    
    [('^ (var) body)
     `(^ (,var) ,(normalize-expr body))]
    [('^ (var . vars) body)
     `(^ (,var)
	 ,(normalize-expr `(^ ,vars ,body)))]
    [('^ . _) (error "lambda error" expr)]
    ;; apply
    [(e1 e2)
     `(APPLY ,(normalize-expr e1)
	     ,(normalize-expr e2))]
    [(e1 e2 . rest)
     `(APPLY ,(normalize-expr `(,e1 ,e2))
	     ,(normalize-expr rest))]
    [(e)
     `(APPLY ,(normalize-expr e) _unit_)]
    [else expr]))

