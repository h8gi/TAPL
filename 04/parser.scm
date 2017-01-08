(use prcc matchable)

(define parser
  (<@> (<and> (<*> (<s+>))
	      (<*_> term))
       second))

(define term
  (<or> (lazy const)
	(lazy succ)
	(lazy pred)
	(lazy iszero)
	(lazy ifs)))

(define (<sym> str)
  (<@> (<s> str) string->symbol))

(define bool
  (<or> (<sym> "true")
	(<sym> "false")))

(define zero
  (<@> (<s> "0") (constantly 'zero)))

(define const
  (<or> bool
	zero))

(define (<app> str)
  (<@> (<and_> (<sym> str)
	       (<c> #\()
	       term
	       (<c> #\)))
       (match-lambda
	[(name leftp val rightp)
[]	 (list name val)])))

(define succ
  (<app> "succ"))

(define pred
  (<@> (<app> "pred")
       (match-lambda
	['(pred zero) 'zero]
	[`(pred (succ ,nv)) nv])))

(define iszero
  (<@> (<app> "iszero")
       (match-lambda
	['(iszero zero) 'true]
	[`(iszero (succ ,nv1)) 'false])))

(define ifs
  (<@> (<and_> (<sym> "if") term
	       (<sym> "then") term
	       (<sym> "else") term)
       (match-lambda [`(if true then ,t2 else ,t3)
		      t2]
		     [`(if false then ,t2 else ,t3)
		      t3])))
