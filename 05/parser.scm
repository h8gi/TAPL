(use prcc)

(define term
  (<or>
   (<#> (<and_> (<c> #\()
		(lazy term)
		(<c> #\)))
	1)
   (lazy app)
   (lazy var)
   (lazy lam)))


(define var
  (<@> (<r> "[a-zA-Z][a-zA-Z0-9]*")
       string->symbol))

(define lam
  (<and_> (<s> "^")
	  var
	  (<s> ".")
	  term))

(define app
  (<and_> term
	  term))



