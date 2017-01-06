;;; parser.scm
(use lalr matchable)

(define lambda-parser
  (lalr-parser
   ;; --- Options
   (output: lambda-parser "lambda-parser.yy.scm")
   ;; output the LALR table to lambda.out
   (out-table: "lambda-parser.out")
   ;; --- token definitions
   (NUMBER LAMBDA DOT LPAREN RPAREN VAR
	   (left: + -)
	   (left: * /))
   ;; --- rules
   (expr (expr term) : (list 'APPLY $1 $2)
	 (term)	     : $1)
   (term (VAR)	  : (list 'VAR $1)
	 (NUMBER) : (list 'NUMBER $1)
	 (LAMBDA VAR DOT expr) : (list 'LAMBDA (list $2) $4)
	 (LPAREN expr RPAREN)  : $2)))
