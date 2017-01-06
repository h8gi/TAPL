;;; parser.scm
(use lalr matchable)

(define lambda-parser
  (lalr-parser
   ;; --- Options
   (output: lambda-parser "lambda.yy.scm")
   ;; output the LALR table to lambda.out
   (out-table: "lambda.out")
   ;; --- token definitions
   (NUMBER LAMBDA DOT LPAREN RPAREN VAR)
   ;; --- rules
   (expr (expr term) : (list 'APPLY $1 $2)         
	 (term)	     : $1)
   (term (VAR)	  : $1
	 (NUMBER) : $1
	 (LAMBDA VAR DOT expr) : (list 'LAMBDA (list $2) $4)
	 (LPAREN expr RPAREN)  : $2)))
