(include "lambda.yy.scm")
(include "lexer.scm")

(define (parse-string str)
  (with-input-from-string str
    (lambda ()
      (lambda-parser lexer identity))))

