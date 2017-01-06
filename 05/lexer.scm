;;; lexer.scm
(use matchable)
(define (read-until predicate #!optional (port (current-input-port)))
  (with-input-from-port port
    (lambda ()
      (let loop ([c (peek-char)]
		 [acc ""])
	(cond [(eof-object? c)
	       (read-char) acc]
	      [(predicate c) acc]
	      [else
	       (read-char)
	       (loop (peek-char) (conc acc c))])))))

(define (read-while predicate #!optional (port (current-input-port)))
  (read-until (complement predicate) port))

(define (read-number #!optional (port (current-input-port)))
  (let ([int (read-while char-numeric? port)])
    (cond [(char=? (peek-char port) #\.)
	   ;; consume dot
	   (read-char port)
	   (string-append int "." (read-number port))]
	  [else int])))

(define (read-var #!optional (port (current-input-port)))
  (conc (read-char port)
	(read-while (lambda (c)
		      (char-set-contains? char-set:letter+digit c))
		    port)))

(define (port-line #!optional (port (current-input-port)))
  (let-values (((line _) (port-position port)))
    line))
 
(define (port-column #!optional (port (current-input-port)))
  (let-values (((_ column) (port-position port)))
    column))

(define (lexer #!optional (port (current-input-port)))
  (with-input-from-port port
    (lambda ()
      (read-while char-whitespace?)
      (let ([location (make-source-location "*stdin*"
					    (port-line)
					    (port-column) -1 -1)]
	    [c (peek-char)])
	(match c
	  [#!eof (read-char) '*eoi*]
	  [(? char-numeric?)
	   (make-lexical-token 'NUMBER location (string->number (read-number)))]
	  [#\^ (read-char)
	   (make-lexical-token 'LAMBDA location #f)]
	  [#\. (read-char)
	   (make-lexical-token 'DOT location #f)]
	  [#\( (read-char)
	   (make-lexical-token 'LPAREN location #f)]
	  [#\) (read-char)
	   (make-lexical-token 'RPAREN location #f)]
	  [(? char-alphabetic?)
	   (make-lexical-token 'VAR location (string->symbol (read-var)))]
	  [else
	   (error "Lexer Error")])))))
