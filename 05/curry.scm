(define-syntax ^
  (syntax-rules ()
    [(_ () body ...) (lambda () body ...)]
    [(_ (arg) body ...) (lambda (arg)  body ...)]
    [(_ (arg args ...) body ...)
     (lambda (arg . rest)
       (let ([next (^ (args ...) body ...)])
         (if (null? rest)
             next
             (apply next rest))))]))

(define tru (^ (t f) t))
(define fls (^ (t f) f))
(define test (^ (l m n) ((l m) n)))

(define I (^ (x) x))
(define K (^ (x y) x))
(define S (^ (x y z) (x z (y z))))
(define Y (S (K (S I I)) (S (S (K S) K) (K (S I I)))))


