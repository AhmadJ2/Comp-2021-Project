(+ 2 2 2)
(fold-left + 0 '(1 2 3 4 5))

(define NN (lambda (y) (letrec ((N (lambda (x) (if (= x 0) 1 (N (- x 1))))))  (N y) )))
(NN 1000000)