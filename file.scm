(+ 1 1)
(map (lambda (x) (- x x)) '(1 2 3 4))
(map (lambda (x) (+ x x)) '(1 2 3 4))
(map (lambda (x) (+ (apply + `(,x)) (apply + `(,x)))) '(1 2 3 4))
(map (lambda (x) (fold-left + 0 `(,x ,x ,x))) '(1 2 3 4))
(define N (lambda (x) (if (= x 0) 1 (* x (N (- x 1))))))
(N 5)



