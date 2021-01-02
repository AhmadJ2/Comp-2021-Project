;(map (lambda (x) x) '(1 1 3 4))
(let ((consf cons) (applyf apply))  (cons (apply + 1 '()) (apply + '())))