(define complex-cycle #0='(1 #1# 1 #1=(2 . #1#) . #0))
(define x (car (car (cdr (cdr (cdr complex-cycle)))))) ;2 
(define y (car (cdr (cdr complex-cycle))))
