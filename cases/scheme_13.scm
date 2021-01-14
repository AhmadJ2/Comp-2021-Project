(define complex-cycle '#0=(1 #1# 1 #1=(2 . #2#) . #2=#1#))
(define x (car (car (cdr (cdr (cdr complex-cycle)))))) ;2 
(define y (car (car (cdr complex-cycle)))) ;2
(* x y) 
