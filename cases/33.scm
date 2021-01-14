(define complex-cycle #{x}='(1 #{y} 1 #{y}=(2 . #{z}) . #{z}=#{x}))
(define x (car (car (cdr (cdr (cdr complex-cycle)))))) ;2 
(define y (car (car (cdr complex-cycle))))
