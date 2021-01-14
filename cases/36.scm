;;;; our style
(define days '#{x}=(sun mon tue wed thu fri sat . #{x}))
(define (foo n curr_week) 
        (if (zero? n)
            (car curr_week)
            (foo (- n 1) (cdr curr_week))))
(foo 24 days);wed
