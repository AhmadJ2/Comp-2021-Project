;;;; our style
(define days '#0=(sun mon tue wed thu fri sat . #0#))
(define (foo n curr_week) 
        (if (zero? n)
            (car curr_week)
            (foo (- n 1) (cdr curr_week))))
(foo 24 days);wed
