(define f2 (lambda (x . y) ( + x (apply + y))) )
(f2 #;#10r10 #11r10 #12r10  #13r10 #14r10  #15r10 1.0 1 )
