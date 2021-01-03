(define (fold-left f init seq) 
   (if (null? seq) 
       init 
       (fold-left f 
                  (f (car seq) init) 
                  (cdr seq)))) 



