(define x '(a #{x}="\\t" #{x}))
(cons 'a (car (cdr x)))
