(let ((x '#1=(#2=#1#)))
  (and (eq? x (car x))
       (eq? x (car (car x)))
       (eq? x (car (car (car x))))))
