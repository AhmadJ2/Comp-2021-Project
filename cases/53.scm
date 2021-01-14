(let ((x '#{foo}=(#{goo}=#{foo})))
  (and (eq? x (car x))
       (eq? x (car (car x)))
       (eq? x (car (car (car x))))))
