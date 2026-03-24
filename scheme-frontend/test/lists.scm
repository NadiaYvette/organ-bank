(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (filter pred xs)
  (cond
    ((null? xs) '())
    ((pred (car xs))
     (cons (car xs) (filter pred (cdr xs))))
    (else (filter pred (cdr xs)))))

(define (fold-left f init xs)
  (if (null? xs)
      init
      (fold-left f (f init (car xs)) (cdr xs))))

(let ((nums '(1 2 3 4 5)))
  (fold-left + 0 (filter (lambda (x) (> x 2)) nums)))
