(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (main)
  (factorial 10))
