(cond-expand (guile (define add1 1+)
                    (define sub1 1-))
             (chicken))


(define (centered-pentagonal n)
  (/ (+ (* 5 (expt n 2))
        (* 5 n)
        2)
     2))


(define (double-square? n)
  (integer? (sqrt (/ n 2))))


(let loop ((i 5))
  (let ((candidate-n (sub1 (centered-pentagonal i))))
    (if (double-square? candidate-n)
        (begin
          (display candidate-n)
          (display " = 2 * ")
          (display (sqrt (/ candidate-n 2)))
          (display "^2")
          (newline))
        (loop (add1 i)))))
