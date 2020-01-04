(cond-expand
  (chicken (import srfi-1
                   (chicken sort)))
  (guile (import (srfi srfi-1))))

(define +check-up-to+ 300)

(define (surface-area x y z)
  (* 2 (+ (* x y)
          (* y z)
          (* x z))))

(define (volume x y z)
  (* x y z))

(define (solution? x y z)
  (= (surface-area x y z)
     (volume x y z)))

(define (cartesian-product . lists)
  (if (null? lists)
      '(())
      (apply append
             (map (lambda (tuple)
                    (map (lambda (element) (cons element tuple))
                         (car lists)))
                  (apply cartesian-product (cdr lists))))))

(define (repeat item count)
  (if (zero? count)
      '()
      (cons item (repeat item (- count 1)))))

(define (combos-with-replacement lst r)
  (filter (lambda (tuple) (sorted? tuple <))
          (apply cartesian-product (repeat lst r))))


(for-each
  (lambda (coords) (if (apply solution? coords)
                       (begin (display coords)
                              (newline))))
  (combos-with-replacement (iota +check-up-to+ 1) 3))
