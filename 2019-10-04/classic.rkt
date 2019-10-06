#lang racket

(require math plot/no-gui)


(define (general-binomial n k)
  (/ (gamma (add1 n))
     (* (gamma (add1 k))
        (gamma (add1 (- n k))))))

(define (bday-prob combo-size population)
  (- 1
     (expt (- 1
              (expt 1/365
                    (sub1 combo-size)))
           (general-binomial population combo-size))))


(plot-width 1000)
(plot-height 1000)
(plot-font-size 24)

(plot-file
  (list
    (hrule 1/2
           #:style 'long-dash
           #:color "dim gray"
           #:width 2)
    (function (curry bday-prob 3)
              3 200
              #:width 3))
  "birthday_probability.png"
  #:x-min 0
  #:x-max 200
  #:y-min 0
  #:y-max 1
  #:x-label "Population"
  #:y-label "Probability that three people share a birthday")
