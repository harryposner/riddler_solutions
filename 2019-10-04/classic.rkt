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


(plot-width 800)
(plot-height 800)
(plot-font-size 14)
(plot-file
  (list
    (hrule 1/2
           #:style 'long-dash
           #:color "dark gray")
    (function (curry bday-prob 3)
              #:width 1.5
              3 200))
  "birthday_probability.png"
  #:x-min 0
  #:x-max 200
  #:y-min 0
  #:y-max 1
  #:x-label "Population"
  #:y-label "Probability that three people share a birthday")
