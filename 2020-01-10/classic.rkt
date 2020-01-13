#lang racket
(require plot)

(define (two-digit-words n)
  (cdr (assoc n
    '((0 . "")
      (1 . "one")
      (2 . "two")
      (3 . "three")
      (4 . "four")
      (5 . "five")
      (6 . "six")
      (7 . "seven")
      (8 . "eight")
      (9 . "nine")
      (10 . "ten")
      (11 . "eleven")
      (12 . "twelve")
      (13 . "thirteen")
      (14 . "fourteen")
      (15 . "fifteen")
      (16 . "sixteen")
      (17 . "seventeen")
      (18 . "eighteen")
      (19 . "nineteen")
      (20 . "twenty")
      (30 . "thirty")
      (40 . "forty")
      (50 . "fifty")
      (60 . "sixty")
      (70 . "seventy")
      (80 . "eighty")
      (90 . "ninety")))))

(define +powers-of-10+
  '((12 . "trillion")
    (9 . "billion")
    (6 . "million")
    (3 . "thousand")))

(define (digits n place (ending-place (add1 place)))
  (quotient (modulo n (expt 10 ending-place))
            (expt 10 place)))

(define (two-digits-as-words n)
  (let ((last-digits (digits n 0 2)))
    (if (<= 10 last-digits 19)
        (two-digit-words last-digits)
        (string-join
          (list (two-digit-words (* 10 (digits n 1)))
                (two-digit-words (digits n 0)))
          " "))))

(define (three-digits-as-words n)
  (let ((hundreds (if (>= n 100)
                      (string-append (two-digit-words (digits n 2)) " hundred")
                      "")))
    (string-join (list hundreds
                       (two-digits-as-words n))
                 " ")))

(define (digits-as-words n (powers-of-10 +powers-of-10+))
  (if (null? powers-of-10)
      (three-digits-as-words (digits n 0 3))
      (let ((place (caar powers-of-10))
            (rest-of-words (digits-as-words n (cdr powers-of-10))))
        (if (>= n (expt 10 place))
            (string-join
              (list (three-digits-as-words (digits n place (+ place 3)))
                    (cdar powers-of-10)
                    rest-of-words)
              " ")
            rest-of-words))))

(define +alphabet+ (string->list " abcdefghijklmnopqrstuvwxyz"))
(define (char-val character (remaining-letters +alphabet+) (value 0))
  (if (equal? character (car remaining-letters))
      value
      (char-val character (cdr remaining-letters) (add1 value))))

(define (gematria a-string)
  (apply + (map char-val (string->list a-string))))

(define (test up-to)
  (apply max
         (filter (lambda (n) (< n (gematria (digits-as-words n))))
                 (range up-to))))

(display (test 1000000))
(newline)

(plot-width 1200)
(plot-height 900)
(plot-file
  (list
    (points (map (lambda (x) (list x (gematria (digits-as-words x))))
                 (range 1000))
            #:sym 'dot
            #:size 20)
    (function (lambda (x) x))
    )
  "number_word_values.png"
  #:x-label "Number"
  #:y-label "Gematria value"
  #:y-max 500
  )
