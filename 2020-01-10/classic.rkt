#lang racket
(require plot)

(define (positive-num->words n (power-names +power-names-table+))
  (if (null? power-names)
      (three-digits->words n)
      (let ((relevant-digits (digits n
                                     (power power-names)
                                     (+ 3 (power power-names)))))
        (if (not (zero? relevant-digits))
            (append (three-digits->words relevant-digits)
                    (name power-names)
                    (positive-num->words n (rest power-names)))
            (positive-num->words n (rest power-names))))))

(define (three-digits->words n)
  (cond
    ((>= n 100) (append (two-digit-word-ref (digits n 2))
                        '(hundred)
                        (three-digits->words (digits n 0 2))))
    ((<= 10 n 19) (two-digit-word-ref n))
    ((zero? n) '())
    (else (append (two-digit-word-ref (* 10 (digits n 1)))
                  (two-digit-word-ref (digits n 0))))))

(define (digits n place (ending-place (add1 place)))
  (quotient (modulo n (expt 10 ending-place))
            (expt 10 place)))

(define +power-names-table+
  '((12 . (trillion))
    (9 . (billion))
    (6 . (million))
    (3 . (thousand))))
(define (power power-names)
  (caar power-names))
(define (name power-names)
  (cdar power-names))

(define +two-digit-word-table+
  '((0 . ())
    (1 . (one))
    (2 . (two))
    (3 . (three))
    (4 . (four))
    (5 . (five))
    (6 . (six))
    (7 . (seven))
    (8 . (eight))
    (9 . (nine))
    (10 . (ten))
    (11 . (eleven))
    (12 . (twelve))
    (13 . (thirteen))
    (14 . (fourteen))
    (15 . (fifteen))
    (16 . (sixteen))
    (17 . (seventeen))
    (18 . (eighteen))
    (19 . (nineteen))
    (20 . (twenty))
    (30 . (thirty))
    (40 . (forty))
    (50 . (fifty))
    (60 . (sixty))
    (70 . (seventy))
    (80 . (eighty))
    (90 . (ninety))))

(define (two-digit-word-ref n)
  (cdr (assoc n +two-digit-word-table+)))


(define +alphabet+ (string->list " abcdefghijklmnopqrstuvwxyz"))
(define (char-val character (remaining-letters +alphabet+) (value 0))
  (if (equal? character (car remaining-letters))
      value
      (char-val character (cdr remaining-letters) (add1 value))))

(define (gematria . symbols)
  (apply + (map char-val
                (string->list (string-join (map symbol->string symbols) " ")))))

(define (test up-to)
  (apply max
         (filter (lambda (n) (< n (apply gematria (positive-num->words n))))
                 (range up-to))))


(display (test 1000000))
(newline)

(plot-width 1200)
(plot-height 900)
(plot-file
  (list
    (points (map (lambda (x) (list x (apply gematria (positive-num->words x))))
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
