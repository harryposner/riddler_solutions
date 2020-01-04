(cond-expand
  (chicken (import srfi-1
                   (chicken random))
           (define random-integer pseudo-random-integer))
  (guile (import (srfi srfi-1)
                 (srfi srfi-27))))


(define +test-range+ (iota 3 12))
(define +n-tracks+ 100)
(define +fav-track+ 42)
(define +n-sims+ 10000000)


(define (next-track current-track)
  (+ 1 (modulo current-track
               +n-tracks+)))

(define (random-track)
  (+ 1 (random-integer +n-tracks+)))

(define (steps-to-fav current-track)
  (modulo (- +fav-track+ current-track)
          +n-tracks+))

(define (test-strategy strategy)
  (let loop ((current-track (random-track))
             (n-button-presses 0))
    (if (= current-track +fav-track+)
        n-button-presses
        (loop (strategy current-track) (+ 1 n-button-presses)))))

(define (make-strategy threshold)
  (lambda (current-track)
    (if (<= (steps-to-fav current-track) threshold)
        (next-track current-track)
        (random-track))))

(define (avg-button-presses strategy)
  (let loop ((acc 0)
             (remaining +n-sims+))
    (if (zero? remaining)
        acc
        (loop (+ acc (test-strategy strategy))
              (- remaining 1)))))

(define (test-thresholds thresholds)
  (map avg-button-presses
       (map make-strategy
            thresholds)))

(for-each (lambda (result) (display result) (newline))
          (map cons
               (test-thresholds +test-range+)
               +test-range+))
