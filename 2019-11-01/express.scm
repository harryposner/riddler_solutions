;;; Works with Chicken and MIT Scheme.
;;; To use with Guile, run "guile --use-srfi=9 express.scm"


(define +n-snails+ 6)
(define +radius+ 10)
(define +ds+ 1/1000000)
(define pi (angle -1))


(define-record-type point
                    (make-point x y)
                    point?
                    (x point-x)
                    (y point-y))

(define-record-type snail
                    (make-snail coords target)
                    snail?
                    (coords snail-coords snail-coords-set!)
                    (target snail-target snail-target-set!))


(define (complex->point complex-number)
  (make-point (real-part complex-number) (imag-part complex-number)))

(define (point->complex a-point)
  (make-rectangular (point-x a-point) (point-y a-point)))

;;; The built-in complex arithmetic is the easiest way of which I'm aware to
;;; manipulate points in a plane.
(define (move-toward location target)
  (let ((complex-location (point->complex location))
        (complex-target (point->complex target)))
    (complex->point
      (+ complex-location
         (make-polar +ds+ (angle (- complex-target complex-location)))))))

(define (move-snail-toward! snail target)
  ;;; I'm not using snail-target because I want the snails to move
  ;;; simultaneously.  If the target snail has already moved in the current
  ;;; step, the moving snail should still move to where the target was at the
  ;;; beginning of the step.
  (snail-coords-set!
    snail
    (move-toward (snail-coords snail) target)))

(define (distance start end)
  (sqrt (+ (expt (- (point-x end) (point-x start)) 2)
           (expt (- (point-y end) (point-y start)) 2))))

(define (snail-at-target? snail)
  (< (distance (snail-coords snail)
               (snail-coords (snail-target snail)))
     (* 2 +ds+)))


(define starting-coords
  (map complex->point
       ;;; Define a complex unit with an angle of 1/n-snails rotations.  We can
       ;;; use that to rotate one initial position n-1 times to get the other
       ;;; starting positions.
       (let ((rotate-complex (make-polar 1 (/ (* 2 pi) +n-snails+))))
         (let loop ((remaining +n-snails+)
                    (points '()))
           (cond
             ((zero? remaining) points)
             ((null? points) (loop (- remaining 1)
                                   (cons (make-polar +radius+ 0) points)))
             (else (loop (- remaining 1)
                         (cons (* rotate-complex (car points))
                               points))))))))

(define (make-all-snails)
  (let ((snails (map (lambda (point) (make-snail point #f)) starting-coords)))
    (let loop ((remaining snails))
      ;;; By the construction of starting-coords, the next snail in the list is
      ;;; the current snail's clockwise neighbor.
      (if (null? (cdr remaining))
        (begin
          (snail-target-set! (car remaining) (car snails))
          snails)
        (begin
          (snail-target-set! (car remaining) (cadr remaining))
          (loop (cdr remaining)))))))

(define (move-all-snails! snails)
  (let ((targets (map (lambda (x) (snail-coords (snail-target x))) snails)))
    (for-each
      (lambda (snail target)
        (move-snail-toward! snail target))
      snails targets)))

(define (every? predicate lst)
  (cond
    ((null? lst) #t)
    ((predicate (car lst)) (every? predicate (cdr lst)))
    (else #f)))

(define (steps-to-center)
  (let ((snails (make-all-snails)))
    (let loop ((n-steps 0))
      (if (every? snail-at-target? snails)
        n-steps
        (begin
          (move-all-snails! snails)
          (loop (+ n-steps 1)))))))


(let ((n-steps (steps-to-center)))
  (display n-steps)
  (display " steps to center with step size ")
  (display +ds+)
  (newline)
  (display "Total distance is " )
  (display (exact->inexact (* n-steps +ds+)))
  (display " meters")
  (newline))

;;; 20000021 steps to center with step size 1/1000000
;;; Total distance is 20.000021 meters
