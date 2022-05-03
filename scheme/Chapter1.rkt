#lang racket
(define (identity x) x)
(identity 42)
(identity "hello world")

(define (compose f g) (lambda (x) (g (f x))))
((compose (lambda (x) (string-length x))
          (lambda (x) (/ x 2)))
 "hello")

; TODO: change to RackUnit
(define (test-compose f x)
  (let ((fid (compose f identity))
        (idf (compose identity f))
        (fx (f x)))
    (and (equal? fx (fid x))
         (equal? fx (idf x)))))
(test-compose (lambda (x) (string-length x)) "hello world")
(test-compose (lambda (x) (/ x 2)) 1)
(test-compose (lambda (x) (/ x 2)) (/ 2 3))