#lang racket
(define (identity x) x)
(identity 42)
(identity "hello world")

(define (compose f g) (lambda (x) (g (f x))))
((compose (lambda (x) (string-length x))
          (lambda (x) (/ x 2)))
 "hello")
