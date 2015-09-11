#lang racket
(require "auto.rkt")
(provide scan-init
	scan
	scan-types)

;; SCAN
(define (scan-init population)
  (foldl
   (lambda (au h)
     (hash-update h
                  (state-name (list-ref
                               (automaton-states au)
                               (automaton-current-state au)))
                  add1 0))
   (hash)
   population))
(define (scan population)
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   population))
(define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))
(define (scan-types population)
  (let ([types (scan-init population)])
    (list
     (hash-ref* types 0)
     (hash-ref* types 1))))
