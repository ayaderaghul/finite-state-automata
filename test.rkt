#lang racket

(require "auto.rkt")

(provide create-test-population)

;; create test population

(define (create-test-population l m h)
  (shuffle
   (append
    (for/list
        ([n l])
      (generate-lows))
    (for/list
        ([n m])
      (generate-mediums))
    (for/list
        ([n h])
      (generate-highs)))))

(define (generate-lows)
  (automaton 0
             (list (state 0 (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3)))))))

(define (generate-mediums)
  (automaton 0
             (list (state 1 (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3)))))))
(define (generate-highs)
  (automaton 0
             (list (state 2 (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3))))
                   (state (random 3) (list (action 0 (random 3))
                                           (action 1 (random 3))
                                           (action 2 (random 3)))))))
