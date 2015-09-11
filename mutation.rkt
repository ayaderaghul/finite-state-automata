#lang racket
(require "auto.rkt")
(provide mutate)

;; MUTATION
(define (mutate an-auto)
  (let ([r (random 21)])
    (cond
     [(< r 5) (set-state-name!
               (list-ref
                (automaton-states an-auto)
                r)
               (random 3))]
     [(< r 10) (set-action-result!
                (first
                 (state-actions
                  (list-ref
                   (automaton-states an-auto)
                   (- r 5))))
                (random 3))]
     [(< r 15) (set-action-result!
                (second
                 (state-actions
                  (list-ref
                   (automaton-states an-auto)
                   (- r 10))))
                (random 3))]
     [(< r 20) (set-action-result!
                (third
                 (state-actions
                  (list-ref
                   (automaton-states an-auto)
                   (- r 15))))
                (random 3))]
     [(= r 21) (set-automaton-current-state!
                an-auto (random 3))])))

#|
  (let ([r (random 3)])
    (cond [(= r 0) (set-automaton-current-state! an-auto (random 3))]
          [(= r 1) (let ([r2 (random 5)])
                     (set-state-name!
                      (list-ref
                       (automaton-states an-auto)
                       r2)
                      (random 3)))]
          [(= r 2) (let ([r3 (random 5)]
                         [r4 (random 3)])
                     (set-action-result!
                      (list-ref
                       (state-actions
                        (list-ref
                         (automaton-states an-auto)
                         r3))
                       r4)
                      (random 3)))])))
|#



