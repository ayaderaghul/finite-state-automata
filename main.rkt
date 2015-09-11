(require "auto.rkt"
         "fit.rkt"
         "match.rkt"
         "mutation.rkt"
         "scan.rkt"
         "tv.rkt")


;; generate population
(define (create-population N)
  (for/list
      ([n N])
    (generate-auto)))

(define A (create-population 100))



(define population-mean '())
(define demographic '())
;; evolve the population over cycles
;; N=100
(define (evolve population cycles speed mutation rounds-per-match)
  (let* ([N (length population)]
         [demo (scan-types population)]
         [round-results (match-population population rounds-per-match)]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            (* rounds-per-match N)))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [new-population
          (shuffle (append survivors successors))]
;         [mutators
;          (for ([m mutation])
;            (mutate (list-ref new-population (random N))))]
         )
    (set! population-mean
          (append population-mean (list average-payoff)))
    (set! demographic
          (append demographic (list demo)))
    (if (zero? cycles)
        population
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match))))
