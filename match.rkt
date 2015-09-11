#lang racket
(require "auto.rkt")
(provide match-claims
	match-pair
	match-population)


(define (match-claims claims)
  (if (<= (apply + claims) 2)
      (map convert-payoff claims)
      (list 0 0)))

(define (convert-payoff x)
  (cond [(= x -1) 0]
        [(= x 0) 2]
        [(= x 1) 5]
        [(= x 2) 8]))

(define (match-pair* auto1 auto2 results previous-claims countdown)
  (if (zero? countdown)
      results
      (let ([reaction1 (react (last previous-claims) auto1)]
            [reaction2 (react (car previous-claims) auto2)])
        (match-pair* (begin (update auto1 reaction1) auto1)
                     (begin (update auto2 reaction2) auto2)
                     (append results (list
                                      (match-claims previous-claims)))
                     (list reaction1 reaction2)
                     (sub1 countdown)))))

;; current claim
(define (current-claim an-auto)
  (state-name
   (list-ref (automaton-states an-auto)
             (automaton-current-state an-auto))))


;; match a pair of automaton for n rounds
;; return a list of round results
(define (match-pair automaton-pair rounds-per-match)
  (match-pair* (car automaton-pair)
               (last automaton-pair)
               '()
               (map current-claim automaton-pair)
               rounds-per-match))



;; in each match, take mean of round results for each automaton
;; returns a pair of means
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))


(define (match-population population rounds-per-match)
  (for/list ([i (/ (length population)
                   2)])
    (take-sums
     (match-pair (list
                  (list-ref population (* 2 i))
                  (list-ref population (add1 (* 2 i))))
                 rounds-per-match))))
