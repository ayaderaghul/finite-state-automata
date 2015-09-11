(require plot)
(plot-new-window? #t)

;; AUTOMATON
(struct action (event result) #:transparent #:mutable)
; a transition rule: an event and the result state
(struct state (name actions) #:transparent #:mutable)
; a state: name and many transition rules
(struct automaton (current-state states) #:transparent #:mutable)
; the machine itself: current state + states

;; when an event happens, the right action needs to be chosen
(define (this-action? an-event an-action)
  (equal? an-event (action-event an-action)))
; in a state, there are many actions, filter out the right action
; with the given event
(define (filter-action an-event actions)
  (filter
   (lambda (an-action)
     (this-action? an-event an-action))
   actions))
; after the right action has been chosen,
; extract the result of that action
(define (act an-event actions)
  (let ([result (filter-action an-event actions)])
    (if (null? result)
        null
        (action-result (car result)))))

; given a name, the right state needs to be found
(define (filter-state a-posn states)
  (list-ref states a-posn))
; an event happens, how the automaton will react?
(define (react an-event an-auto)
  (let ([result-state (filter-state (automaton-current-state an-auto)
                                    (automaton-states an-auto))])
    (if (null? result-state)
        an-auto
        (act an-event
             (state-actions
              result-state)))))
; update the state of the auto (new auto created)
(define (update old-auto new-state)
  (set-automaton-current-state! old-auto new-state))

(define accommodator
  (automaton 1
             (list (state 0 (list (action 0 2)
                                  (action 1 1)
                                  (action 2 0)))
                   (state 1 (list (action 0 2)
                                    (action 1 1)
                                    (action 2 0)))
                   (state 2 (list (action 0 2)
                                  (action 1 1)
                                  (action 2 0)))
                   (state 0 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2)))
                   (state 0 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2))))))
(define all-highs
  (automaton 2
             (list (state 2 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2)))
                   (state 2 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2)))
                   (state 2 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2)))
                   (state 2 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2)))
                   (state 2 (list (action 0 2)
                                  (action 1 2)
                                  (action 2 2)))
                   )))

; generate random automaton (random current state, random result-state
; after each event
(define (generate-auto)
  (automaton (random 3)
             (list (state (random 3) (list (action 0 (random 3))
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

;; generate population
(define A
  (for/list
      ([n 100])
    (generate-auto)))


;; in each match, take mean of round results for each automaton
;; returns a pair of means
(define (take-sums round-results)
  (map (lambda (f) (apply +  (map f round-results)))
       (list first second)))

(define (take-discounts delta round-results)
  (map (lambda (f)
         (sum
          (for/list ([i (length round-results)])
            (* (list-ref (map f round-results) i)
               (expt delta i)))))
       (list first second)))

(define (match-population population rounds-per-match)
  (for/list ([i (/ (length population)
                   2)])
    (take-sums
     (match-pair (list
                  (list-ref population (* 2 i))
                  (list-ref population (add1 (* 2 i))))
                 rounds-per-match))))

;; from the matching result, calculate the fitness
(define (reductions-h f accumulated init a-list)
  (if (null? a-list)
      accumulated
      (let ((next-init (f init (first a-list))))
        (reductions-h f
                      (append accumulated (list next-init))
                      next-init
                      (rest a-list)))))
(define (reductions f init a-list)
  (if (null? a-list)
      accumulated
      (reductions-h f '() init a-list)))
(define (reductions* f a-list)
  (let ([init (first a-list)])
    (reductions-h f (list init) init (rest a-list))))

(define (accumulate a-list)
  (reductions* + (cons 0 a-list)))

(define (payoff-percentages payoff-list)
  (let ([s (apply + payoff-list)])
    (if (zero? s)
        (for/list ([i (length payoff-list)])
          (/ 1 (length payoff-list)))
        (for/list ([i (length payoff-list)])
          (/ (list-ref payoff-list i)
             s)))))

(define (accumulated-fitness population rounds-per-match)
  (accumulate
   (payoff-percentages
    (flatten
     (match-population population rounds-per-match)))))

;; generate new automaton by randomising over the fitness vector
(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (let
      ([len (length population)])
    (for/list
        ([n speed])
      (let ([r (random)])
        (for/and ([i len])
          #:break (< r (list-ref accumulated-payoff-percentage i))
          (list-ref population i))))))

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


(define population-mean '())
;; evolve the population over cycles
;; N=100
(define (evolve population cycles speed mutation rounds-per-match)
  (let* ([round-results (match-population population rounds-per-match)]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            (* rounds-per-match 100)))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [new-population
          (shuffle (append survivors successors))]
         [mutators
          (for ([m mutation])
            (mutate (list-ref new-population m)))]
         )
    (set! population-mean
          (append population-mean (list average-payoff)))
    (if (zero? cycles)
        population
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match))))

;; TV
(define (plot-mean data)
  (let* ([l (length data)]
         [coors (map list (build-list l values)
                     data)])
    (plot (lines coors))))
