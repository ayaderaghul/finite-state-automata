#lang racket

(provide struct-out automaton
 	struct-out state
	struct-out action
	state-name
	state-actions
	action-event
	action-result
	automaton-states
	automaton-current-state
	generate-auto
	set-state-name!
	set-automaton-current-state!
	set-action-result!
         react
         update)

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

