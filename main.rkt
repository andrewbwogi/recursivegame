#lang racket

(require 2htdp/universe)
(require "structs-constants.rkt" "rendering.rkt" "key-events.rkt" "mechanics.rkt")

; start the game
(define (start-game)
  (big-bang (initialize-world GAME-FRAME 1)
    (on-key player-action)
    (to-draw render-game)
    (on-tick update-game TICK-RATE)
    (stop-when collision? render-the-end)))

(start-game)

; TODO:

; is there a better name for 'old-timers'? where should the 'old' prefix be used?
; fix all comments
; obstacle front edge and rear edge not correctly computed
; use array to hold worlds for faster jumps?
; use racket object system to represent objects and their methods
; (A) fix so time and distance follow seconds and meter. calculate precise default-timers
; (A) why does update-box need a current-world-frame? can this frame be a part of the box?
; (A) link TIME to box growth, and all velocities. make them a fraction of time so when time changes, relationships remain the same.
; (A) add option for fullcscreen acceleration for objects
; (A) load constants from file
; put up todo list as github issues
; if box grows too fast, boxes do not land on the original ground
; what happens if a function returns nothing? does a for loop recur on #void?
; spawn objects so they can be evaded (either spawn tight enough or spread enough)
; sometimes boxes spawn when overboxes are in air. here, objects on that level can spawn?
; set up point system and a database

;;;;
;;;; lower priority
;;;;

; (draw the box one point above the ground. fix so that this happens in render)
; (fix full screen (display-mode 'fullscreen) https://groups.google.com/forum/#!topic/racket-users/9caVVvUGqlo
; https://docs.racket-lang.org/gui/windowing-overview.html?q=get-display-size#%28part._display-resolution%29)

;;;;
;;;; about constants (customer decision)
;;;;

; (what acceleration should objects have under the box? what if accelerated through whole screen?)
; (set all constants right)
; spawn logic/difficulty
; velocity and acceleration
; object size
