#lang racket
(require 2htdp/universe "structs-constants.rkt" (file "/usr/racket/share/pkgs/htdp-lib/2htdp/private/stop.rkt"))

; actions of the player
(define (player-action w k)
  (cond
    [(key=? "1" k) (jump w 1)]
    [(key=? "2" k) (jump w 2)]
    [(key=? "3" k) (jump w 3)]
    [(key=? "4" k) (jump w 4)]
    [(key=? "5" k) (jump w 5)]
    [(key=? "q" k) (stop-with w)]))

; let the box at particular level jump if it is on the ground
(define (jump worlds level)
  (define current-world (vector-ref worlds (- level 1))) 
  (when (= (box-jump-value (world-box current-world)) 0)
    (vector-set! worlds (- level 1) (make-jump current-world)))
  worlds)
(module+ test
  (require rackunit)
  (check-equal? (jump worlds 1) new-worlds))

; construct new box with a just begun jump
(define (make-jump w)
  (define new-box (struct-copy box (world-box w)
                                 [velocity DEFAULT-JUMP-VELOCITY]))
  (struct-copy world w
                 [box new-box]))

(provide 
 (contract-out
  [player-action ((vectorof world?) string? . -> . (or/c (vectorof world?) stop-the-world?))]))