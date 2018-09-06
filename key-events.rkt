#lang racket
(provide player-action)
(require 2htdp/universe "structs-constants.rkt")

; here we define actions of the player
(define (player-action w k)
  (cond
    [(key=? "1" k) (jump w 1)]
    [(key=? "2" k) (jump w 2)]
    [(key=? "3" k) (jump w 3)]
    [(key=? "4" k) (jump w 4)]
    [(key=? "5" k) (jump w 5)]
    [(key=? "q" k) (stop-with w)]))

; let box at particular level jump if it is on the ground
(define (jump worlds level)
  (for/list ([w worlds])
    (if (and (< (box-jump-value (world-box w)) 1.5)
             (equal? (world-level w) level))
        (make-jump w)
        w)))
(module+ test
  (require rackunit)
  (check-equal? (jump worlds 1) new-worlds))

; construct new box with fresh jump
(define (make-jump w)
  (define new-box (struct-copy box (world-box w)
                                 [velocity DEFAULT-JUMP-VELOCITY]))
  (struct-copy world w
                 [box new-box]))