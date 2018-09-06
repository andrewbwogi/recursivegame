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
  (define worlds (list
                  (world (box 0 (point 490 820.2651909722222) 318.0 (frame 980 331.25 648.75) 0 0)
                         (list (obstacle 690.5 688.0 693.0 5 35 1.5 45)) (frame 979 0 980) 1 (timers 217 17 -25))
                  (world (box 0 (point 490 970.7651909722222) 17.0 (frame 980 481.75 498.25) 0 0)
                         '() (frame 980 331.25 648.75) 2 (timers 386 20 576))))
  (define new-worlds (list
                  (world (box 0 (point 490 820.2651909722222) 318.0 (frame 980 331.25 648.75) 0 DEFAULT-JUMP-VELOCITY)
                         (list (obstacle 690.5 688.0 693.0 5 35 1.5 45)) (frame 979 0 980) 1 (timers 217 17 -25))
                  (world (box 0 (point 490 970.7651909722222) 17.0 (frame 980 481.75 498.25) 0 0)
                         '() (frame 980 331.25 648.75) 2 (timers 386 20 576))))
  (check-equal? (jump worlds 1) new-worlds))

; construct new box with fresh jump
(define (make-jump w)
  (define new-box (struct-copy box (world-box w)
                                 [velocity DEFAULT-JUMP-VELOCITY]))
  (struct-copy world w
                 [box new-box]))