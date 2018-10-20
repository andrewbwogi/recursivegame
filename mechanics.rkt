#lang racket
(require "structs-constants.rkt" rackunit)

; update the state of all the worlds
(define (update-game w)
  (spawn-world (update-worlds w)))

; every tick all boxes grow and perhaps move vertically.
; objects spawn and move closer to the boxes.
; remove passed obstacles and the world when it is as big as the frame
(define (update-worlds worlds)
  (define new-world-frame GAME-FRAME)
  (for/list ([w worlds]
             #:when (< (box-side-length (world-box w)) FRAME-X))
    (define updated-world
      (world
       (update-box (world-box w) new-world-frame)
       (update-obstacles (world-box w) (world-obstacles w) new-world-frame (world-timers w))
       new-world-frame
       (world-level w)
       (update-timers (world-timers w))))
    (set! new-world-frame (box-frame (world-box updated-world)))
    updated-world))

; spawn a new world if the last world is old enough
(define (spawn-world worlds)
  (if (< (timers-spawn-world (world-timers (last worlds))) 0)
      (append (for/list ([w worlds]
                         [k (in-naturals 1)])
                (struct-copy world w
                             [level k]))
              (initialize-world (box-frame (world-box (last worlds))) (+ (length worlds) 1)))
      worlds))

; make the box grow in the corners, and update its y-value according to velocity
(define (update-box current-box current-world-frame)
  (define new-side-length (update-box-side-length current-box))
  (define new-frame (update-box-frame current-box current-world-frame))
  (box
   (box-id current-box)
   (update-box-center-point new-frame new-side-length)
   new-side-length
   new-frame
   (update-box-jump-value current-box)
   (update-box-velocity current-box)))

; set correct box center point in relation to the ground and its distance to ground
(define (update-box-center-point current-frame current-side-length)
  (point (/ FRAME-X 2) (- (frame-bottom current-frame)
                          (/ current-side-length 2))))
(module+ test
  (require rackunit)
  (define half-x-screen (/ FRAME-X 2))
  (check-equal? (update-box-center-point (box-frame test-box) (box-side-length test-box)) (point half-x-screen 920)))

; update the size of the box
(define (update-box-side-length current-box)
  (+ (box-side-length current-box) BOX-GROWTH))
(module+ test
  (check-equal? (update-box-side-length test-box) (+ 40 BOX-GROWTH)))

; set the area that defines the borders of the box
(define (update-box-frame current-box current-world-frame)
  (define half-side-length (/ (box-side-length current-box) 2))
  (define center-x (point-x (box-center-point current-box)))
  (frame
   (- (frame-bottom current-world-frame) (box-jump-value current-box))
   (- center-x half-side-length)
   (+ center-x half-side-length)
   ))
(module+ test
  (define current-world-frame (frame 900 0 900))
  (check-equal? (update-box-frame test-box current-world-frame)
                (frame 860 (- half-x-screen 20) (+ half-x-screen 20))))

; set new jump value, which is the y distance to the world bottom
(define (update-box-jump-value current-box)
  (define jump-value (+ (box-jump-value current-box)
         (+ (* (box-velocity current-box) TIME) (* TIME TIME (/ 1 2) GRAVITY))))
  (if (<= jump-value 0)
      0
      jump-value))
(module+ test
  (define new-jump-value (+ 40 (+ (* -10 TIME) (* TIME TIME (/ 1 2) GRAVITY))))
  (check-equal? (update-box-jump-value test-box) new-jump-value)
  (check-equal? (update-box-jump-value test-box-ground) 0))

; set new box velocity
(define (update-box-velocity current-box)
  (if (< (round (box-jump-value current-box)) 0)
      0
      (+ (box-velocity current-box) (* GRAVITY TIME))))
(module+ test
  (define new-velocity (+ -10 (* GRAVITY TIME)))
  (check-equal? (update-box-velocity test-box) new-velocity)
  (check-equal? (update-box-velocity test-box-ground) 0))

; make each obstacle move one step to the left.
; if an obstacle is under a box, make it accelerate
(define (update-obstacles current-box obstacles current-world-frame current-timers) 
  (append (map (lambda (o)
                 (define new-x-value (- (obstacle-x-value o) (obstacle-velocity o)))
                 (struct-copy obstacle o
                              [x-value new-x-value]
                              [y-value (- (frame-bottom current-world-frame) (/ (obstacle-height o) 2))]
                              [front-edge (- new-x-value (/ (obstacle-width o) 2))]
                              [rear-edge (+ new-x-value (/ (obstacle-width o) 2))]
                              [velocity (update-obstacle-velocity current-box o)]))
               (remove-gone-obstacles obstacles current-world-frame))
          (spawn-obstacle current-timers current-box current-world-frame)))
(module+ test 
  (check-equal? (update-obstacles test-box empty test-frame test-timers) '())
  (check-equal? (update-obstacles test-box test-obstacle test-frame test-timers)
                test-obstacle-updated)
  (check-equal? (update-obstacles test-box test-obstacles test-frame test-timers)
                test-obstacles-updated)
  (check-equal? (update-obstacles test-box test-gone-obstacle test-frame test-timers)
                test-gone-obstacle-updated)
  (check-equal? (update-obstacles test-box test-obstacle test-frame test-timers-spawn)
                (list (first test-obstacle-updated) (initialize-obstacle test-frame))))

; add a new obstacle to the world's obstacle list if it
; is time and the box size is not too big
(define (spawn-obstacle current-timers current-box current-world-frame)
  (if
   (and (< (timers-arm-obstacle-spawn current-timers) 0)
        (< (timers-spawn-obstacle current-timers) 0)
        (< (box-side-length current-box) BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN))
   (list (initialize-obstacle current-world-frame))
   '()))
(module+ test
  (check-equal? (spawn-obstacle test-timers-spawn test-box test-frame)
                (list (initialize-obstacle test-frame)))
  (check-equal? (spawn-obstacle test-timers test-box test-frame) '()))

; accelerate the obstacle if it is under the box
(define (remove-gone-obstacles obstacles current-world-frame)
  (if (not (empty? obstacles))
      (if (< (obstacle-rear-edge (first obstacles)) (frame-left current-world-frame))
          (rest obstacles)
          obstacles)
      '()))
(module+ test
  (check-equal? (remove-gone-obstacles test-gone-obstacle test-frame) (rest test-gone-obstacle))
  (check-equal? (remove-gone-obstacles test-obstacle test-frame) test-obstacle)
  (check-equal? (remove-gone-obstacles empty test-frame) '()))

; accelerate the obstacle if it is under the box
(define (update-obstacle-velocity current-box o)
  (if (and (< (frame-left (box-frame current-box)) (obstacle-rear-edge o))
           (> (frame-right (box-frame current-box)) (obstacle-front-edge o)))
      (+ (obstacle-velocity o) (* (obstacle-acceleration o) TIME))
      (obstacle-velocity o)))
(module+ test
  (check-equal? (update-obstacle-velocity test-box (first test-obstacle)) 1.5)
  (check-equal? (update-obstacle-velocity test-box (first test-obstacle-under)) 2.4375))

; update all timers
(define (update-timers current-timers)
  (cond
    [(< (timers-spawn-obstacle current-timers) 0)
     (reset-obstacle-arm current-timers)]
    [(< (timers-arm-obstacle-spawn current-timers) 0)
     (decrement-world-obstacle current-timers)]
    [else (decrement-world-arm current-timers)]))
(module+ test
  (check-equal? (update-timers test-timers) (timers 353 13 111))
  (check-equal? (update-timers (timers -1 13 112)) (timers -1 12 111)))

; reset spawn-obstacle and arm-spawn-obstacle timers
(define (reset-obstacle-arm current-timers)
  (timers ARM-TIME (random (round OBSTACLE-SPAWN-TIME)) (timers-spawn-world current-timers)))

; count down time during which an obstacle may be spawned
(define (decrement-world-obstacle current-timers) 
  (timers (timers-arm-obstacle-spawn current-timers)
          (sub1 (timers-spawn-obstacle current-timers))
          (sub1 (timers-spawn-world current-timers))))

; count down the time to the spawn window
(define (decrement-world-arm current-timers) 
  (timers (sub1 (timers-arm-obstacle-spawn current-timers))
          (timers-spawn-obstacle current-timers)
          (sub1 (timers-spawn-world current-timers))))

; are all obstacles outside the box' collision area?
(define (above-obstacles? current-box obstacles)
  (for/and ([o obstacles])
    (or (> (box-jump-value current-box) (obstacle-height o))
        (or (> (frame-left (box-frame current-box)) (obstacle-rear-edge o))
            (< (frame-right (box-frame current-box)) (obstacle-front-edge o))))))
(module+ test
  (check-equal? (above-obstacles? test-box test-obstacles) #t)
  (check-equal? (above-obstacles? test-box-ground test-obstacle-under) #f))

; is there a collision between any box and any obstacle?
(define (collision? worlds)
  (for/or ([w worlds])
    (if (not (above-obstacles? (world-box w) (world-obstacles w)))
        #t
        #f)))

; we begin the game with one box and one obstacle
(define (initialize-world current-world-frame level)
  (define box0 (initialize-box))
  (define obstacles0 '())
  (list (world box0 obstacles0 current-world-frame level
               (timers ARM-TIME OBSTACLE-SPAWN-TIME WORLD-SPAWN-TIME))))

; return a new box. default values will be replaced with correct values by update-box
(define (initialize-box)
  (box 0 DEFAULT-CENTER-POINT DEFAULT-BOX-SIZE (frame 0 0 0) 0 0))

; return a new obstacle. default values will be replaced with correct values by update-obstacles
(define (initialize-obstacle current-world-frame)
  (define x-value (frame-right current-world-frame))
  (define y-value (frame-bottom current-world-frame))
  (obstacle x-value y-value (- x-value (/ DEFAULT-OBSTACLE-WIDTH 2))
            (+ x-value (/ DEFAULT-OBSTACLE-WIDTH 2)) DEFAULT-OBSTACLE-WIDTH
            DEFAULT-OBSTACLE-HEIGHT DEFAULT-OBSTACLE-VELOCITY DEFAULT-OBSTACLE-ACCELERATION))

(provide
 (contract-out
  [update-game ((listof world?) . -> . (listof world?))]
  [initialize-world (frame? integer? . -> . (listof world?))]
  [collision? ((listof world?) . -> . boolean?)]))
