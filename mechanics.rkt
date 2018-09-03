#lang racket
(provide update-game initialize-world collision?)
(require "structs-constants.rkt" rackunit)

; update the state of all the worlds
(define (update-game w)
  (spawn-world (update-worlds w)))

; every tick all boxes grow and perhaps move vertically, and the objects spawn and move closer to the boxes.
; remove passed obstacles and the world when it is as big as the frame
(define (update-worlds worlds)
  (define new-world-frame GAME-FRAME)
  (for/list ([w worlds]
             #:when (< (box-side-length (world-box w)) FRAME-X))
    (define updated-world
      (world
       (update-box (world-box w) (world-frame w))
       (update-obstacles (world-box w) (world-obstacles w) (world-frame w) (world-timers w))
       new-world-frame
       (world-level w)
       (update-timers (world-timers w))))
    (set! new-world-frame (box-frame (world-box updated-world)))
    updated-world))
(module+ test
  (require rackunit))

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
(define (update-box old-box old-world-frame)
  (box
   (box-id old-box)
   (update-box-center-point old-box)
   (update-box-side-length old-box)
   (update-box-frame old-box old-world-frame)
   (update-box-jump-value old-box)
   (update-box-velocity old-box)))

; set correct box center point in relation to the ground and its distance to ground
(define (update-box-center-point box)
  (point (/ FRAME-X 2) (- (frame-bottom (box-frame box))
                          (/ (box-side-length box) 2))))
(module+ test
  (define half-x-screen (/ FRAME-X 2))
  (define test-box (box 0 (point half-x-screen 20) 40 (frame 40 0 15) 20 10))
  (check-equal? (update-box-center-point test-box) (point half-x-screen 20)))

; update the size of the box
(define (update-box-side-length box)
  (+ (box-side-length box) BOX-GROWTH))
(module+ test
  (check-equal? (update-box-side-length test-box) (+ 40 BOX-GROWTH)))

; set the area that defines the borders of the box
(define (update-box-frame box old-world-frame)
  (define half-side-length (/ (box-side-length box) 2))
  (define center-x (point-x (box-center-point box)))
  (frame
   (- (frame-bottom old-world-frame) (box-jump-value box))
   (- center-x half-side-length)
   (+ center-x half-side-length)
   ))
(module+ test
  (define old-world-frame (frame 900 0 900))
  (check-equal? (update-box-frame test-box old-world-frame)
                (frame 880 (- half-x-screen 20) (+ half-x-screen 20))))

; set new jump value, which is the y distance to the world bottom
(define (update-box-jump-value old-box)  
  (if (< (round (box-jump-value old-box)) 0)
      0
      (+ (box-jump-value old-box)
         (+ (* (box-velocity old-box) TIME) (* TIME TIME (/ 1 2) GRAVITY)))))
(module+ test
  (define new-jump-value (+ 20 (+ (* 10 TIME) (* TIME TIME (/ 1 2) GRAVITY))))
  (define test-box-ground (struct-copy box test-box [jump-value -1]))
  (check-equal? (update-box-jump-value test-box) new-jump-value)
  (check-equal? (update-box-jump-value test-box-ground) 0))

; set new box velocity
(define (update-box-velocity old-box)
  (if (< (round (box-jump-value old-box)) 0)
      0
      (+ (box-velocity old-box) (* GRAVITY TIME))))
(module+ test
  (define new-velocity (+ 10 (* GRAVITY TIME)))
  (check-equal? (update-box-velocity test-box) new-velocity)
  (check-equal? (update-box-velocity test-box-ground) 0))

; make each obstacle move one step to the left
(define (update-obstacles box obstacles frame timers)
  (append (map (lambda (o)
                 (define new-x-value (- (obstacle-x-value o) (obstacle-velocity o)))
                 (struct-copy obstacle o
                              [x-value new-x-value]
                              [front-edge (- new-x-value (/ (obstacle-width o) 2))]
                              [rear-edge (+ new-x-value (/ (obstacle-width o) 2))]
                              [velocity (update-obstacle-velocity box o)]))
               (remove-gone-obstacles obstacles frame))
          (spawn-obstacle timers box frame)))
(module+ test
  (define empty '())
  (define test-obstacle (list (obstacle 896.0 895.0 900.0 5 35 1.5 45)))
  (define test-obstacles (list (obstacle 693.5 692.5 697.5 5 35 1.5 45) (obstacle 896.0 895.0 900.0 5 35 1.5 45) ))
  (define test-gone-obstacle (list (obstacle -1 -2 -3 5 35 1.5 45) (obstacle 896.0 895.0 900.0 5 35 1.5 45)))
  (define test-frame (frame 979 0 980))
  (define test-timers (timers 354 13 112))
  (define test-timers-spawn (timers -1 -1 112))
  (check-equal? (update-obstacles test-box empty test-frame test-timers) '())
  (check-equal? (update-obstacles test-box test-obstacle test-frame test-timers)
                (list (obstacle 894.5 892.0 897.0 5 35 1.5 45)))
  (check-equal? (update-obstacles test-box test-obstacles test-frame test-timers)
                (list (obstacle 692.0 689.5 694.5 5 35 1.5 45) (obstacle 894.5 892.0 897.0 5 35 1.5 45)))
  (check-equal? (update-obstacles test-box test-gone-obstacle test-frame test-timers)
                (list (obstacle 894.5 892.0 897.0 5 35 1.5 45)))
  (check-equal? (update-obstacles test-box test-obstacle test-frame test-timers-spawn)
                (list (obstacle 894.5 892.0 897.0 5 35 1.5 45) (initialize-obstacle test-frame))))

; add a new obstacle to the world's obstacle list if it is time and the box size is not too big
(define (spawn-obstacle timers box frame)
  (if
   (and (< (timers-arm-obstacle-spawn timers) 0)
        (< (timers-spawn-obstacle timers) 0)
        (< (box-side-length box) BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN))
   (list (initialize-obstacle frame))
   '()))
(module+ test
  (check-equal? (spawn-obstacle test-timers-spawn test-box test-frame)
                (list (initialize-obstacle test-frame)))
  (check-equal? (spawn-obstacle test-timers test-box test-frame) '()))

; accelerate the obstacle if it is under the box
(define (remove-gone-obstacles obstacles frame)
  (if (not (empty? obstacles))
      (if (< (obstacle-rear-edge (first obstacles)) (frame-left frame))
          (rest obstacles)
          obstacles)
      '()))
(module+ test
  (check-equal? (remove-gone-obstacles test-gone-obstacle test-frame) (rest test-gone-obstacle))
  (check-equal? (remove-gone-obstacles test-obstacle test-frame) test-obstacle)
  (check-equal? (remove-gone-obstacles empty test-frame) '()))

; accelerate the obstacle if it is under the box
(define (update-obstacle-velocity b o)
  (if (and (< (frame-left (box-frame b)) (obstacle-rear-edge o))
           (> (frame-right (box-frame b)) (obstacle-front-edge o)))
      (+ (obstacle-velocity o) (* (obstacle-acceleration o) TIME))
      (obstacle-velocity o)))
(module+ test
  (define test-obstacle-under (obstacle 7 5 9 5 35 1.5 45))
  (check-equal? (update-obstacle-velocity test-box (first test-obstacle)) 1.5)
  (check-equal? (update-obstacle-velocity test-box test-obstacle-under) 2.4375))

; update all timers
(define (update-timers old-timers)
  (cond
    [(< (timers-spawn-obstacle old-timers) 0)
     (reset-obstacle-arm old-timers)]
    [(< (timers-arm-obstacle-spawn old-timers) 0)
     (decrement-world-obstacle old-timers)]
    [else (decrement-world-arm old-timers)]))
(module+ test
  (check-equal? (update-timers test-timers) (timers 353 13 111))
  (check-equal? (update-timers (timers -1 13 112)) (timers -1 12 111)))

; reset spawn-obstacle and arm-spawn-obstacle timers
(define (reset-obstacle-arm old-timers)
  (timers ARM-TIME (random (round OBSTACLE-SPAWN-TIME)) (timers-spawn-world old-timers)))

; count down time during which an obstacle may be spawned
(define (decrement-world-obstacle old-timers) 
  (timers (timers-arm-obstacle-spawn old-timers)
          (sub1 (timers-spawn-obstacle old-timers))
          (sub1 (timers-spawn-world old-timers))))

; count down the time to the spawn window
(define (decrement-world-arm old-timers) 
  (timers (sub1 (timers-arm-obstacle-spawn old-timers))
          (timers-spawn-obstacle old-timers)
          (sub1 (timers-spawn-world old-timers))))

; make sure all obstacles are outside the box' collision area
(define (above-obstacles? box obstacles)
  (for/and ([o obstacles])
    (or (> (box-jump-value box) (obstacle-height o))
        (or (> (frame-left (box-frame box)) (obstacle-rear-edge o))
            (< (frame-right (box-frame box)) (obstacle-front-edge o))))))
(module+ test
  (define test-obstacle-collision (list (obstacle 9 8 10 5 35 1.5 45)))
  (check-equal? (above-obstacles? test-box test-obstacles) #t)
  (check-equal? (above-obstacles? test-box-ground test-obstacle-collision) #f))

; is there a collision between any box and any obstacle?
(define (collision? worlds)
  (for/or ([w worlds])
    (if (not (above-obstacles? (world-box w) (world-obstacles w)))
        #t
        #f)))

; we begin the game with one box and one obstacle
(define (initialize-world frame level)
  (define box0 (initialize-box))
  (define obstacles0 '())
  (list (world box0 obstacles0 frame level (timers ARM-TIME OBSTACLE-SPAWN-TIME WORLD-SPAWN-TIME))))

; a new box
(define (initialize-box)
  (box 0 DEFAULT-CENTER-POINT DEFAULT-BOX-SIZE (frame 0 25 25) 0 0))

; a new obstacle
(define (initialize-obstacle frame)
  (obstacle (frame-right frame) (- DEFAULT-OBSTACLE-X-VALUE (/ DEFAULT-OBSTACLE-WIDTH 2))
            (+ DEFAULT-OBSTACLE-X-VALUE (/ DEFAULT-OBSTACLE-WIDTH 2)) DEFAULT-OBSTACLE-WIDTH
            DEFAULT-OBSTACLE-HEIGHT DEFAULT-OBSTACLE-VELOCITY DEFAULT-OBSTACLE-ACCELERATION))