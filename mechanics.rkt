#lang racket
(provide update-game initialize-world collision?)
(require "structs-constants.rkt")

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

; update the size of the box
(define (update-box-side-length box)
  (+ (box-side-length box) BOX-GROWTH))

; set the area that defines the borders of the box
(define (update-box-frame box old-world-frame)
  (define half-side-length (/ (box-side-length box) 2))
  (define center-x (point-x (box-center-point box)))
  (frame
   (- (frame-bottom old-world-frame) (box-jump-value box))
   (- center-x half-side-length)
   (+ center-x half-side-length)
   ))

; set new jump value, which is the y distance to the world bottom
(define (update-box-jump-value old-box)  
  (if (< (round (box-jump-value old-box)) 0)
      0
      (+ (box-jump-value old-box)
         (+ (* (box-velocity old-box) TIME) (* TIME TIME (/ 1 2) GRAVITY)))))

; set new box velocity
(define (update-box-velocity old-box)
  (if (< (round (box-jump-value old-box)) 0)
      0
      (+ (box-velocity old-box) (* GRAVITY TIME))))

; make each obstacle move one step to the left
(define (update-obstacles box obstacles old-frame timers) 
  (append (map (lambda (o)
                 (define new-x-value (- (obstacle-x-value o) (obstacle-velocity o)))
                 (struct-copy obstacle o
                              [x-value new-x-value]
                              [y-value (- (frame-bottom old-frame) (/ (obstacle-height o) 2))]
                              [front-edge (- new-x-value (/ (obstacle-width o) 2))]
                              [rear-edge (+ new-x-value (/ (obstacle-width o) 2))]
                              [velocity (update-obstacle-velocity (box-frame box) o)]))
               (remove-gone-obstacles obstacles old-frame))
          (spawn-obstacle timers box old-frame)))

; add a new obstacle to the world's obstacle list if it is time and the box size is not too big
(define (spawn-obstacle timers box frame)
  (if
   (and (< (timers-arm-obstacle-spawn timers) 0)
        (< (timers-spawn-obstacle timers) 0)
        (< (box-side-length box) BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN))
   (list (initialize-obstacle frame))
   '()))

; accelerate the obstacle if it is under the box
(define (remove-gone-obstacles obstacles frame)
  (if (not (empty? obstacles))
      (if (< (obstacle-rear-edge (first obstacles)) (frame-left frame))
          (rest obstacles)
          obstacles)
      '()))

; accelerate the obstacle if it is under the box
(define (update-obstacle-velocity b-frame o)
  (if (and (< (frame-left b-frame) (obstacle-rear-edge o))
           (> (frame-right b-frame) (obstacle-front-edge o)))
      (+ (obstacle-velocity o) (* (obstacle-acceleration o) TIME))
      (obstacle-velocity o)))

; update all timers
(define (update-timers old-timers)
  (cond
    [(< (timers-spawn-obstacle old-timers) 0)
     (reset-obstacle-arm old-timers)]
    [(< (timers-arm-obstacle-spawn old-timers) 0)
     (decrement-world-obstacle old-timers)]
    [else (decrement-world-arm old-timers)]))

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

; is there a collision between any box and any obstacle?
(define (collision? worlds)
  (for/or ([w worlds])
    (if (not (above-obstacles? (world-box w) (world-obstacles w)))
        #t
        #f)))

; make sure all obstacles are outside the box' collision area
(define (above-obstacles? box obstacles)
  (for/and ([o obstacles])
    (or (> (box-jump-value box) (obstacle-height o))
        (or (> (frame-left (box-frame box)) (obstacle-rear-edge o))
            (< (frame-right (box-frame box)) (obstacle-front-edge o))))))

; we begin the game with one box and one obstacle
(define (initialize-world frame level)
  (define box0 (initialize-box))
  (define obstacles0 '())
  (list (world box0 obstacles0 frame level (timers ARM-TIME OBSTACLE-SPAWN-TIME WORLD-SPAWN-TIME))))

; a new box
(define (initialize-box)
  (box 0 DEFAULT-CENTER-POINT DEFAULT-BOX-SIZE (frame 0 25 25) 0 0))

; a new obstacle
(define (initialize-obstacle old-frame)
  (obstacle (frame-right old-frame) (- DEFAULT-OBSTACLE-X-VALUE (/ DEFAULT-OBSTACLE-WIDTH 2))
            (+ DEFAULT-OBSTACLE-X-VALUE (/ DEFAULT-OBSTACLE-WIDTH 2))
            (- (frame-bottom old-frame) (/ DEFAULT-OBSTACLE-HEIGHT 2)) DEFAULT-OBSTACLE-WIDTH
            DEFAULT-OBSTACLE-HEIGHT DEFAULT-OBSTACLE-VELOCITY DEFAULT-OBSTACLE-ACCELERATION))