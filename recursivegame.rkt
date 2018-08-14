;;;;;;;; RECURSION
;;;;;;;;
;;;;;;;;

#lang racket

(require 2htdp/image 2htdp/universe lang/posn racket/date)
(require (prefix-in gui: racket/gui/base))

;;;;;;;;
;;;;;;;; STRUCTS
;;;;;;;;

; a world of the game
(struct world (box obstacles frame level timers))

; the two main objects of the game
(struct box (id center-point side-length frame jump-value velocity))
(struct obstacle (x-value front-edge rear-edge width height velocity acceleration))

; coordinates for a point
(struct point (x y))

; frame defines the x-coordinates of the frame's left and right edge and y-coordinate for the bottom edge
(struct frame (bottom left right))

; a container for timers that decide when next obstacle can be spawned
(struct timers ([time-to-spawn-window #:mutable] [spawn-window #:mutable]))

;;;;;;;;
;;;;;;;; CONSTANTS
;;;;;;;;

; constants for the main window frame
(define-values (X Y) (gui:get-display-size))
(define FRAME-X (- Y 100))
(define FRAME-Y (- Y 100))
(define FRAME (rectangle FRAME-X FRAME-Y 'outline 'white))
(define BASE-OF-FRAME ( - FRAME-Y 1))
(define GAME-FRAME (frame BASE-OF-FRAME 0 FRAME-X))

; constants for boxes
(define DEFAULT-CENTER-POINT (point (/ FRAME-X 2) (/ FRAME-Y 2)))
(define DEFAULT-BOX-SIZE 5)
(define BOX-GROWTH 0.5)
(define DEFAULT-JUMP-VELOCITY 90)
(define BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN (* FRAME-X 0.9))
(define BOX-SIDE-LENGTH-FOR-NEW-WORLD-SPAWN 200)

; constants for obstacles
(define OBSTACLE-STEP 1.5)
(define SPAWN-RATE 1500)
(define DEFAULT-OBSTACLE-X-VALUE FRAME-X)
(define DEFAULT-OBSTACLE-HEIGHT 35)
(define DEFAULT-OBSTACLE-VELOCITY 1.5)
(define DEFAULT-OBSTACLE-WIDTH 5)
(define DEFAULT-OBSTACLE-ACCELERATION 45)

; constants for world physics
(define GRAVITY -70)
(define TICK-RATE (/ 1 48))
(define TIME TICK-RATE)

; constants for spawn time
(define PERIOD1 10)
(define PERIOD2 400)
(define DEFAULT-TIME-TO-SPAWN-WINDOW (+ PERIOD1 PERIOD2))
(define DEFAULT-SPAWN-WINDOW (* 2 PERIOD1))
 
;;;;;;;;
;;;;;;;; MAIN
;;;;;;;;

; start the game
(define (start-game)
  (big-bang (initialize-game GAME-FRAME)
    (on-key player-action)
    (to-draw render-world)
    (on-tick update-game TICK-RATE)
    (stop-when collision? render-the-end)))

; we begin the game with one box and one obstacle
(define (initialize-game frame)
  (define box0 (initialize-box))
  (define obstacles0 '())
  (world box0 obstacles0 frame 1 (timers DEFAULT-TIME-TO-SPAWN-WINDOW DEFAULT-SPAWN-WINDOW)))

; here we define actions of the player
(define (player-action w k)
  (cond
    [(key=? "1" k) (jump w 1)]
    [(key=? "2" k) (jump w 2)]
    [(key=? "3" k) (jump w 3)]
    [(key=? "4" k) (jump w 4)]
    [(key=? "5" k) (jump w 5)]
    [(key=? "q" k) (stop-with w)]))

; first, render one layer of a box and obstacles.
; then, render all the remaining layers and
; combine them into one composite image
(define (render-world w)
  (define images-and-positions (make-images-and-positions (recursive-world-box w)
                                                          (recursive-world-obstacles w)
                                                          (frame-bottom (recursive-world-frame w)))) 

  (if (equal? (recursive-world-next-world w) 'empty)
      (place-images (first images-and-positions)
                    (second images-and-positions)
                    FRAME)
      (place-images (first images-and-positions)
                    (second images-and-positions)
                    (render-world (recursive-world-next-world w)))))

; update the state of all the worlds
(define (update-game w)
  (spawn-world (update-worlds w)))

; is there a collision between any box and any obstacle?
(define (collision? w)
  (cond
    [(not (box-above-all-obstacles? (recursive-world-box w) (recursive-world-obstacles w))) #t]
    [(equal? (recursive-world-next-world w) 'empty) #f]
    [else (collision? (recursive-world-next-world w))]))

; just place "game over" text on top of last rendered image
(define (render-the-end w)
  (overlay (text "Game Over" 36 "black"); (text (number->string (spawn-timers-timer2 (recursive-world-spawn-timers w)))  36 "black")
           (render-world w)))

;;;;;;;;
;;;;;;;; INIT
;;;;;;;;

; a new box
(define (initialize-box)
  (box 0 DEFAULT-CENTER-POINT DEFAULT-BOX-SIZE (frame 0 25 25) 0 0))

; a new obstacle
(define (initialize-obstacle frame)
  (obstacle (frame-right frame) (- DEFAULT-OBSTACLE-X-VALUE (/ DEFAULT-OBSTACLE-WIDTH 2))
            (+ DEFAULT-OBSTACLE-X-VALUE (/ DEFAULT-OBSTACLE-WIDTH 2)) DEFAULT-OBSTACLE-WIDTH
            DEFAULT-OBSTACLE-HEIGHT DEFAULT-OBSTACLE-VELOCITY DEFAULT-OBSTACLE-ACCELERATION))

;;;;;;;;
;;;;;;;; RENDERING
;;;;;;;;

; draw the different objects, extract their respective positions
; and combine everything into a list of two lists (images and positions).
(define (make-images-and-positions box obstacles world-frame-bottom)
  (define box-image (list (draw-box box)))
  (define obstacle-images (draw-obstacles obstacles))
  (define box-position (list (make-posn (point-x (box-center-point box))
                                        (point-y (box-center-point box)))))
  (define obstacle-positions (get-obstacle-positions obstacles world-frame-bottom))       
  (define all-images (append box-image obstacle-images))
  (define all-positions (append box-position obstacle-positions))
  (list all-images all-positions))

; make a list of obstacles into a list of positions of these obstacles
(define (get-obstacle-positions obstacles world-frame-bottom)
  (define (extract-positions obstacle list-rest)
    (cons (make-posn (obstacle-x-value obstacle) (- world-frame-bottom (/ (obstacle-height obstacle) 2)))
          list-rest))
  (foldr extract-positions '() obstacles))

; draw an image of the box
(define (draw-box box)
  (square (box-side-length box) "outline" "slateblue"))

; draw images of the obstacles, put them in a list
(define (draw-obstacles obstacles)
  (define (extract-images obstacle list-rest)
    (cons (rectangle (obstacle-width obstacle) (obstacle-height obstacle) "solid" "green")
          list-rest))
  (foldr extract-images '() obstacles))

;;;;;;;;
;;;;;;;; MECHANICS
;;;;;;;;

; every tick all boxes grow and perhaps move vertically, and the objects spawn and move closer to the boxes.
; remove passed obstacles and the world when it is as big as the frame
(define (update-worlds worlds)
  (define world-frame DEFAULT-WORLD-FRAME)
  (for/list ([w worlds]
             #:when (< (box-side-length (world-box w)) FRAME-X))
    (define updated-world
      (world
       (update-box (world-box w) (world-frame w))
       (update-obstacles (world-box w) (world-obstacles w) (world-frame w))
       world-frame
       (world-level w)
       (update-timers (world-timers w))))
    (set! world-frame (world-box updated-world))
    updated-world))

; spawn a new world if the last world is old enough
(define (spawn-world worlds)
  (if (< (timers-spawn-world (world-timers (last worlds))) 0)
      (cons (for/list ([w worlds]
                       [k (in-naturals)])
              (struct-copy world w
                           [level k]))
            (initialize-recursive-world (box-frame (recursive-world-box current-world)) current-world)) 
      worlds))

; make the box grow in the corners, and update its y-value according to velocity
(define (update-box box world-frame)
  (box
   (box-id box)
   (update-box-center-point box)
   (update-box-side-length box)
   (update-box-frame box world-frame)
   (update-box-jump-value box)
   (update-box-velocity box)))

; set correct box center point in relation to the ground and its distance to ground
(define (update-box-center-point box)
  (point (/ FRAME-X 2) (- (frame-bottom (box-frame box))
                          (/ (box-side-length box) 2))))

; update the size of the box
(define (update-box-side-length box)
  (+ (box-side-length box) BOX-GROWTH))

; set the area that defines the borders of the box
(define (update-box-frame box world-frame)
  (define half-side-length (/ (box-side-length box) 2))
  (define center-x (point-x (box-center-point box)))
  (frame
   (- center-x half-side-length)
   (+ center-x half-side-length)
   (- (frame-bottom world-frame) (box-jump-value box))))

; set new jump value, which is the y distance to the world bottom
(define (update-box-jump-value box)
  (if (< (box-jump-value box) 0)
      0
      (+ (box-jump-value box)
         (+ (* (box-velocity box) TIME) (* TIME TIME (/ 1 2) GRAVITY)))))

; set new box velocity
(define (update-box-velocity box)
  (if (< (box-jump-value box) 0)
      0
      (+ (box-velocity box) (* GRAVITY TIME))))

; make each obstacle move one step to the left
(define (update-obstacles box obstacles frame timers)
  (append (map (lambda (o)
                 (struct-copy obstacle o
                              [x-value (- (obstacle-x-value o) (obstacle-velocity o))]
                              [front-edge (- (obstacle-x-value o) (/ (obstacle-width o) 2))]
                              [rear-edge (+ (obstacle-x-value o) (/ (obstacle-width o) 2))]
                              [velocity (update-obstacle-velocity b o)]))
               (remove-gone-obstacles obstacles frame))
          (spawn-obstacle timers box frame)))

; add a new obstacle to the world's obstacle list if it is time and the box size is not too big
(define (spawn-obstacle timers box frame)
  (if
      (and (< (timers-arm-timer timers) 0)
           (< (timers-spawn-timer timers) 0)
           (< (box-side-length box) BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN))
    (initialize-obstacle frame)
    '()))

; accelerate the obstacle if it is under the box
(define (remove-gone-obstacles obstacles frame)
  if (< (obstacle-rear-edge (first obstacles)) (frame-left frame))
                 (rest obstacles)
                 obstacles)

; accelerate the obstacle if it is under the box
(define (update-obstacle-velocity b o)
  (if (and (< (frame-left (box-frame b)) (obstacle-rear-edge o))
           (> (frame-right (box-frame b)) (obstacle-front-edge o)))
      (+ (obstacle-velocity o) (* (obstacle-acceleration o) TIME))
      (obstacle-velocity o)))

; update all timers
(define (update-timers timers)
  (cond
    [(and (< (timers-arm-timer timers) 0)
          (< (timers-spawn-timer timers) 0))
     (reset-arm-spawn)]
    [(< (timers-arm-timer timers) 0)
     (decrement-spawn timers)]
    [else (decrement-arm timers)]))

; reset timers
(define (reset-arm-spawn)
  (timers DEFAULT-TIME-TO-SPAWN-WINDOW (random (round DEFAULT-SPAWN-WINDOW))))

; count down time during which an obstacle may be spawned
(define (decrement-spawn timers) 
  (timers (timers-arm-timer timers)
          (sub1 (timers-spawn-timer timers))))

; count down the time to the spawn window
(define (decrement-arm timers) 
  (timers (sub1 (timers-arm-timer timers))
          (timers-spawn-timer timers)))









; make sure all obstacles are outside the box' collision area
(define (above-obstacles? box obstacles)
  (if (empty? obstacles)
      #t
      (and (outside-collision-area? box (first obstacles))
           (above-obstacles? box (rest obstacles)))))

; return true if the box is above the obstacle height and the obstacle is either
; outside the right edge of the box or the left edge of the box.
(define (outside-collision-area? box obstacle)
  (or (> (box-jump-value box) (obstacle-height obstacle))
      (or (>= (frame-left (box-frame box)) (obstacle-rear-edge obstacle))
          (<= (frame-right (box-frame box)) (obstacle-front-edge obstacle)))))

; time for box to reach max jump height from the height of an obstacle
(define PERIOD1-EXACT (/ (sqrt (+ (sqr DEFAULT-JUMP-VELOCITY) 
                                  (* 2 GRAVITY DEFAULT-OBSTACLE-HEIGHT)))(abs GRAVITY)))

; time for obstacle to pass underneath the box
(define PERIOD3 (/ (* 2 DEFAULT-BOX-SIZE) 
                   (+ DEFAULT-OBSTACLE-VELOCITY
                      (sqrt (* 2 DEFAULT-OBSTACLE-ACCELERATION DEFAULT-BOX-SIZE)))))

; time for box to reach ground from max jump height
(define PERIOD2-EXACT (abs(/ DEFAULT-JUMP-VELOCITY 
                             GRAVITY)))

;;;;;;;;
;;;;;;;; KEY EVENTS
;;;;;;;;

; let box at particular level jump if it is on the ground
(define (jump worlds level)
  (for/list ([w worlds])
    (if (and (equal? (box-jump-value (world-box w)) 0)
             (equal? (world-level w) level))
        (make-jump w)
        w)))

; construct new box with fresh jump
(define (make-jump w)
  (define new-box (struct-copy box (world-box w)
                                 [velocity DEFAULT-JUMP-VELOCITY]))
  (struct-copy world old-world
                 [box new-box]))

(start-game)

; TODO:

; refactor to functional style code
; use for loops instead of recursion
; add unit tests
; (A) refactor
; (A) fix so time and distance follow seconds and meter. calculate precise default-timers
; (A) why does update-box need a current-world-frame? can this frame be a part of the box?
; (A) link TIME to box growth, and all velocities. make them a fraction of time so when time changes, relationships remain the same.
; (A) add option for fullcscreen acceleration for objects
; (A) load constants from file
; if box grows too fast, boxes do not land on the original ground
; spawn objects so they can be evaded (either spawn tight enough or spread enough)
; sometimes boxes spawn when overboxes are in air. here, objects on that level can spawn?
; set up point system and a database

;;;;
;;;; lower priority
;;;;

; (change 5 different loops to 1 loop in which each world gets its needed update (jump, render, etc))
; (test fix box height limit, and make it disappear from the list of active boxes.)
; (test so obstacles disappear from the list of obstacles.)
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
