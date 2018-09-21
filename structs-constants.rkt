#lang racket
(provide (all-defined-out))
(require (prefix-in gui: racket/gui/base))
(require (prefix-in 2htdp: 2htdp/image))

; a world of the game
(struct world (box obstacles frame level timers) #:transparent)

; the two main objects of the game
(struct box (id center-point side-length frame jump-value velocity) #:transparent)
(struct obstacle (x-value y-value front-edge rear-edge width height velocity acceleration) #:transparent)

; coordinates for a point
(struct point (x y) #:transparent)

; frame defines the x-coordinates of the frame's left and right edge and y-coordinate for the bottom edge
(struct frame (bottom left right) #:transparent)

; a container for timers that decide when next obstacle can be spawned
(struct timers (arm-obstacle-spawn spawn-obstacle spawn-world) #:transparent)

; constants for the main window frame
(define-values (X Y) (gui:get-display-size))
(define FRAME-X (- Y 100))
(define FRAME-Y (- Y 100))
(define FRAME (2htdp:rectangle FRAME-X FRAME-Y 'outline 'white))
(define BASE-OF-FRAME ( - FRAME-Y 1))
(define GAME-FRAME (frame BASE-OF-FRAME 0 FRAME-X))

; constants for boxes
(define DEFAULT-CENTER-POINT (point (/ FRAME-X 2) (/ FRAME-Y 2)))
(define DEFAULT-BOX-SIZE 5)
(define BOX-GROWTH 0.5)
(define DEFAULT-JUMP-VELOCITY 90)
(define BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN (* FRAME-X 0.8))
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
(define ARM-TIME (+ PERIOD1 PERIOD2))
(define OBSTACLE-SPAWN-TIME (* 2 PERIOD1))
(define WORLD-SPAWN-TIME (* 60 PERIOD1))