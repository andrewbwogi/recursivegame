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
(define DEFAULT-BOX-SIZE 5)
(define DEFAULT-CENTER-POINT (point (/ FRAME-X 2)
                                    (- FRAME-Y (/ DEFAULT-BOX-SIZE 2))))
(define BOX-GROWTH 0.5)
(define DEFAULT-JUMP-VELOCITY 90)
(define BOX-SIZE-LIMIT-FOR-OBSTACLE-SPAWN (* FRAME-X 0.8))
(define BOX-SIDE-LENGTH-FOR-NEW-WORLD-SPAWN 200)

; constants for obstacles
(define OBSTACLE-STEP 1.5)
(define SPAWN-RATE 1500)
(define DEFAULT-OBSTACLE-X-VALUE FRAME-X)
(define DEFAULT-OBSTACLE-WIDTH 5)
(define DEFAULT-OBSTACLE-HEIGHT 35)
(define DEFAULT-OBSTACLE-Y-VALUE (- BASE-OF-FRAME (/ DEFAULT-OBSTACLE-HEIGHT 2)))
(define DEFAULT-OBSTACLE-VELOCITY 1.5)
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

; objects for tests
(define half-x-screen (/ FRAME-X 2))
(define empty '())
(define test-obstacle (list (obstacle 896.0 BASE-OF-FRAME 895.0 900.0 5 35 1.5 45)))
(define test-obstacles (list (obstacle 693.5 BASE-OF-FRAME 692.5 697.5 5 35 1.5 45) (obstacle 896.0 BASE-OF-FRAME 895.0 900.0 5 35 1.5 45) ))
(define test-gone-obstacle (list (obstacle -1 BASE-OF-FRAME -2 -3 5 35 1.5 45) (obstacle 896.0 BASE-OF-FRAME 895.0 900.0 5 35 1.5 45)))
(define test-obstacle-updated (list (obstacle 894.5 (/ 1923 2) 892.0 897.0 5 35 1.5 45)))
(define test-obstacles-updated (list (obstacle 692.0 (/ 1923 2) 689.5 694.5 5 35 1.5 45) (obstacle 894.5 (/ 1923 2) 892.0 897.0 5 35 1.5 45)))
(define test-gone-obstacle-updated (list (obstacle 894.5 (/ 1923 2) 892.0 897.0 5 35 1.5 45)))
(define test-obstacle-under (list (obstacle half-x-screen BASE-OF-FRAME (- half-x-screen 2) (+ half-x-screen 2) 5 35 1.5 45)))
(define test-frame (frame 979 0 980))
(define test-timers (timers 354 13 112))
(define test-timers-spawn (timers -1 -1 112))
(define test-box (box 0 (point half-x-screen (- FRAME-Y 60)) 40 (frame (- FRAME-Y 40) (- half-x-screen 20) (+ half-x-screen 20)) 40 -10))
(define test-box-ground (box 0 (point half-x-screen (- FRAME-Y 21)) 40 (frame (- FRAME-Y -1) (- half-x-screen 20) (+ half-x-screen 20)) -1 -10))
(define worlds (list (world (box 0 (point 490 820.2651909722222) 318.0 (frame 980 331.25 648.75) 0 0)
                         (list (obstacle 690.5 BASE-OF-FRAME 688.0 693.0 5 35 1.5 45)) (frame 979 0 980) 1 (timers 217 17 -25))
                     (world (box 0 (point 490 970.7651909722222) 17.0 (frame 980 481.75 498.25) 0 0)
                            '() (frame 980 331.25 648.75) 2 (timers 386 20 576))))
(define new-worlds (list (world (box 0 (point 490 820.2651909722222) 318.0 (frame 980 331.25 648.75) 0 DEFAULT-JUMP-VELOCITY)
                                (list (obstacle 690.5 BASE-OF-FRAME 688.0 693.0 5 35 1.5 45)) (frame 979 0 980) 1 (timers 217 17 -25))
                         (world (box 0 (point 490 970.7651909722222) 17.0 (frame 980 481.75 498.25) 0 0)
                                '() (frame 980 331.25 648.75) 2 (timers 386 20 576))))
