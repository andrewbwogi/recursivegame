#lang racket
(require lang/posn "structs-constants.rkt")
(require (prefix-in 2htdp: 2htdp/image))

; first, render one layer of a box and obstacles.
; then, render all the remaining layers and
; combine them into one composite image
(define (render-game worlds)
  (define (get-images w) (cons (draw-box (world-box w)) (draw-obstacles (world-obstacles w))))
  (define (get-positions w) (cons (box-position (world-box w))
                                  (obstacles-positions (world-obstacles w))))
  (2htdp:place-images (get-all get-images worlds)
                (get-all get-positions worlds)
                (2htdp:scene+line FRAME FRAME-X ( + FRAME-Y 0) 0 ( + FRAME-Y 0) "black")))



; get a list of images of boxes and obstacles
(define (get-all func worlds)
  (for/fold ([acc '()])
            ([w worlds])            
    (append (func w)
            acc)))

; make a box into a position of the box
(define (box-position box)
  (make-posn (point-x (box-center-point box))
             (point-y (box-center-point box))))
(module+ test
  (require rackunit)
  (check-equal? (box-position test-box) (make-posn half-x-screen 920)))

; make a list of obstacles into a list of positions of these obstacles
(define (obstacles-positions obstacles)
  (define (extract-positions obstacle)
    (make-posn (obstacle-x-value obstacle) (obstacle-y-value obstacle)))
  (map extract-positions obstacles))
(module+ test
  (check-equal? (obstacles-positions test-obstacle) (list (make-posn 896.0 979))))

; draw an image of the box
(define (draw-box box)
  (2htdp:square (box-side-length box) "outline" "slateblue"))

; draw images of the obstacles, put them in a list
(define (draw-obstacles obstacles)
  (define (extract-images obstacle list-rest)
    (cons (2htdp:rectangle (obstacle-width obstacle) (obstacle-height obstacle) "solid" "green")
          list-rest))
  (foldr extract-images '() obstacles))

; just place "game over" text on top of last rendered image
(define (render-the-end w)
  (2htdp:overlay (2htdp:text "Game Over" 36 "black"); (text (number->string (spawn-timers-timer2 (recursive-world-spawn-timers w)))  36 "black")
           (render-game w)))

(provide
 (contract-out
  [render-game ((listof world?) . -> . 2htdp:image?)]
  [render-the-end ((listof world?) . -> . 2htdp:image?)]))