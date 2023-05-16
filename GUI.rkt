#lang racket
(require racket/gui/base)
(require racket/draw)

(define horse-shown #f)
(define horse-i 0)
(define horse-j 0)
(define current-value 1)
(define visited-cells '())

(define (crear-tablero-draw matriz)
  (define n (length matriz))
  (define tablero (make-bitmap (* n 50) (* n 50)))
  (define dc (new bitmap-dc% [bitmap tablero]))
  (for ([i n])
    (for ([j n])
      (define color (if (even? (+ i j)) "white" "black"))
      (send dc set-brush color 'solid)
      (send dc draw-rectangle (* i 50) (* j 50) 50 50)))
  (for ([cell visited-cells])
  (define i (first cell))
  (define j (second cell))
  (send dc set-pen "red" 5 'solid)
  (send dc draw-point (+ (* i 50) 25) (+ (* j 50) 25)))
  (when horse-shown
    (define horse-text "♞")
    (send dc set-text-foreground "red")
    (send dc set-font (make-object font% 36 'default))
    (define-values [w h ascent descent] (send dc get-text-extent horse-text))
    (define x-offset (+ (* horse-i 50) (- (/ 50 2) (/ w 2))))
    (define y-offset (+ (* horse-j 50) (- (/ 50 2) (/ h 2))))
    (send dc draw-text horse-text x-offset y-offset))
  tablero)

(define matriz-prueba
  (list
   (list 2 9 3 13)
   (list 4 5 6 7)
   (list 14 8 1 15)
   (list 16 11 12 10)
   ))

(define tablero-prueba #f)

(define frame-width 1280)
(define frame-height 720)

(define frame
  (new frame%
       [label "Tablero de Ajedrez"]
       [width frame-width]
       [height frame-height]))

(define canvas
  (new canvas%
       [parent frame]
       [paint-callback
        (λ (_ dc)
          (set! tablero-prueba
                (crear-tablero-draw matriz-prueba))
          (define tablero-width (* 50 (length matriz-prueba)))
          (define tablero-height (* 50 (length matriz-prueba)))
          (define x-offset (/ (- frame-width tablero-width) 2))
          (define y-offset (/ (- frame-height tablero-height) 2))
          (send dc draw-bitmap tablero-prueba x-offset y-offset))]))

(define colocar-caballo-button
  (new button%
       [parent frame]
       [label "Colocar Caballo"]
       [callback
        (λ _
          (for ([i (in-range (length matriz-prueba))])
            (for ([j (in-range (length matriz-prueba))])
              (when (= (list-ref (list-ref matriz-prueba i) j) 1)
                (set! horse-i j)
                (set! horse-j i))))
          (set! horse-shown #t)
          (send comenzar-button enable #t)
          (send colocar-caballo-button enable #f)
          (send canvas refresh))]))

(define comenzar-button
  (new button%
       [parent frame]
       [label "Movimiento"]
       [enabled #f]
       [callback
        (λ _
          (set! current-value (+ current-value 1))
          (set! visited-cells (cons (list horse-i horse-j) visited-cells))
          (for ([i (in-range (length matriz-prueba))])
            (for ([j (in-range (length matriz-prueba))])
              (when (= (list-ref (list-ref matriz-prueba i) j) current-value)
                (set! horse-i j)
                (set! horse-j i))))
          (send canvas refresh))]))

(send frame show #t)


