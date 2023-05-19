#lang racket
(require racket/gui/base)
(require racket/draw)

;;Función encargada de comenzar el ambiente gráfico relacionado con el problema del caballo
;;recibe una matriz y con ella crea un tablero del tamaño necesario ademas de 2 botones uno encargado de
;;enseñar el caballo en pantalla y otro encargado de ir moviendolo paso por paso.
(define (PDC-Paint matrizCaballo)
  
  (define horse-shown #f)
  (define horse-i 0)
  (define horse-j 0)
  (define current-value 1)
  (define visited-cells '())
  
  ;Creación del tablero
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
      (define-values [w h ascent descent]
      (send dc get-text-extent horse-text))
      (define x-offset (+ (* horse-i 50) (- (/ 50 2) (/ w 2))))
      (define y-offset (+ (* horse-j 50) (- (/ 50 2) (/ h 2))))
      (send dc draw-text horse-text x-offset y-offset))
    tablero)

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
                  (crear-tablero-draw matrizCaballo))
            (define tablero-width (* 50 (length matrizCaballo)))
            (define tablero-height (* 50 (length matrizCaballo)))
            (define x-offset (/ (- frame-width tablero-width) 2))
            (define y-offset (/ (- frame-height tablero-height) 2))
            (send dc draw-bitmap tablero-prueba x-offset y-offset))]))
  
  ;Botón colocar caballo
  (define colocar-caballo-button
    (new button%
         [parent frame]
         [label "Colocar Caballo"]
         [callback
          (λ _
            (for ([i (in-range (length matrizCaballo))])
              (for ([j (in-range (length matrizCaballo))])
                              (when (= (list-ref (list-ref matrizCaballo i) j) 1)
                (set! horse-i j)
                (set! horse-j i))))
            (set! visited-cells (cons (list horse-i horse-j) visited-cells))
            (set! horse-shown #t)
            (send movimiento-button enable #t)
            (send colocar-caballo-button enable #f)
            (send canvas refresh))]))
  
  ;Botón movimiento
  (define movimiento-button
    (new button%
         [parent frame]
         [label "Movimiento"]
         [enabled #f]
         [callback
          (λ _
            (set! current-value (+ current-value 1))
            (for ([i (in-range (length matrizCaballo))])
              (for ([j (in-range (length matrizCaballo))])
                (when (= (list-ref (list-ref matrizCaballo i) j) current-value)
                  (set! horse-i j)
                  (set! horse-j i))))
            (set! visited-cells (cons (list horse-i horse-j) visited-cells))
            (send canvas refresh))]))

  (send frame show #t))

;;Función encargada de mostrar la matriz de forma ordenada en la consola.
;;recibe una matriz y se encarga de imprimirla de tal forma que quede cuadrada
;;poniendo un cero antes de los números menores a 10.
(define (PDC-Test matriz)
  (define (print-row row)
    (cond [(null? row) (displayln ")")]
          [else
           (cond [(< (car row) 10) (display (string-append "0" (number->string (car row))))]
                 [else (display (car row))])
           (display " ")
           (print-row (cdr row))]))
  (define (print-matriz matriz)
    (cond [(null? matriz) (display ")")]
          [else
           (display "(")
           (print-row (car matriz))
           (print-matriz (cdr matriz))]))
  (displayln "'(")
  (print-matriz matriz))


;;Ejemplos de como correr el código.
#|
(PDC-Paint '((1 2 3 4 5 6)
                   (7 8 9 10 11 12)
                   (13 14 15 16 17 18)
                   (19 20 21 22 23 24)
                   (25 26 27 28 29 30)
                   (31 32 33 34 35 36)
                   ))

(PDC-Test '((1 2 3 4 5 6)
                   (7 8 9 10 11 12)
                   (13 14 15 16 17 18)
                   (19 20 21 22 23 24)
                   (25 26 27 28 29 30)
                   (31 32 33 34 35 36)
                   ))
|#