#lang racket


;Función para determinar los movimientos que puede hacer el caballo
;Entradas: tamaño del tablero y pocision inicial de caballo 
;Salidas: casillas a las que se puede mover el caballo


(define (movimientos-validos tamaño (posicion '(x y)))
  (define x (car posicion))
  (define y (cdr posicion))
  
  (define deltas '((1 . -2) (2 . -1) (2 . 1) (1 . 2) (-1 . 2) (-2 . 1) (-2 . -1) (-1 . -2)))
  
  (define (valido? movimiento)
    (define new-x (+ x (car movimiento)))
    (define new-y (+ y (cdr movimiento)))
    (and (>= new-x 1) (<= new-x tamaño) (>= new-y 1) (<= new-y tamaño)))
  
  (define (filtrar-movimientos deltas resultado)
    (cond
      ((null? deltas) resultado)
      ((valido? (car deltas))
       (filtrar-movimientos (cdr deltas) (cons (cons (+ x (car (car deltas))) (+ y (cdr (car deltas)))) resultado)))
      (else (filtrar-movimientos (cdr deltas) resultado))))
  
  (reverse (filtrar-movimientos deltas '())))


;caso de prueba

(displayln (movimientos-validos 8 '(1 . 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Función para determianr si el tablero ya esta lleno
;Entradas: el tablero y el tamaño de ese tablero
;Salidas: verdadero o falso



(define (tablero-lleno? tablero tamaño)
  (define (coordenada-visitada? x y)
    (member (cons x y) tablero))
  
  (define (iterar-coordenadas x y)
    (cond
      ((> x tamaño) #t) ; Todas las filas se han recorrido, el tablero está lleno
      ((coordenada-visitada? x y) ; Si la coordenada ya ha sido visitada
       (iterar-coordenadas (if (= y tamaño) (+ x 1) x) (if (= y tamaño) 1 (+ y 1))))
      (else #f))) ; Si hay una coordenada no visitada, el tablero no está lleno
  
  (iterar-coordenadas 1 1))


;caso de prueba

(define tablero '((1 . 1) (1 . 2) (2 . 1) (2 . 2))) ; Tablero con coordenadas visitadas
(define tamaño 2) ; Tamaño del tablero

(displayln (tablero-lleno? tablero tamaño)) ; Llama a la función tablero-lleno?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Función para agregar las casillas visitadas a una lista
;Entradas: tablero y la posicion que se desea agregar
;Salidas: la lista con las casillas visitadas 



(define (agregar-visitado tablero posición)
  (cons posición tablero))


;caso de prueba

(define tableron '()) ; Tablero vacío al inicio

(displayln "Tablero inicial:")
(displayln tableron)

(set! tableron (agregar-visitado tableron '(2 3))) ; Agregar la posición '(2 3)' al tablero
(set! tableron (agregar-visitado tableron '(4 1))) ; Agregar la posición '(4 1)' al tablero

(displayln "Tablero después de agregar casillas visitadas:")
(displayln tableron)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Función para determinar el camino recorrido de caballo
;Entradas: secuancia y la posicion que se desea agregar
;Salidas: las casiilas ordenadas por las que paso el caballo


(define (agregar-secuencia secuencia posición)
  (append secuencia (list posición)))


;caso de prueba 

(define secuencia '()) ; Secuencia de casillas inicialmente vacía

(displayln "Secuencia inicial:")
(displayln secuencia)

(set! secuencia (agregar-secuencia secuencia '(2 3))) ; Agregar la posición '(2 3)' a la secuencia
(set! secuencia (agregar-secuencia secuencia '(4 1))) ; Agregar la posición '(4 1)' a la secuencia

(displayln "Secuencia después de agregar posiciones:")
(displayln secuencia)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Función para determnar si ya se encontro una solucion
;Entradas: posible solucion y el tamaño del tablero
;Salidas: lista con la solucion


(define (find-solución soluciones tamaño)
  (cond
    ((null? soluciones) #f) ; No se encontró ninguna solución válida
    ((tablero-lleno? (car soluciones) tamaño) (car soluciones)) ; Se encontró una solución válida
    (else (find-solución (cdr soluciones) tamaño)))) ; Continuar buscando en las soluciones restantes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Función para encontar el recorrido que debe hacer el caballo para pasar por todas las casillas sin repetir 
;Entradas: tamaño del tablero y la posicion inicial
;Salidas: lista con recorrido que debe hacer el caballo para pasar por todas las casillas sin repetir 


(define (PDC-Sol tamaño posicion-inicial)
  (define (generar-secuencias-aux secuencia posición movimientos resultado)
    (cond
      ((null? movimientos) resultado)
      ((tablero-lleno? secuencia tamaño) (agregar-secuencia resultado secuencia))
      (else
       (find-solución (generar-secuencias-aux (agregar-visitado secuencia posición)
                                               (car (movimientos-validos tamaño posición))
                                               (cdr (movimientos-validos tamaño posición))
                                               resultado)
                      tamaño))))
  
  (define (find-solución soluciones tamaño)
    (cond
      ((null? soluciones) #f) ; No se encontró ninguna solución válida
      ((tablero-lleno? (car soluciones) tamaño) (car soluciones)) ; Se encontró una solución válida
      (else (find-solución (cdr soluciones) tamaño)))) ; Continuar buscando en las soluciones restantes
  
  (generar-secuencias-aux '() posicion-inicial (movimientos-validos tamaño posicion-inicial) '()))

;caso de prueba

;(PDC-Sol 8 ‘(1 1))


