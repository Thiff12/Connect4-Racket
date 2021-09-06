;TEC
;Ingeniería en computación
;Proyecto1 - Inteligencia Artificial
;Autores: Thifany González Vargas
;         Yerlyn Guerrero León

#lang racket/gui
(require math/array)

(define tablero (make-vector 42 0)); Tablero 
(define jugador 1);Jugador que empieza
(define draw-context 0);Se requiere para dibujar
(define profundidad 4)

;Despliega la ventana
(define ventana (new frame%
                   [label "Cuatro en línea"]
                   [width 700]
                   [height 600]))

(define canvas-hijo%
   ( class canvas%
          ( define/override ( on-char evento )                                         
              ( let ([ t ( send evento get-key-code ) ])
                ( if ( char? t )  (casilla-vacia (string t)) 0 )
                )
             )
          ( super-new )
      ))

;Crea un hijo de ventana en donde se llama la función dibujarTablero 
( define c ( new canvas-hijo% [ parent ventana ]
                 [paint-callback
                       (lambda (canvas dc) ; instancia de canvas y el draw-context
                         ( dibujarTablero dc )
                         (set! draw-context dc)
                         )]))

;Define los colores que se van a utilizar para dibujar
(define negro (make-object color% 0 0 0));Color tablero 
(define rojo (make-object color% 255 0 0));Color IA
(define amarillo (make-object color% 255 255 0));Color jugador

;Define el lápiz y la brocha que se usará para dibujar
(define lápiz-negro (make-object pen% negro 1 'xor ))
(define brocha-rojo (make-object brush% rojo 'xor ))
(define brocha-amarilla (make-object brush% amarillo 'xor))
 
;Dibuja el tablero
( define ( dibujarTablero dc ) ; recibe una instancia del 'drawing context'
   ( send dc set-pen lápiz-negro );Elige el lápiz negro.
   ; Dibuja las celdas
   ( send dc draw-line 0 100 700 100) ; x1 y1 x2 y2
   ( send dc draw-line 0 200 700 200) ; x1 y1 x2 y2
   ( send dc draw-line 0 300 700 300) ; x1 y1 x2 y2
   ( send dc draw-line 0 400 700 400) ; x1 y1 x2 y2
   ( send dc draw-line 0 500 700 500) ; x1 y1 x2 y2
   ( send dc draw-line 100 0 100 600) ; x1 y1 x2 y2
   ( send dc draw-line 200 0 200 600) ; x1 y1 x2 y2
   ( send dc draw-line 300 0 300 600) ; x1 y1 x2 y2
   ( send dc draw-line 400 0 400 600) ; x1 y1 x2 y2
   ( send dc draw-line 500 0 500 600) ; x1 y1 x2 y2
   ( send dc draw-line 600 0 600 600) ; x1 y1 x2 y2
)

;Dibuja las fichas tanto las rojas y amarillas
(define (dibujar-fichas x y)
  (if (= jugador 1) (send draw-context set-brush brocha-rojo) ( send draw-context set-brush brocha-amarilla))
  (send draw-context draw-ellipse x y 90 90) ; x y ancho alto
  )

;Muestra la ventana
(send ventana show #t)

;Cambia de turno al jugador y si el jugador es la IA llama a la función min-max
(define (cambiarTurno)
  (if(= jugador 1)  (and (mini-max) (set! jugador 2))  (set! jugador 1))
  )


;Mete la ficha en el espacio vacio y si la columna no esta completa 
(define (casilla-vacia columna)
  (define numColumna (string->number columna))
  (displayln  numColumna)
  (define fichaPuesta #f)
  (if (equal? (columna-incompleta (- numColumna 1)) #t)
      (for ([i 6])
        (if(and(=(vector-ref tablero (+ (* 7 i) (- numColumna 1))) 0) (equal? fichaPuesta #f))
           (and (poner-ficha i (- numColumna 1))  (set! fichaPuesta #t)) 0)
        )(displayln "Casilla no disponible, prueba otra casilla")
      )
  )

;Pone la ficha en el tablero y la dibuja
(define(poner-ficha i j)
  (vector-set! tablero (+ (* 7 i) j) jugador)
  (dibujar-fichas (+(* j 100) 5) (+(*(- 5 i)100)5))
  (cambiarTurno)
  )


(define(mini-max)
  (define copiaTablero (vector-copy tablero 0 42))
  (displayln copiaTablero)
  ;(if estadoTerminal (eval copiaTablero) (- profundidad 1))
  (jugadas-posibles copiaTablero)
  (eval copiaTablero)

)

;Dice si un estado es terminal o no 
(define(estadoTerminal)
  (if (= profundidad 0) #t #f)
  )

;Verifica las posibles jugadas
(define(jugadas-posibles copiaTablero)
  (define posiblesJugadas (make-vector 7 -1)); 
  (define jugadas #f)
  (for ([j 7])
    (set! jugadas #f)
    (when (equal? (columna-incompleta j) #t)
        (for ([i 6])
          (if(and(=(vector-ref tablero (+ (* 7 i) j)) 0) (equal? jugadas #f))
          (and (vector-set! posiblesJugadas j (+ (* 7 i) j)) (set! jugadas #t)) 0)
    ))
    
  )
  (displayln posiblesJugadas)
  )


;Función eval
(define (eval copiaTablero)
  (cond[(equal? (cuatro-linea copiaTablero) #t) (displayln 5)]
       ;[(bloqueo copiaTablero) (displayln 4)]
       [(equal? (tres-linea copiaTablero) #t) (displayln 3)]
       [(equal? (dos-linea copiaTablero) #t) (displayln 2)]
       [else (displayln 1)]
       ))

;Función cuatro en línea
(define (cuatro-linea copiaTablero)
  (if (equal? (verificar4Columna copiaTablero) #t) #t #f) 
  ;Evalua diagonales
)

;Verifica si en las columnas hay jugadas de 4
(define (verificar4Columna tablero)
  (define seguidos 1)
  (for ([j 7]) ;Es el que se suma
    (for ([i 5]); Es el que se multiplica
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)))
      (and (display i) (displayln j))
      (define fichaSiguiente (vector-ref tablero (+ (* 7 (+ i 1)) j)))
      (and (display jugador) (displayln "jugador"))
      (if(and (= fichaActual fichaSiguiente) (= fichaActual jugador))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (displayln seguidos)
      (when (= seguidos 4) #t)
      )
    )
  )
  
;Verifica si en las filas hay jugadas de 4
(define (verificar4Fila tablero)
  (define seguidos 1)
  (for ([i 6])
    (for ([j 6])
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
      (define fichaSiguiente (vector-ref tablero (+ (* 7 i) (+ j 1))))
      (if(and (= fichaActual fichaSiguiente) (= fichaActual jugador))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 4) #t))
      )
    )

;Función tres en línea
(define (tres-linea copiaTablero)
  (if (or (equal? (verificar3Columna copiaTablero) #t) (equal? (verificar3Fila copiaTablero) #t)) #t #f) 
  ;Evalua diagonales
)

;Verifica si en las columnas hay jugadas de 3
(define (verificar3Columna tablero)
  (define seguidos 1)
  (for ([j 7]) ;Es el que se suma
    (for ([i 5]); Es el que se multiplica
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)))
      (define fichaSiguiente (vector-ref tablero (+ (* 7 (+ i 1)) j))) 
      (if(and (= fichaActual fichaSiguiente) (= fichaActual jugador))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 3) #t))
      )
    )
  

;Verifica si en las filas hay jugadas de 3
(define (verificar3Fila tablero)
  (define seguidos 1)
  (for ([i 6])
    (for ([j 6])
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
      (define fichaSiguiente (vector-ref tablero (+ (* 7 i) (+ j 1))))
      (if(and (= fichaActual fichaSiguiente) (= fichaActual jugador))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 3) #t))
      )
    )


;Función dos en línea
(define (dos-linea copiaTablero)
  (if (or (equal? (verificar2Columna copiaTablero) #t) (equal? (verificar2Fila copiaTablero) #t)) #t #f) 
  ;Evalua diagonales
)

;Verifica si en las columnas hay jugadas de 2
(define (verificar2Columna tablero)
  (define seguidos 1)
  (for ([j 7]) ;Es el que se suma
    (for ([i 5]); Es el que se multiplica
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)))
      (define fichaSiguiente (vector-ref tablero (+ (* 7 (+ i 1)) j))) 
      (if(and (= fichaActual fichaSiguiente) (= fichaActual jugador))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 2) #t))
      )
    )
  

;Verifica si en las filas hay jugadas de 2
(define (verificar2Fila tablero)
  (define seguidos 1)
  (for ([i 6])
    (for ([j 6])
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
      (define fichaSiguiente (vector-ref tablero (+ (* 7 i) (+ j 1))))
      (if(and (= fichaActual fichaSiguiente) (= fichaActual jugador))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 2) #t))
      )
  )

;Verifica si la columna ya está completa o no
(define (columna-incompleta numColumna)
  (if(>(vector-ref tablero (+ 35 numColumna)) 0) #f #t)
)


(define (ganador tablero)
  (define i 0)
  (define j 0)
  (define seguidos 1)
  ;Llamar las tres funciones aquí Columnas, Diagonal, Filas
  ;(verificarColumna tablero)
  ;(verificarFila tablero)
  (define num 0)
   (set! num (verificarDiagonalDerecha tablero seguidos i j))
  ;(display num)
  ;(verificarDiagonalIzquierda tablero)
)


(define (verificarColumna tablero)
  (define seguidos 1)
  (for ([j 7]) ;Es el que se suma
    (for ([i 5]); Es el que se multiplica
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
      (define fichaSiguiente (vector-ref tablero (+ (* 7 (+ i 1)) j))) 
      (if(and (= fichaActual fichaSiguiente) (> fichaActual 0))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 4) (display fichaActual))
      )
    )
  )

(define (verificarFila tablero)
  (define seguidos 1)
  (for ([i 6])
    (for ([j 6])
      (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
      (define fichaSiguiente (vector-ref tablero (+ (* 7 i) (+ j 1))))
      (if(and (= fichaActual fichaSiguiente) (> fichaActual 0))
         (set! seguidos (+ seguidos 1)) (set! seguidos 1))
      (when (= seguidos 4)  fichaActual)
      )
    )
  )

(define (verificarDiagonalDerecha tablero seguidos i j)
  (define cont 1)
  ;(displayln i)
  ;(displayln j)
  ;(displayln seguidos)
  (if (= seguidos 4) (vector-ref tablero (+ (* 7 i) j)) #f)
  (if(or (< i 2) (< j 4))  (set! seguidos (recorrerDiagonalDerecha tablero seguidos i j cont)) #f)
  (cond[(and (= j 3) (< i 2)) (verificarDiagonalDerecha tablero seguidos (+ i 1) 0)]
       [(< j 3) (verificarDiagonalDerecha tablero seguidos i (+ j 1))]
       )
  
  ;(if (= seguidos 4)  #f)
  ;(displayln i)
  ;(displayln j)
  ;(displayln (vector-ref tablero (+ (* 7 i) j)))
  
  ;(display seguidos)
  ;(display (vector-ref tablero (+ (* 7 i) j)))
  )


(define (recorrerDiagonalDerecha tablero seguidos i j cont)
  (set! cont (+ cont 1))
  ;(displayln i)
  ;(displayln j)
  (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
  (define fichaSiguiente (vector-ref tablero (+ (* 7 (+ i 1)) (+ j 1))))
  (if (and (= fichaActual fichaSiguiente) (> fichaActual 0)) (set! seguidos (+ seguidos 1)) (set! seguidos 1))
  (cond [(= seguidos 4) seguidos] 
        [(> cont 3) seguidos]
        [else (recorrerDiagonalDerecha tablero seguidos (+ i 1) (+ j 1) cont)]
  )
  ;(display seguidos)
  )

(define (verificarDiagonalIzquierda tablero)
  (define seguidos 1)
  (define cont 1)
  (for ([i 3])
    (for ([j (in-range 6 2 -1)])
      (recorrerDiagonalIzquierda tablero seguidos i j cont)
      )
    )
  )

(define (recorrerDiagonalIzquierda tablero seguidos i j cont)
  (set! cont (+ cont 1))
  (define fichaActual (vector-ref tablero (+ (* 7 i) j)) )
  (define fichaSiguiente (vector-ref tablero (+ (* 7 (+ i 1)) (- j 1))))
  (if (and (= fichaActual fichaSiguiente) (> fichaActual 0)) (set! seguidos (+ seguidos 1)) (set! seguidos 1))
  (cond [(= seguidos 4) (displayln fichaActual)]
        [(> cont 3) 0]
        [else (recorrerDiagonalIzquierda tablero seguidos (+ i 1) (- j 1) cont)]
  )
  )

(ganador tablero)
