#lang racket

(module+ test (require rackunit))

(require "juego.rkt")
(require "estrategia-random.rkt")

(define (estrategia funcion profundidad heuristica)
  (lambda (tablero jugador-inicial posiciones debug)
    (funcion tablero jugador-inicial posiciones heuristica profundidad debug)
  )
)
(module+ test (check-equal? ((estrategia estrategia-minimax 2 heuristica-simple) tablero (list 0 1 2 3 4 5) 1 0) 0))

(define (heuristica-simple tablero jugador posicion-final)
  (let (
        [casa1 (t-casa-semillasj1 tablero)]
        [casa2 (t-casa-semillasj2 tablero)]
        )
   (if (= jugador 1)
       (- casa1 casa2)
       (- casa2 casa1)
   )
 )
)
(module+ test (test-begin
  (check-equal? (heuristica-simple tablero 1 0) 0)
  (check-equal? (heuristica-simple tablero 2 0) 0)))

(define (heuristica-turno-extra tablero jugador posicion-final)
  (if (= posicion-final (casa jugador))
      1
      0
   )
)
(module+ test (test-begin
  (check-equal? (heuristica-turno-extra tablero 1 6) 1)
  (check-equal? (heuristica-turno-extra tablero 1 4) 0)))

(define (contar-robos tablero jugador-actual posiciones numero-robos)
  (cond
    [(empty? posiciones) numero-robos]
    [(not (equal? (semillas-oponente tablero (calcular-posicion (car posiciones) jugador-actual)) 0)) (contar-robos tablero jugador-actual (cdr posiciones) (+ numero-robos 1))]
    [else (contar-robos tablero jugador-actual (cdr posiciones) numero-robos)]
    )
)
(module+ test (check-equal? (contar-robos (list 4 4 0 4 0 4 1 0 5 5 0 0 0 1) 1 (list 2 4) 0) 1))

(define (posicion-vacia agujeros vacios posicion)
  (cond
    [(empty? agujeros) vacios]
    [(= (car agujeros) 0) (posicion-vacia (cdr agujeros) (append vacios (list posicion)) (+ posicion 1))]
    [else (posicion-vacia (cdr agujeros) vacios (+ posicion 1))]
   )
)
(module+ test (check-equal? (posicion-vacia (list 4 4 0 4 0 4) (list) 0) (list 2 4)))

(define (heuristica-prevenir-robos tablero jugador)
  (contar-robos tablero (siguiente-jugador jugador) (posicion-vacia (t-agujeros tablero (siguiente-jugador jugador)) (list) 0) 0)
)
(module+ test (check-equal? (heuristica-prevenir-robos tablero 1) 0))

(define (heuristica-compleja tablero jugador posicion-final)
  (+ (* (heuristica-simple tablero jugador posicion-final) 0.4) (* (heuristica-prevenir-robos tablero jugador) -0.1) (* (heuristica-turno-extra tablero jugador posicion-final) 0.5))
)
(module+ test (check-equal? (heuristica-compleja tablero 1 5) 0))

(define (max-valor tablero valor posiciones jugador-actual min-max funcion profundidad-max heuristica poda alfa beta)
  (if (empty? posiciones)
      valor
      (let* (
             [posicion (car posiciones)]
             [semillas (t-semillas tablero posicion)]
             [movimiento (mover (coger-semillas tablero posicion) (+ posicion 1) semillas jugador-actual)]
             [tablero-final (car movimiento)]
             [posicion-final (cdr movimiento)]
             [valor-nuevo (min-max valor (funcion tablero-final (siguiente-jugador jugador-actual) (- profundidad-max 1) heuristica posicion-final poda alfa beta))]
             )
        (if (and poda (>= valor-nuevo beta))
            valor-nuevo
            (max-valor tablero valor-nuevo (cdr posiciones) jugador-actual max min-value profundidad-max heuristica poda (max alfa valor-nuevo) beta)
        )
      )
   )
)
(module+ test (test-begin
  (check-equal? (max-valor tablero 0 (list 4 4 4 4 4 4) 1 min max-value 2 heuristica-simple  #t -inf.0 +inf.0) 0.0)
  (check-equal? (max-valor tablero 0 (list 4 4 4 4 4 4) 1 min max-value 2 heuristica-simple  #t -inf.0 0) 0.0)))

(define (max-value tablero jugador-actual profundidad-max heuristica posicion-final poda alfa beta)
  (let (
        [posiciones (obtener-posiciones (list) 0 (t-agujeros tablero jugador-actual) jugador-actual)]
        )
     (if (or (<= profundidad-max 0) (empty? posiciones))
         (heuristica tablero (siguiente-jugador jugador-actual) posicion-final)
         (max-valor tablero -inf.0 posiciones jugador-actual max min-value profundidad-max heuristica poda alfa beta)
     )
   )
)
(module+ test (test-begin
  (check-equal? (max-value tablero 1 2 heuristica-simple 5 #t -inf.0 +inf.0) 0.0)
  (check-equal? (max-value tablero 1 0 heuristica-simple 5 #t -inf.0 +inf.0) 0)))

(define (min-valor tablero valor posiciones jugador-actual min-max funcion profundidad-max heuristica poda alfa beta)
  (if (empty? posiciones)
      valor
      (let* (
             [posicion (car posiciones)]
             [semillas (t-semillas tablero posicion)]
             [movimiento (mover (coger-semillas tablero posicion) (+ posicion 1) semillas jugador-actual)]
             [tablero-final (car movimiento)]
             [posicion-final (cdr movimiento)]
             [valor-nuevo (min-max valor (funcion tablero-final (siguiente-jugador jugador-actual) (- profundidad-max 1) heuristica posicion-final poda alfa beta))]
             )
        (if (and poda (<= valor-nuevo alfa))
            valor-nuevo
            (min-valor tablero valor-nuevo (cdr posiciones) jugador-actual min max-value profundidad-max heuristica poda alfa (min beta valor-nuevo))
        )
      )
   )
)
(module+ test (test-begin
  (check-equal? (min-valor tablero 0 (list 4 4 4 4 4 4) 1 min max-value 2 heuristica-simple #t -inf.0 +inf.0) 0.0)
  (check-equal? (min-valor tablero 0 (list 4 4 4 4 4 4) 1 min max-value 2 heuristica-simple #t 0 +inf.0) 0.0)))

(define (min-value tablero jugador-actual profundidad-max heuristica posicion-final poda alfa beta)
   (let (
        [posiciones (obtener-posiciones (list) 0 (t-agujeros tablero jugador-actual) jugador-actual)]
        )
     (if (or (<= profundidad-max 0) (empty? posiciones))
         (heuristica tablero (siguiente-jugador jugador-actual) posicion-final)
         (min-valor tablero +inf.0 posiciones jugador-actual min max-value profundidad-max heuristica poda alfa beta)
     )
   )
)
(module+ test (test-begin
  (check-equal? (min-value tablero 1 2 heuristica-simple 5 #t -inf.0 +inf.0) 0.0)
  (check-equal? (min-value tablero 1 0 heuristica-simple 5 #t -inf.0 +inf.0) 0)))

(define (minimax tablero resultados jugador-actual posiciones profundidad-max heuristica poda alfa beta)
  (if (empty? posiciones) resultados
      (let* (
             [posicion (car posiciones)]
             [semillas (t-semillas tablero posicion)]
             [movimiento (mover (coger-semillas tablero posicion) (+ posicion 1) semillas jugador-actual)]
             [tablero-final (car movimiento)]
             [posicion-final (cdr movimiento)]
             )
        (if (= posicion-final (casa jugador-actual))
            (minimax tablero (append resultados (list (cons (max-value tablero-final (siguiente-jugador jugador-actual) (- profundidad-max 1) heuristica posicion-final poda alfa beta) posicion))) jugador-actual (cdr posiciones) profundidad-max heuristica poda alfa beta)
            (minimax tablero (append resultados (list (cons (min-value tablero-final (siguiente-jugador jugador-actual) (- profundidad-max 1) heuristica posicion-final poda alfa beta) posicion))) jugador-actual (cdr posiciones) profundidad-max heuristica poda alfa beta)
        ) 
      )
  )
)
(module+ test (check-equal? (minimax tablero (list) 1 (list 0 2 3) 2 heuristica-simple #t -inf.0 +inf.0) (list (cons 0.0 0) (cons 0.0 2) (cons -1.0 3))))

(define (estrategia-minimax tablero posiciones jugador-actual heuristica profundidad-max debug)
  (let* (
         [maximo (argmax car (minimax tablero (list) jugador-actual posiciones profundidad-max heuristica #t -inf.0 +inf.0))] ; #t activa poda, #f desactiva poda
         [valor (exact-round (car maximo))]
         [movimiento (cdr maximo)]
         )
    (when (= debug 1)
      (pintar tablero jugador-actual (string-append "Jugador " (format "~v" jugador-actual) " tiene el turno con el tablero:"))
      (display "Jugador ") (display jugador-actual) (display ": Encontrado mejor movimiento ") (display movimiento) (display " con valor de heurística ") (displayln valor)
    )
    movimiento
  )
)
(module+ test (check-equal? (estrategia-minimax tablero (list 0 2 3) 1 heuristica-simple 2 0) 0))

(define (prueba-minimax jugador-empieza profundidadj1 profundidadj2 heuristicaj1 heuristicaj2 debug)
  (if jugador-empieza
      (begin 
        (display "Comienza el jugador 1")
        (display ", el jugador 1 con minimax ") (display profundidadj1) (display " y el jugador 2 con minimax ") (displayln profundidadj2)    
        (jugar tablero 1 (estrategia estrategia-minimax profundidadj1 heuristicaj1) (estrategia estrategia-minimax profundidadj2 heuristicaj2) debug)
        )
      (begin
        (display "Comienza el jugador 2")
        (display ", el jugador 1 con minimax ") (display profundidadj1) (display " y el jugador 2 con minimax ") (displayln profundidadj2)    
        (jugar tablero 2 (estrategia estrategia-minimax profundidadj2 heuristicaj2) (estrategia estrategia-minimax profundidadj1 heuristicaj1) debug)
      )
   )
)

(prueba-minimax #t 3 1 heuristica-simple heuristica-simple 1)
(prueba-minimax #f 1 3 heuristica-simple heuristica-simple 1)

(define (prueba-multi-minimax-random n profundidad heuristica)
  (display "Se jugarán ") (display n) (displayln " con el jugador 1 usando minimax con profundidad ") (display profundidad)
  (displayln " y el jugador 2 usando una estrategia aleatoria, la mitad empezando el jugador 1 y la otra mitad empezando el jugador 2") (display "Tras ") (display n)
  (multi-jugar n 1 0 0 0 (estrategia estrategia-minimax profundidad heuristica) estrategia-random)
)

(define actual (current-milliseconds))
(prueba-multi-minimax-random 100 1 heuristica-compleja)
(display "Tiempo ") (displayln (- (current-milliseconds) actual))