#lang racket

(module+ test (require rackunit))

(require "juego.rkt")
(provide estrategia-random)

(define (estrategia-random tablero posiciones jugador-actual debug)
  (let* (
        [posicion (list-ref posiciones (random (length posiciones)))]
       )
    (when (= debug 1)
      (pintar tablero jugador-actual (string-append "Jugador " (format "~v" jugador-actual) " tiene el turno con el tablero:"))
      (display "Jugador ") (display jugador-actual) (display ": Elijo el movimiento ") (displayln posicion)
    )
    posicion
  )
)
(module+ test (check-equal? (estrategia-random tablero (list 0) 1 0) 0))

(define (prueba-random)
  (displayln "Comienza el jugador 1, ambos con estrategia random")
  (jugar tablero 1 estrategia-random estrategia-random 1)
)
(prueba-random)

(define (prueba-multi-random n)
  (display "Se jugarán ") (display n) (displayln " partidas con agentes aleatorios, la mitad empezando el jugador 1 y la otra mitad empezando el jugador 2")
  (display "Tras ") (display n)
  (multi-jugar n 1 0 0 0 estrategia-random estrategia-random)
)

(prueba-multi-random 1000)