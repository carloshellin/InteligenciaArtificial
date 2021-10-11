#lang racket

(module+ test (require rackunit))

(provide (all-defined-out))

(define tablero (list 4 4 4 4 4 4 0 4 4 4 4 4 4 0))
(define (t-j1 t) (take t 7))
(module+ test (check-equal? (t-j1 tablero) (list 4 4 4 4 4 4 0)))

(define (t-j2 t) (take-right t 7))
(module+ test (check-equal? (t-j2 tablero) (list 4 4 4 4 4 4 0)))

(define (t-agujerosj1 t) (take t 6))
(module+ test (check-equal? (t-agujerosj1 tablero) (list 4 4 4 4 4 4)))

(define (t-agujerosj2 t) (t-agujerosj1 (take-right t 7)))
(module+ test (check-equal? (t-agujerosj2 tablero) (list 4 4 4 4 4 4)))

(define (t-agujeros t jugador)
  (cond
    [(= jugador 1) (t-agujerosj1 t)]
    [else (t-agujerosj2 t)]
    )
)
(module+ test (test-begin
  (check-equal? (t-agujeros tablero 1) (list 4 4 4 4 4 4))
  (check-equal? (t-agujeros tablero 2) (list 4 4 4 4 4 4))))
  
(define (t-semillas t indice) (list-ref t indice))
(module+ test (check-equal? (t-semillas tablero 0) 4))

(define (t-casa-semillasj1 t) (car (take-right t 8)))
(module+ test (check-equal? (t-casa-semillasj1 tablero) 0))
(define (t-casa-semillasj2 t) (car (take-right t 1)))
(module+ test (check-equal? (t-casa-semillasj2 tablero) 0))

(define t-casaj1 6)
(define t-casaj2 (- (length tablero) 1))

(define (casa jugador-actual)
  (if (= jugador-actual 1)
      t-casaj1
      t-casaj2
  )
)
(module+ test (test-begin
  (check-equal? (casa 1) 6)
  (check-equal? (casa 2) 13)))

(define (obtener-posiciones posiciones indice lista jugador-actual)
  (cond
    [(empty? lista) posiciones]
    [(= (car lista) 0) (obtener-posiciones posiciones (+ indice 1) (cdr lista) jugador-actual)]
    [else (obtener-posiciones (append posiciones (list (calcular-posicion indice jugador-actual))) (+ indice 1) (cdr lista) jugador-actual)]
  )
)
(module+ test (check-equal? (obtener-posiciones (list) 0 (list 1 0) 1) (list 0)))

(define (cambiar-semillas tablero posicion valor)
  (append (take tablero posicion) (list valor) (take-right tablero (- (length tablero) 1 posicion)))
)
(module+ test (check-equal? (cambiar-semillas tablero 0 1) (list 1 4 4 4 4 4 0 4 4 4 4 4 4 0)))

(define (agregar-semilla tablero posicion)
  (cambiar-semillas tablero posicion (+ (t-semillas tablero posicion) 1))
)
(module+ test (check-equal? (agregar-semilla tablero 0) (list 5 4 4 4 4 4 0 4 4 4 4 4 4 0)))

(define (coger-semillas tablero posicion)
  (cambiar-semillas tablero posicion 0)
)
(module+ test (check-equal? (coger-semillas tablero 0) (list 0 4 4 4 4 4 0 4 4 4 4 4 4 0)))

(define (calcular-p-final tablero posicion)
  (if (= posicion 0)
      (- (length tablero) 1)
      (- posicion 1)
  )
)
(module+ test (test-begin
  (check-equal? (calcular-p-final tablero 0) 13)
  (check-equal? (calcular-p-final tablero 1) 0)))

(define (mover tablero posicion semillas jugador-actual)
  (cond
    [(= semillas 0)
     (let (
           [posicion-final (calcular-p-final tablero posicion)]
           )
       (if (puede-robar tablero posicion-final jugador-actual)
           (cons (robar tablero posicion-final jugador-actual) posicion-final)
           (cons tablero posicion-final)
        )
      )]
    [(and (= jugador-actual 1) (= posicion t-casaj2)) (mover tablero 0 semillas jugador-actual)]
    [(and (= jugador-actual 2) (= posicion t-casaj1)) (mover tablero (+ posicion 1) semillas jugador-actual)]
    [(= (- (length tablero) 1) posicion) (mover (agregar-semilla tablero posicion) 0 (- semillas 1) jugador-actual)]
    [else (mover (agregar-semilla tablero posicion) (+ posicion 1) (- semillas 1) jugador-actual)]
  )
)
(module+ test (test-begin
  (check-equal? (mover (list 4 4 4 4 4 4 0 4 4 4 4 4 0 0) 13 8 2) (cons (list 5 5 5 5 5 5 0 5 4 4 4 4 0 1) 7))
  (check-equal? (mover (list 0 4 4 4 4 0 0 4 4 4 4 4 4 0) 6 8 1) (cons (list 0 4 4 4 4 0 7 5 5 5 5 5 0 0) 0))))

(define (calcular-posicion posicion jugador-actual)
  (if (= jugador-actual 1)
      posicion
      (+ posicion 7)
  )
)
(module+ test (test-begin
  (check-equal? (calcular-posicion 0 1) 0)
  (check-equal? (calcular-posicion 0 2) 7)))

(define (roba-semillas tablero posicion)
  (append (take tablero (- (length tablero) posicion 2)) (list 0) (take-right tablero (+ 1 posicion)))
)
(module+ test (check-equal? (roba-semillas tablero 0) (list 4 4 4 4 4 4 0 4 4 4 4 4 0 0)))

(define (suma-semillas tablero semillas casa)
  (cambiar-semillas tablero casa (+ 1 (t-semillas tablero casa) semillas))
)
(module+ test (check-equal? (suma-semillas tablero 4 6) (list 4 4 4 4 4 4 5 4 4 4 4 4 4 0)))

(define (semillas-oponente tablero posicion)
  (car (take-right tablero (+ 2 posicion)))
)
(module+ test (check-equal? (semillas-oponente tablero 0) 4))

(define (robar tablero posicion jugador-actual)
  (let (
         [tablero (coger-semillas tablero posicion)]
        )
  (suma-semillas (roba-semillas tablero posicion) (semillas-oponente tablero posicion) (casa jugador-actual))
  )
)
(module+ test (check-equal? (robar tablero 0 1) (list 0 4 4 4 4 4 5 4 4 4 4 4 0 0)))

(define (es-posicion-jugador? tablero posicion jugador-actual)
  (if (and (= jugador-actual 1))
      (and (>= posicion 0) (< posicion 6))
      (and (>= posicion 6) (< posicion (- (length tablero) 1)))
  )
)
(module+ test (test-begin
  (check-equal? (es-posicion-jugador? tablero 0 1) #t)
  (check-equal? (es-posicion-jugador? tablero 0 2) #f)))

(define (puede-robar tablero posicion jugador-actual)
  (and (es-posicion-jugador? tablero posicion jugador-actual) (= (t-semillas tablero posicion) 1) (not (equal? (semillas-oponente tablero posicion) 0)))
)
(module+ test (check-equal? (puede-robar (list 1 4 4 4 4 4 5 4 4 4 4 4 5 0) 0 1) #t))

(define (siguiente-jugador jugador-inicial)
  (if (= jugador-inicial 1)
      2
      1
  )
)
(module+ test (test-begin
  (check-equal? (siguiente-jugador 1) 2)
  (check-equal? (siguiente-jugador 2) 1)))

(define (suma agujeros resultado)
  (if (empty? agujeros)
      resultado
      (suma (cdr agujeros) (+ resultado (car agujeros)))
  )
)
(module+ test (check-equal? (suma (list 1 1 1 1 1 1) 0) 6))

(define (pintar-agujero agujeros)
  (if (empty? agujeros)
      (newline)
      (begin
        (display (car agujeros)) (display " ") (pintar-agujero (cdr agujeros))
      )
  )
)

(define (pintar tablero jugador-actual cabecera)
  (displayln "###########################")
  (displayln cabecera)
  (displayln "---------------")
  (display " ") (pintar-agujero (reverse (t-agujerosj1 tablero)))
  (display (t-casa-semillasj1 tablero)) (display "           ") (displayln (t-casa-semillasj2 tablero))
  (display " ") (pintar-agujero (t-agujerosj2 tablero))
  (displayln "---------------")
)

(define (resultado tablero jugador-actual debug)
  (let* (
        [casa1 (suma (t-j1 tablero) 0)]
        [casa2 (suma (t-j2 tablero) 0)]
        [valor (- casa1 casa2)]
      )
    (if (= debug 1)
      (begin
        (pintar (list 0 0 0 0 0 0 casa1 0 0 0 0 0 0 casa2) jugador-actual (string-append "Fin de partida con valor " (format "~v" valor)))
        (cond
          [(> valor 0) (displayln "Gana el jugador 1")]
          [(< valor 0) (displayln "Gana el jugador 2")]
          [else (displayln "Empate")]
          )
       )
      valor
     )
  )
)
(module+ test (check-equal? (resultado tablero 1 0) 0))

(define (jugar tablero jugador-inicial estrategia-jugador1 estrategia-jugador2 debug)
  (let (
        [posiciones (obtener-posiciones (list) 0 (t-agujeros tablero jugador-inicial) jugador-inicial)]
        )
    (if (or (> (t-casa-semillasj1 tablero) 24) (> (t-casa-semillasj2 tablero) 24) (empty? posiciones))
          (resultado tablero jugador-inicial debug)
        (let* (
               [posicion (estrategia-jugador1 tablero posiciones jugador-inicial debug)]
               [semillas (t-semillas tablero posicion)]
               [movimiento (mover (coger-semillas tablero posicion) (+ posicion 1) semillas jugador-inicial)]
               [tablero-final (car movimiento)]
               [posicion-final (cdr movimiento)]
               )
          (if (= posicion-final (casa jugador-inicial))
              (jugar tablero-final jugador-inicial estrategia-jugador1 estrategia-jugador2 debug)
              (jugar tablero-final (siguiente-jugador jugador-inicial) estrategia-jugador2 estrategia-jugador1 debug)
          )
        )
    )
  )
)
(module+ test (check-equal? (number? (jugar tablero 1 (lambda (tablero posiciones jugador-inicial debug) (list-ref posiciones (random (length posiciones)))) (lambda (tablero posiciones jugador-inicial debug) (list-ref posiciones (random (length posiciones)))) 0)) #t))

(define (multi-jugar n jugador-inicial ganadas perdidas empatadas estrategiaj1 estrategiaj2)
  (if (= n 0)
    (begin
      (display ", el jugador 1 ha ganado ") (display ganadas) (display ", ha empatado ") (display empatadas) (display " y ha perdido ") (displayln perdidas)
    )
    (let (
          [partida (jugar tablero jugador-inicial estrategiaj1 estrategiaj2 0)]
          )
      (cond
        [(> partida 0) (multi-jugar (- n 1) (siguiente-jugador jugador-inicial) (+ ganadas 1) perdidas empatadas estrategiaj2 estrategiaj1)]
        [(< partida 0) (multi-jugar (- n 1) (siguiente-jugador jugador-inicial) ganadas (+ perdidas 1) empatadas estrategiaj2 estrategiaj1)]
        [else (multi-jugar (- n 1) (siguiente-jugador jugador-inicial) ganadas perdidas (+ empatadas 1) estrategiaj2 estrategiaj1)]
        )
      )
  )
)