#lang racket

(module+ test (require rackunit))

(define (get-initial-state) (cons 1 1))
(module+ test (check-equal? (get-initial-state) (cons 1 1)))

(define (get-end-state rows columns) (cons (- rows 2) (- columns 2)))
(module+ test (check-equal? (get-end-state 10 10) (cons 8 8)))

(define (generate-positions rows columns)
  (define all-positions (for*/list ([i rows][j columns]) (cons i j)))
  (let ([initial-state (get-initial-state)]
        [end-state (get-end-state rows columns)])
  (remove initial-state (remove end-state all-positions))))

(module+ test (test-begin
  (define positions (generate-positions 5 5))
  (check-equal? (length positions) 23)
  (check-false (member (get-initial-state) positions))
  (check-false (member (get-end-state 5 5) positions))))

(define (pick-random picked source n)
  (if (= n 0)
      (values source picked)
      (let* (
             [r (random (length source))]
             [value (list-ref source r)]
             )
        (pick-random (cons value picked) (remove value source) (- n 1))
      )
  )
)

(module+ test (test-begin
   (define-values (remaining picked) (pick-random '() (list 1 2 3) 2))
   (check-equal? (length remaining) 1)
   (check-equal? (length picked) 2)
   (check-not-false (or (member 1 remaining) (member 1 picked)))
   (check-not-false (or (member 2 remaining) (member 2 picked)))
   (check-not-false (or (member 3 remaining) (member 3 picked)))
   ))

(define (generate-room-recursive i j rows columns block-positions door-positions superlist sublist)
  (if (= i rows)
      (reverse superlist)
        (let (
              [character
                (cond [(member (cons i j) block-positions) "#"]
                      [(member (cons i j) door-positions) ":"]
                      [(equal? (cons i j) (get-initial-state)) "x"]
                      [(equal? (cons i j) (get-end-state rows columns)) "X"]
                      [else "_"])]
              )
          (if (= j (- columns 1))
              (generate-room-recursive (+ i 1) 0 rows columns block-positions door-positions (cons (reverse (cons character sublist)) superlist) empty)
              (generate-room-recursive i (+ j 1) rows columns block-positions door-positions superlist (cons character sublist))
          )
        )
  )
)

(define (generate-room rows columns blocks doors)
  (define-values (free-positions block-positions) (pick-random '() (generate-positions rows columns) blocks))
  (define-values (final-free-positions door-positions) (pick-random '() free-positions doors))
  (define empty-list (build-list (* rows columns) (lambda (x) 0)))
  
  (generate-room-recursive 0 0 rows columns block-positions door-positions '() '())
)

(define (contar-filas casilla filas n)
  (if (empty? filas)
      n
      (if (equal? (car filas) casilla)
          (contar-filas casilla (cdr filas) (+ n 1))
          (contar-filas casilla (cdr filas) n)
      )
   )
)

(define (contar casilla mazmorra-test n)
  (if (empty? mazmorra-test)
      n
      (contar casilla (cdr mazmorra-test) (contar-filas casilla (car mazmorra-test) n))
  )
)

(module+ test (test-begin
               (define mazmorra-test (generate-room 10 10 20 7))
               (check-equal? (length mazmorra-test) 10)
               (check-equal? (length (car mazmorra-test)) 10)
               (check-equal? (celda (cons 1 1)) "x")
               (check-equal? (celda (cons 8 8)) "X")
               (check-equal? (contar "x" mazmorra-test 0) 1)
               (check-equal? (contar "X" mazmorra-test 0) 1)
               (check-equal? (contar "#" mazmorra-test 0) 20)
               (check-equal? (contar ":" mazmorra-test 0) 7)
               )
)

(define mazmorra (generate-room 10 10 20 7))

(define filas (length mazmorra))
(define columnas (length (car mazmorra)))

(define inicio (cons 1 1))
(define meta (cons (- filas 2) (- columnas 2)))

(define (pintar-celda p)
  (if (equal? (celda p) "_")
      (display "o")
      (display "ç")
  )
  (when (not (= (posicion-columna p) (- columnas 1))) (display " "))
)

(define (pintar camino p)
  (let* (
        [fila (posicion-fila p)]
        [columna (posicion-columna p)]
        [siguiente-posicion (cons fila (+ columna 1))]

        )
    (when (= columna 0) (display "("))
    (cond
      [(and (>= fila (- filas 1)) (> columna (- columnas 1))) (displayln ")") (newline)]
      [(> columna  (- columnas 1)) (displayln ")") (pintar camino (cons (+ fila 1) 0))]
      [(list? (member p camino)) (pintar-celda p) (pintar camino siguiente-posicion)]
      [(= columna (- columnas 1)) (display (celda p)) (pintar camino siguiente-posicion)]
      [else (display (string-append (celda p) " ")) (pintar camino siguiente-posicion)]
      )
  )
)

(define (posicion-fila p) (car p))

(module+ test (check-equal? (posicion-fila (cons 1 2)) 1))

(define (posicion-columna p) (cdr p))

(module+ test (check-equal? (posicion-columna (cons 1 2)) 2))

(define (celda p)
  (list-ref (list-ref mazmorra (posicion-fila p)) (posicion-columna p))
)

(module+ test (test-begin
               (check-equal? (celda inicio) "x")
               (check-equal? (celda meta) "X")
              )
)

(define (es-camino? p)
  (not (equal? (celda p) "#"))
)

(module+ test (check-not-false (es-camino? inicio)))

(define (arriba p)
  (let* (
        [fila (- (posicion-fila p) 1)]
        [nueva-posicion (cons fila (posicion-columna p))]
        )
    nueva-posicion
  )
)

(module+ test (check-equal? (arriba inicio) (cons 0 1)))

(define (abajo p)
  (let* (
        [fila (+ (posicion-fila p) 1)]
        [nueva-posicion (cons fila (posicion-columna p))]
        )
    nueva-posicion
  )
)

(module+ test (check-equal? (abajo inicio) (cons 2 1)))

(define (izquierda p)
  (let* (
        [columna (- (posicion-columna p) 1)]
        [nueva-posicion (cons (posicion-fila p) columna)]
        )
    nueva-posicion
  )
)

(module+ test (check-equal? (izquierda inicio) (cons 1 0)))

(define (derecha p)
  (let* (
        [columna (+ (posicion-columna p) 1)]
        [nueva-posicion (cons (posicion-fila p) columna)]
        )
    nueva-posicion
  )
)

(module+ test (check-equal? (derecha inicio) (cons 1 2)))

(define (sucesor-valido? p)
  (let* (
       [hijo (car p)]
       [fila (posicion-fila hijo)]
       [columna (posicion-columna hijo)]
       )
    (and (>= fila 0) (< fila filas) (>= columna 0) (< columna columnas) (es-camino? hijo))
  )
)

(module+ test (test-begin
               (check-not-false (sucesor-valido? (cons inicio inicio)))
               (check-false (sucesor-valido? (cons (cons -1 1) inicio)))
               (check-false (sucesor-valido? (cons (cons +inf.0 1) inicio)))
               (check-false (sucesor-valido? (cons (cons 1 -1) inicio)))
               (check-false (sucesor-valido? (cons (cons 1 +inf.0) inicio)))
              )
)

(define (sucesores p)
  (filter sucesor-valido? (list (cons (arriba p) p) (cons (abajo p) p) (cons (izquierda p) p) (cons (derecha p) p)))
)

(module+ test (check-equal? (length (sucesores (cons -5 -5))) 0))

(define (en-lista? elemento lista)
  (cond
    [(empty? lista) #f]
    [(equal? elemento (caar lista)) #t]
    [else (en-lista? elemento (cdr lista))]
  )
)

(module+ test (test-begin
               (check-false (en-lista? inicio '()))
               (check-not-false (en-lista? inicio (list (cons meta (cons 3 4)) (cons inicio (cons 2 3)))))
               )
)

(define (es-salida? p)
  (equal? (celda p) "x")
)

(module+ test (test-begin
               (check-not-false (es-salida? inicio))
               (check-false (es-salida? (cons 0 0)))
               )
)

(define (es-meta? p)
  (equal? (celda p) "X")
)

(module+ test (test-begin
               (check-not-false (es-meta? meta))
               (check-false (es-meta? inicio))
               )
)

(define (backtrack-antiguo camino hijos cerrados)
  (let (
        [cerrado (car cerrados)]
       )
    (cond
      [(es-salida? cerrado) camino]
      [(list? (member cerrado hijos)) (backtrack-antiguo (append (list cerrado) camino) (sucesores cerrado) (remove cerrado cerrados))]
      [else (backtrack-antiguo camino hijos (cdr cerrados))]
    )
  )
)

(module+ test (check-equal? (length (backtrack-antiguo (list) (list (cons 0 0) inicio) (list (cons 1 2) (cons 0 0) inicio))) 1))

(define (obtener-padre hijo cerrados)
  (if (equal? (caar cerrados) hijo)
      (cdar cerrados)
      (obtener-padre hijo (cdr cerrados))
  )
)

(module+ test (check-equal? (obtener-padre inicio (list (cons meta (cons 1 2)) (cons inicio (cons 0 1)))) (cons 0 1)))

(define (backtrack camino hijo cerrados)
  (let (
        [padre (obtener-padre hijo cerrados)]
       )
    (if (es-salida? hijo)
        (cdr camino)
        (backtrack (append (list padre) camino) padre cerrados)
    )
  )
)

(module+ test (check-equal? (length (backtrack (list (cons 0 0) (cons 0 1)) (cons 1 2) (list (cons (cons 1 2) inicio) (cons inicio (cons 0 0))))) 2))

(define (busqueda-anchura abiertos cerrados)
  (if (empty? abiertos) #f
      (let* (
            [actual (caar abiertos)]
            [hijo-padre (car abiertos)]
            [resto (cdr abiertos)]
            [hijos (sucesores actual)]
            )
        (cond
          [(es-meta? actual) (backtrack (list) actual (reverse (append cerrados (list hijo-padre))))]
          [(en-lista? actual cerrados) (busqueda-anchura resto cerrados)]
          [else (busqueda-anchura (append resto hijos) (append cerrados (list hijo-padre)))]
        )
      )
  )
)

(module+ test (test-begin
               (check-false (busqueda-anchura (list) (list)))
               (check-equal? (length (busqueda-anchura (list (cons inicio (cons 0 0)) (cons inicio (cons 2 3)) (cons meta inicio)) (list))) 0)
               )
)

(define (obtener-coste p)
  (if (or (equal? (celda p) "_") (equal? (celda p) "X"))
      1
      4
   )
)

(module+ test (test-begin
               (check-equal? (obtener-coste meta) 1)
               (check-equal? (obtener-coste inicio) 4)
               )
)

(define (manhattan p)
  (+ (abs (- (posicion-fila p) (posicion-fila meta))) (abs (- (posicion-columna p) (posicion-columna meta))))
)

(module+ test (check-equal? (manhattan meta) 0))

(define (calcular-prioridad actual)
  (+ (obtener-coste actual) (manhattan actual))
)

(module+ test (check-equal? (calcular-prioridad meta) 1))

(define (sumar-coste hijos coste lista)
  (if (empty? hijos) lista
      (let (
            [hijo-padre (car hijos)]
            [hijo (caar hijos)]
            )
    
        (sumar-coste (cdr hijos) coste (append lista (list (cons hijo-padre (+ (obtener-coste hijo) coste)))))
       )
  )
)

(module+ test (check-equal? (length (sumar-coste (list (cons inicio meta)) 0 (list))) 1))

(define (ordenar abiertos hijos coste actual)
  (sort (append abiertos (sumar-coste hijos coste (list))) #:key (lambda (x) (+ (cdr x) (manhattan actual))) <)
)

(module+ test (check-equal? (length (ordenar (list) (list (cons inicio meta) (cons inicio (cons 0 0))) 0 inicio)) 2))

(define (busqueda-a-estrella abiertos cerrados)
  (if (empty? abiertos) #f
      (let* (
            [actual (caaar abiertos)]
            [hijo-padre (caar abiertos)]
            [resto (cdr abiertos)]
            [hijos (sucesores actual)]
            [coste (cdar abiertos)]
            )
        (cond
          [(es-meta? actual) (backtrack (list) actual (reverse (append cerrados (list hijo-padre))))]
          [(en-lista? actual cerrados) (busqueda-a-estrella resto cerrados)]
          [else (busqueda-a-estrella (ordenar resto hijos coste actual) (append cerrados (list hijo-padre)))]
        )
      )
  )
)

(module+ test (test-begin
               (check-false (busqueda-a-estrella (list) (list)))
               (check-equal? (length (busqueda-a-estrella (list (cons (cons inicio (cons 0 0)) 0) (cons (cons inicio (cons 2 3)) 0) (cons (cons meta inicio) 0)) (list))) 0)
               )
)

(define (calcula-coste camino)
  (if (empty? camino)
      0
      (if (equal? (celda (car camino)) "_")
          (+ (calcula-coste (cdr camino)) 1)
          (+ (calcula-coste (cdr camino)) 4)
       )
   )
)

(module+ test (check-pred number? (calcula-coste (list inicio (cons 0 1) (cons 1 0) (cons 1 2) (cons 2 1)))))


(define (pintar-coste-mazmorra camino)
  (if camino
      (begin
              (display "COST: ")
              (displayln (+ (calcula-coste camino) 1))
              (pintar camino (cons 0 0))
      )
      (displayln "No hay camino")
   )
)

(define (run)
  (let (
        [camino-anchura (busqueda-anchura (list (cons inicio inicio)) (list))]
        [camino-a-estrella (busqueda-a-estrella (list (cons (cons inicio inicio) 0)) (list))]
       )
    (displayln "ROOM")
    (pintar (list) (cons 0 0))

    (displayln "SIMPLE PATH")
    (pintar-coste-mazmorra camino-anchura)

    (displayln "A* PATH")
    (pintar-coste-mazmorra camino-a-estrella)
  )
)

; Para el cover más fiable, comentar esto
(run)