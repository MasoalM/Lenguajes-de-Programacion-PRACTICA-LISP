; Título: Juego Laberinto LISP
; Autores: Marcos Socías Alberto y Hugo Valls Sabater
; Fecha última modificación: 21/04/2025
; Asignatura: Lenguajes de Programación
; Grupo: 102
; Profesor teoría: Antoni Oliver Tomàs
; Profesor prácticas: Francesc Xavier Gaya Morey
; Convocatoria: Ordinaria
; ---------------------------------- PONER MÁS COSAS
; EXTRAS: SE EVITAN ESQUINAS

(setq rs (make-random-state t))

(defun genera (nom x y)
    (setq lWidth x)
    (setq lHeight y)
    (setq entradaX (+ (random (- lWidth 2) rs) 1))
    (setq entradaY (+ (random (- lHeight 2) rs) 1))
    (setq actualX entradaX)
    (setq actualY entradaY)
    (escriuLaberint nom (generaContingut (crea-sortida (crea-cami (crea-matriu 0 lWidth lHeight) actualX actualY (crea-llista-random '())))))
)

; algoritmo DFS
(defun crea-cami (m x y r)
    (cond
        ((null r) m)
        ((and (= (car r) 0) (= (contador-camins-veins m (+ x 1) y) 1) (= (contador-esquines-veines m (+ x 1) y 0) 0) (equal (obtenir-posicio m (+ x 1) y) 'paret) (limites (+ x 1) y))
            (crea-cami (crea-cami (actualitza-posicio m (+ x 1) y 'cami) (+ x 1) y (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 1) (<= (contador-camins-veins m (- x 1) y) 1) (= (contador-esquines-veines m (- x 1) y 1) 0) (equal (obtenir-posicio m (- x 1) y) 'paret) (limites (- x 1) y))
            (crea-cami (crea-cami (actualitza-posicio m (- x 1) y 'cami) (- x 1) y (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 2) (<= (contador-camins-veins m x (+ y 1)) 1) (= (contador-esquines-veines m x (+ y 1) 2) 0) (equal (obtenir-posicio m x (+ y 1)) 'paret) (limites x (+ y 1)))
            (crea-cami (crea-cami (actualitza-posicio m x (+ y 1) 'cami) x (+ y 1) (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 3) (<= (contador-camins-veins m x (- y 1)) 1) (= (contador-esquines-veines m x (- y 1) 3) 0) (equal (obtenir-posicio m x (- y 1)) 'paret) (limites x (- y 1)))
            (crea-cami (crea-cami (actualitza-posicio m x (- y 1) 'cami) x (- y 1) (crea-llista-random '())) x y (cdr r)))
        (t (crea-cami m x y (cdr r)))
    )
)

(defun crea-sortida (m)
    (setq randomX (+ (random (- lWidth 2) rs) 1))
    (setq randomY (+ (random (- lHeight 2) rs) 1))
    (cond
        ((equal (obtenir-posicio m randomX randomY) 'cami) (actualitza-posicio m randomX randomY 'sortida)) 
        (t (crea-sortida m))
    )
)

; Función que verifica si una posición está dentro de los límites del laberinto
(defun limites (x y)
    (and (> x 0) (> y 0) (< x lWidth) (< y lHeight))
)

; Función que actualiza el tipo de una celda del laberinto en la posición dada
(defun actualitza-posicio (l x y nou-valor)
    (cond
        ((null l) nil)
        ((= y 0) (cons (actualitza-posicio-fila (car l) x nou-valor) (cdr l)))
        (t (cons (car l) (actualitza-posicio (cdr l) x (- y 1) nou-valor)))
    )
)
; Función que actualiza el tipo de una celda en una fila
(defun actualitza-posicio-fila (fila x nou-valor)
    (cond
        ((null fila) nil)
        ((= x 0) (cons nou-valor (cdr fila)))
        (t (cons (car fila) (actualitza-posicio-fila (cdr fila) (- x 1) nou-valor)))
    )
)
; genera una lista de direcciones aleatorias para el recorrido
(defun crea-llista-random (l)
    (cond
        ((and (pertany 0 l) (pertany 1 l) (pertany 2 l) (pertany 3 l)) (fer-conjunt l))
        (t (crea-llista-random (cons (random 4 rs) l)))
    )
)
; verifica si un elemento pertenece a una lista
(defun pertany (x l)
    (cond 
        ((null l) nil)
        ((equal x (car l)) t)
        (t (pertany x (cdr l)))
    )
)
;  elimina los elementos duplicados en una lista (lo usamos para la lista random)
(defun fer-conjunt (l)
    (cond 
        ((null l) nil)
        ((pertany (car l) (cdr l)) (fer-conjunt (cdr l)))
        (t (cons (car l) (fer-conjunt(cdr l))))
    )
)
; comprueba si estamos en un camino legal, mirando el tipo de casillas vecinas
; MEJORAR PORQUE SOLO USA UN COND Y SALE
(defun contador-camins-veins (l x y)
    (+  (cond ((not (equal (obtenir-posicio l (+ x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l (- x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (+ y 1)) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (- y 1)) 'paret)) 1) (t 0))
    )
)

(defun contador-esquines-veines (l x y dir)
    (cond
        ; DERECHA (0)
        ((= dir 0) (+
            (cond ((not (equal (obtenir-posicio l (+ x 1) (+ y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (+ x 1) (- y 1)) 'paret)) 1) (t 0))
        ))
        ; IZQUIERDA (1)
        ((= dir 1) (+
            (cond ((not (equal (obtenir-posicio l (- x 1) (+ y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (- x 1) (- y 1)) 'paret)) 1) (t 0))
        ))
        ; ARRIBA (2)
        ((= dir 2) (+  
            (cond ((not (equal (obtenir-posicio l (+ x 1) (+ y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (- x 1) (+ y 1)) 'paret)) 1) (t 0))
        ))
        ; ABAJO (3)
        ((= dir 3) (+
            (cond ((not (equal (obtenir-posicio l (- x 1) (- y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (+ x 1) (- y 1)) 'paret)) 1) (t 0))
        ))
    )
)


; Obtiene el valor de una posición en el laberinto
(defun obtenir-posicio (l x y)
    (cond
        ((<= y 0) (obtenir-posicio-exacta (car l) x))
        (t (obtenir-posicio (cdr l) x (- y 1)))
    )
)
; Obtiene el valor exacto de una posición en una fila
(defun obtenir-posicio-exacta (l x)
    (cond
        ((<= x 0) (car l))
        (t (obtenir-posicio-exacta (cdr l) (- x 1)))
    )
)
; Crea una matriz de tamaño lHeight con filas vacías
(defun crea-matriu (y anchura altura)
    (cond
        ((= y altura) '())
        (t (cons (crea-fila 0 anchura y) (crea-matriu (+ y 1) anchura altura)))
    )
)
; Crea una fila de celdas llena de paredes
(defun crea-fila (x anchura y)
    (cond
        ((= x anchura) '())
        ((and (= entradaX x) (= entradaY y)) (cons 'entrada (crea-fila (+ x 1) anchura y)))
        (t (cons 'paret (crea-fila (+ x 1) anchura y)))
    )
)
; Función que genera el contenido del laberinto 
(defun generaContingut (m)
  (cond
    ((null m) '())
    (t (append (generaContingutFila (car m)) (generaContingut (cdr m))))
  )
)
; segun el tipo de casilla añadimos una serie de caracteres al archivo
(defun generaContingutFila (fila)
  (cond
    ((null fila) (list #\Newline))
    ((equal (car fila) 'paret)    (cons #\# (generaContingutFila (cdr fila))))
    ((equal (car fila) 'cami)     (cons #\. (generaContingutFila (cdr fila))))
    ((equal (car fila) 'entrada)  (cons #\e (generaContingutFila (cdr fila))))
    ((equal (car fila) 'sortida)  (cons #\s (generaContingutFila (cdr fila))))
    (t                            (cons #\? (generaContingutFila (cdr fila))))
  )
)
; Escribe el contenido generado en un archivo especificado
(defun escriuLaberint (nom contingut)  
    (let ((fp (open nom :direction :output)))
        (escriu-intern fp contingut)
        (close fp)
    )
)

(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp) (escriu-intern fp (cdr contingut)))
    )
)

(print (genera "pruebanueva.txt" 25 25))