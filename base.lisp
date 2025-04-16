; Título: Juego Laberinto LISP
; Autores : Marcos Socías Alberto y Hugo Valls Sabater
; Fecha última modificación: 04/04/2025

(setq xi 10) ; posicion x donde empieza a pintar
(setq yi 10) ; posicion y donde empieza a pintar 
(setq m 14)  ; tamaño de un cuadrado
(setq px 10) ; posicion inicial x en la matriz del jugador   
(setq py 10) ; posicion inicial y en la matriz del jugador
(setq lWidth 25) ; anchura en cuadrados del laberinto
(setq lHeight 25) ; altura en cuadrados del laberinto

(putprop 'colors '(0 0 0) 'paret)

(putprop 'colors '(255 255 255) 'cami)

(putprop 'colors '(255 0 0) 'rojo)

(putprop 'colors '(0 255 0) 'verde)

(putprop 'colors '(0 0 255) 'azul)


(setq l (list '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret cami paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret cami paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret cami cami paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret cami paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret cami cami cami paret paret paret cami paret paret paret paret paret paret paret paret paret paret paret)  ; HACERLO ÓPTIMO
              '(paret paret paret paret paret paret paret paret paret cami paret paret paret cami paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret cami cami cami cami cami paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)
              '(paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret paret)))

(defun quadrat (x m)
    (drawrel 0 (+ m 1))
    (moverel 1 (- (+ m 1)))
    (cond
        ((> x 0) (quadrat (- x 1) m))
        (t (moverel (- (+ m 1)) 0))
    )
)

(defun fila-quadrats (n l px py)
    (cond
        ((= n 0) nil)
        (t
            (apply 'color (get 'colors (car l)))    ; TOO FEW ARGUMENTS: Falla cuando hago (apply 'color (get 'colors (car l)))
            (moverel m 0)
            (quadrat m m)
            ; pintar (o no) al jugador
            (cond
                ((and (= px 0) (= py 0)) 
                    (apply 'color (get 'colors 'verde))
                    (moverel 2 2)
                    (quadrat (- m 5) (- m 5))
                    (moverel -2 -2)
                )
            )
            (fila-quadrats (- n 1) (cdr l) (- px 1) py)
        )
    )
)

(defun pinta (l px py)
    (cond
        ((null l) nil)
        (t 
            (fila-quadrats lWidth (car l) px py)
            ; siguiente fila
            (moverel (- (* m lWidth)) m)
            (pinta (cdr l) px (- py 1))
        )
    )
)

; CREAR FUNCIÓN "obtenir-posicio" QUE OBTIENE EL COLOR DE UNA POSICIÓN A PARTIR DE PX y PY

(defun dreta (px)
    (mod (+ px 1) lWidth)
)

(defun esquerra (px)
    (cond
        ((< (- px 1) 0) (+ px (- lWidth 1)))
        (t (mod (- px 1) lWidth))
    )
)

(defun amunt (py)
    (mod (+ py 1) lHeight)
)

(defun abaix (py)
    (cond
        ((< (- py 1) 0) (+ py (- lHeight 1)))
        (t (mod (- py 1) lHeight))
    )
)

(defun passa (l px py)
    (cls)
    (move xi yi)
    (pinta l px py)
    (setq tecla (get-key))
    (cond
        ; si la tecla pulsada es A o flecha izquierda
        ((or (= 65 tecla) (= 97 tecla) (= 331 tecla)) (passa l (esquerra px) py))
        ; si la tecla pulsada es D o flecha derecha
        ((or (= 68 tecla) (= 100 tecla) (= 333 tecla)) (passa l (dreta px) py))
        ; si la tecla pulsada es W o flecha arriba
        ((or (= 87 tecla) (= 119 tecla) (= 328 tecla)) (passa l px (amunt py)))
        ; si la tecla pulsada es S o flecha abajo
        ((or (= 83 tecla) (= 115 tecla) (= 336 tecla)) (passa l px (abaix py)))
        ; si la tecla pulsada es ESC
        ((= 27 tecla) l)
        ; si no, se llama recursivamente tal cual
        (t (passa l px py))
    )
)

(passa l px py)