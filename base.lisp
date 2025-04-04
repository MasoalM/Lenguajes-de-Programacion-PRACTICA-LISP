; Título: Juego Laberinto LISP
; Autor: Marcos Socías Alberto
; Fecha última modificación: 04/04/2025

(setq xi 10)
(setq yi 10)
(setq m 25)
(setq px 2)
(setq py 2)
(setq lWidth 10)
(setq lHeight 10)

(putprop 'colors '(0 0 0) 'negro)

(putprop 'colors '(255 0 0) 'rojo)

(putprop 'colors '(0 255 0) 'verde)

(putprop 'colors '(0 0 255) 'azul)

;(putprop 'colors '(0 255 255) 'cian)


(setq l (list '(negro negro negro negro negro negro negro negro negro negro)    ; HACERLO AUTOMÁTICO
              '(negro negro negro negro negro negro negro negro negro negro)
              '(negro negro negro negro negro negro negro negro negro negro) 
              '(negro negro negro negro negro negro negro negro negro negro)    ; HACERLO AUTOMÁTICO
              '(negro negro negro negro negro negro negro negro negro negro)
              '(negro negro negro negro negro negro negro negro negro negro)
              '(negro negro negro negro negro negro negro negro negro negro)    ; HACERLO AUTOMÁTICO
              '(negro negro negro negro negro negro negro negro negro negro)
              '(negro negro negro negro negro negro negro negro negro negro)
              '(negro negro negro negro negro negro negro negro negro negro)))

(defun quadrat (x)
    (drawrel x 0)
    (drawrel 0 x)
    (drawrel (- x) 0)
    (drawrel 0 (- x))
)

(defun columna-quadrats (n l px py)
    (cond
        ((= n 0) nil)
        (t
            (apply 'color (get 'colors (car l)))    ; TOO FEW ARGUMENTS: Falla cuando hago (apply 'color (get 'colors (car l)))
            (moverel 0 m)
            (quadrat (- m 1))
            ; pintar (o no) al jugador
            (cond
                ((and (= px 0) (= py 0)) 
                    (apply 'color (get 'colors 'verde))
                    (moverel 2 2)
                    (quadrat (- m 5))
                    (moverel -2 -2)
                )
            )
            (columna-quadrats (- n 1) (cdr l) px (- py 1))
        )
    )
)

; SIN ADAPTAR AL 2D
(defun canvia (l i v)
    (cond
        ((= i 0) ((cons v (cdr l))))
        (t (cons (car l) (canvia (cdr l) (- i 1) v)))
    )
)

(defun pinta (l px py)
    (cond
        ((null l) nil)
        (t 
            (columna-quadrats lHeight (car l) px py)
            ; siguiente columna
            (moverel m (- (* m lHeight)))
            (pinta (cdr l) (- px 1) py)
        )
    )
)

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
        ; si la tecla pulsada es 0-3
        ;((<= 48 tecla 51) (passa (canvia l p tecla) p)) ; SIN ADAPTAR A 2D
        ; si la tecla pulsada es ESC
        ((= 27 tecla) l)
        ; si no, se llama recursivamente tal cual
        (t (passa l px py))
    )
)

(passa l px py)