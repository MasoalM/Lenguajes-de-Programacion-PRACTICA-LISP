; Título: Juego Laberinto LISP
; Autor: Marcos Socías Alberto
; Fecha última modificación: 01/04/2025

(setq xi 10)
(setq yi 10)
(setq m 25)
(setq p '(2 2))
(setq lWidth 10)
(setq lHeigh 10)

(putprop 'colors '(0 0 0) 'negro)

(putprop 'colors '(255 0 0) 'rojo)

(putprop 'colors '(0 255 0) 'verde)

(putprop 'colors '(0 0 255) 'azul)

(putprop 'colors '(0 255 255) 'cian)


(setq l (list 'negro 'negro 'negro 'negro 'negro))

(defun quadrat (x) 
    (drawrel x 0)
    (drawrel 0 x)
    (drawrel (- x) 0)
    (drawrel 0 (- x))
)

(defun canvia (l i v)
    (cond
        ((= i 0) ((cons v (cdr l))))
        (t (cons (car l) (canvia (cdr l) (- i 1) v)))
    )
)

(defun pinta (l p)
    (cond
        ((null l) nil)
        (t 
            ; pintar casilla
            (apply 'color (get 'colors (car l)))
            (quadrat (- m 1))
            ; pintar jugador
            (cond
                ((= p 0) 
                    (apply 'color (get 'colors 'cian))
                    (moverel 2 2)
                    (quadrat (- m 5))
                    (moverel -2 -2)
                )
            )
            ; siguiente casilla
            (moverel m 0)
            (pinta (cdr l) (- p 1))
        )
    )
)

(defun dreta (p)
    (mod (+ (car p) 1) lWidth)
)

(defun esquerra (p)
    (cond
        ((< (- (car p) 1) 0) (+ (car p) 4))
        (t (mod (- (car p) 1) lWidth))
    )
)

;(defun amunt (p)

;)

;(defun abaix (p)

;)

(defun passa (l p)
    (cls)
    (move xi yi)
    (pinta l p)
    (cond
        ((every '/= '(0 0 0 0 0) (get 'colors (car l))) l)
        (t 
            (setq tecla (get-key))
            (cond
                ; si la tecla pulsada es A o flecha izquierda
                ((or (= 65 tecla) (= 97 tecla) (= 331 tecla)) (passa l (esquerra p)))
                ; si la tecla pulsada es D o flecha derecha
                ((or (= 68 tecla) (= 100 tecla) (= 333 tecla)) (passa l (dreta p)))
                ; si la tecla pulsada es W o flecha arriba
                ((or (= 87 tecla) (= 119 tecla) (= 328 tecla)) (passa l (amunt p)))
                ; si la tecla pulsada es S o flecha abajo
                ((or (= 83 tecla) (= 115 tecla) (= 336 tecla)) (passa l (abaix p)))
                ; si la tecla pulsada es 0-3
                ((<= 48 tecla 51) (passa (canvia l p tecla) p))
                ; si la tecla pulsada es ESC
                ((= 27 tecla) l)
                ; si no, se llama recursivamente tal cual
                (t (passa l p))
            )
        )
    )
)

(passa l p)