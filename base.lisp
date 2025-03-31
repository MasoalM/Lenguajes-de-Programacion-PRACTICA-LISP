; Título: Juego Laberinto LISP
; Autor: Marcos Socías Alberto
; Fecha última modificación: 31/03/2025

(setq xi 10)
(setq yi 10)
(setq m 25)
(setq p 2)

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
            (apply 'color (get 'colors (car l)))
            (quadrat (- m 1))
            (moverel m 0)
            (cond
                ((= p 0) 
                    (apply 'color (get 'colors 'cian))
                    (moverel 2 2)
                    (quadrat (- m 5))
                    (moverel -2 -2)
                )
            )
            (pinta (cdr l) (- p 1))
        )
    )
)

(defun dreta (p)
    (mod (+ p 1) 5)
)

(defun esquerra (p)
    (cond
        ((< 0 (- p 1)) (+ p 4))
        (t (mod (+ p 1) 5))
    )
)

(defun passa (l p)
    (pinta l p)
    (cond
        ((apply '/= '0 'l) l)
        (t 
            (cond
                ; si la tecla pulsada es a
                (() ad l (esquerra p))
                ; si la tecla pulsada es d
                (() ad l (dreta p))
                ; si la tecla pulsada es 0-3
                (() numero (canvia l p tecla))
                ; si la tecla pulsada es ESC
                (() l)
            )
        )
    )
)

(defun ad (l p)

)

(defun numero (l p)

)

(cls)
(move xi yi)
(passa l p)