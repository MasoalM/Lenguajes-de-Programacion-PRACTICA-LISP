(setq lWidth 10)
(setq lHeight 10)
(setq entradaX (+ (random lWidth) 1))
(setq entradaY (+ (random lHeight) 1))
(setq actualX entradaX)
(setq actualY entradaY)

(defun genera ()
    ; abrir fichero en modo escribir
    ; let de nombre f al fichero
    ; (escribir f (crea-matriu ...))
    
    (crea-cami (crea-matriu lWidth lHeight) actualX actualY)
)

; algoritmo DFS
(defun crea-cami (l x y)
    ; moverme en posición random y modificar x e y según donde vaya avanzando
    
)

(defun crea-matriu (x y)
    (cond
        ((= 0 x) '())
        (t (append (crea-matriu (- x 1) y) (list (crea-fila x y))))
    )
)

(defun crea-fila (x y)
    (cond
        ((= 0 y) '())
        ((and (= entradaX x) (= entradaY y)) (cons 'entrada (crea-fila x (- y 1))))
        (t (cons 'paret (crea-fila x (- y 1))))
    )
    
)

(print (genera))