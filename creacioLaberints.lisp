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
    (let z (random 4))
    (cond
        ((and (= z 0) (= (contador-camins-veins l (+ x 1) y) 1)) (crea-cami l (+ x 1) y)) ; and es pared and no se junta con camino (excepto el actual)
        ((and (= z 1) (= (contador-camins-veins l (- x 1) y) 1)) (crea-cami l (- x 1) y))
        ((and (= z 2) (= (contador-camins-veins l x (+ y 1)) 1)) (crea-cami l x (+ y 1)))
        ((and (= z 3) (= (contador-camins-veins l x (- y 1)) 1)) (crea-cami l x (+ y 1)))
        ; cómo hago para que si falla uno, pruebe otros antes del caso final?
        (t )
    )
)

(defun contador-camins-veins (l x y)
    (let c 0)
    (cond ((not (eq (obtenir-posicio l (+ x 1) y) 'paret)) (+ c 1)))  ; eq, = o equal¿?
    (cond ((not (eq (obtenir-posicio l (- x 1) y) 'paret)) (+ c 1)))
    (cond ((not (eq (obtenir-posicio l x (+ y 1)) 'paret)) (+ c 1)))
    (cond ((not (eq (obtenir-posicio l x (- y 1)) 'paret)) (+ c 1)))
    ; cómo hago que devuelva c?
)

(defun obtenir-posicio (l x y)
    (cond
        ((= y 0) (obtenir-posicio-exacta (car l) x))
        (t (obtenir-posicio (cdr l) x (- y 1)))
    )
)

(defun obtenir-posicio-exacta (l x)
    (cond
        ((= x 0) (car l))
        (t (obtenir-posicio-exacta (cdr l) (- x 1)))
    )
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