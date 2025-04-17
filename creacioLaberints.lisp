(setq lWidth 10)
(setq lHeight 10)
(setq entradaX (+ (random lWidth) 1))
(setq entradaY (+ (random lHeight) 1))
(setq actualX entradaX)
(setq actualY entradaY)

(setq mi 14)  ; tamaño de un cuadrado ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(putprop 'colors '(0 0 0) 'paret) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(putprop 'colors '(255 255 255) 'cami) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun genera ()
    ; abrir fichero en modo escribir
    ; let de nombre f al fichero
    ; (escribir f (crea-matriu ...))
    
    (crea-cami (crea-matriu lWidth lHeight) actualX actualY (crea-llista-random '()))
)

; algoritmo DFS
(defun crea-cami (m x y r)
    ;(print m)
    (cond
        ((null (car r)) m)
        ((and (= (car r) 0) (= (contador-camins-veins m (+ x 1) y) 1) (equal (obtenir-posicio m x y) 'paret)) 
            (crea-cami (actualitza-posicio m (+ x 1) y 'cami) (+ x 1) y (crea-llista-random '())) 
        )
        ((and (= (car r) 1) (= (contador-camins-veins m (- x 1) y) 1) (equal (obtenir-posicio m x y) 'paret))
            (crea-cami (actualitza-posicio m (- x 1) y 'cami) (- x 1) y (crea-llista-random '()))
        )
        ((and (= (car r) 2) (= (contador-camins-veins m x (+ y 1)) 1) (equal (obtenir-posicio m x y) 'paret))
            (crea-cami (actualitza-posicio m x (+ y 1) 'cami) x (+ y 1) (crea-llista-random '()))
        )
        ((and (= (car r) 3) (= (contador-camins-veins m x (- y 1)) 1) (equal (obtenir-posicio m x y) 'paret))
            (crea-cami (actualitza-posicio m x (- y 1) 'cami) x (- y 1) (crea-llista-random '()))
        )
        ;((>= (contador-camins-veins m x y) 2) m) ; demasiadas conexiones
        (t (crea-cami m x y (cdr r)))
    )
)

(defun actualitza-posicio (l x y nou-valor)
    (cond
        ((null l) nil)
        ((= y 0) (cons (actualitza-posicio-fila (car l) x nou-valor) (cdr l)))
        (t (cons (car l) (actualitza-posicio (cdr l) x (- y 1) nou-valor)))
    )
)

(defun actualitza-posicio-fila (fila x nou-valor)
    (cond
        ((null fila) nil)
        ((= x 0) (cons nou-valor (cdr fila)))
        (t (cons (car fila) (actualitza-posicio-fila (cdr fila) (- x 1) nou-valor)))
    )
)

(defun crea-llista-random (l)
    (cond
        ((and (pertany 0 l) (pertany 1 l) (pertany 2 l) (pertany 3 l)) (fer-conjunt l))
        (t (crea-llista-random (cons (random 4) l)))
    )
)

(defun pertany (x l)
    (cond 
        ((null l) nil)
        ((equal x (car l)) t)
        (t (pertany x (cdr l)))
    )
)

(defun fer-conjunt (l)
    (cond 
        ((null l) nil)
        ((pertany (car l) (cdr l)) (fer-conjunt (cdr l)))
        (t (cons (car l) (fer-conjunt(cdr l))))
    )
)

(defun contador-camins-veins (l x y)
    (+  (cond ((not (equal (obtenir-posicio l (+ x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l (- x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (+ y 1)) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (- y 1)) 'paret)) 1) (t 0)))
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