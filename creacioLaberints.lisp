(setq lWidth 10)
(setq lHeight 10)
(setq rs (make-random-state t))
(setq entradaX (+ (random lWidth rs) 1))
(setq entradaY (+ (random lHeight rs) 1))
(setq actualX entradaX)
(setq actualY entradaY)

(setq mi 14)  ; tamaño de un cuadrado ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(putprop 'colors '(0 0 0) 'paret) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(putprop 'colors '(255 255 255) 'cami) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun genera ()
    ; (escriuLaberint 'nom (genera-contingut (crea-cami (crea-matriu lWidth lHeight) actualX actualY (crea-llista-random '())) lWidth lHeigh))
    (escriuLaberint "prueba.txt" (generaContingut (crea-cami (crea-matriu lWidth lHeight) actualX actualY (crea-llista-random '()))))
)

; algoritmo DFS
(defun crea-cami (m x y r)
; (get-key)
; (pinta)
    (cond
        ((null r) m)
        ((and (= (car r) 0) (= (contador-camins-veins m (+ x 1) y) 1) (equal (obtenir-posicio m (+ x 1) y) 'paret))
            (crea-cami (crea-cami (actualitza-posicio m (+ x 1) y 'cami) (+ x 1) y (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 1) (= (contador-camins-veins m (- x 1) y) 1) (equal (obtenir-posicio m (- x 1) y) 'paret))
            (crea-cami (crea-cami (actualitza-posicio m (- x 1) y 'cami) (- x 1) y (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 2) (= (contador-camins-veins m x (+ y 1)) 1) (equal (obtenir-posicio m x (+ y 1)) 'paret))
            (crea-cami (crea-cami (actualitza-posicio m x (+ y 1) 'cami) x (+ y 1) (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 3) (= (contador-camins-veins m x (- y 1)) 1) (equal (obtenir-posicio m x (- y 1)) 'paret))
            (crea-cami (crea-cami (actualitza-posicio m x (- y 1) 'cami) x (- y 1) (crea-llista-random '())) x y (cdr r)))
        (t (crea-cami m x y (cdr r)))
    )
)

(defun crea-sortida (m)

)

; -----------------------------? (and (>= x 1) (>= y 1) (< x (- lWidth 1)) (< y (- lHeight 1)))
(defun limites (m x y)
    (and (>= x 0) (>= y 0) (< x lWidth) (< y lHeight))
)
; -----------------------------?

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
        (t (crea-llista-random (cons (random 4 rs) l)))
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
        ((<= y 0) (obtenir-posicio-exacta (car l) x))
        (t (obtenir-posicio (cdr l) x (- y 1)))
    )
)

(defun obtenir-posicio-exacta (l x)
    (cond
        ((<= x 0) (car l))
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

(defun generaContingut (m)
  (cond
    ((null m) '())
    (t (append (generaContingutFila (car m)) (generaContingut (cdr m))))
  )
)

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




(defun escriuLaberint (nom contingut)  
    (let ((fp (open nom :direction :output)))
        (escriu-intern fp contingut)
        (close fp)
    )
)

;(escriu "text2.txt" '(#\h #\o #\l #\a #\Space #\c
;#\o #\m #\Space #\Newline #\Space #\Space #\v #\a
;#\Space #\a #\i #\x #\o #\. #\Newline))

(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp) (escriu-intern fp (cdr contingut)))
    )
)

(print (genera))