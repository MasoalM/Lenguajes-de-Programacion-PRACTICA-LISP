; Título: Juego Laberinto LISP
; Autores : Marcos Socías Alberto y Hugo Valls Sabater
; Fecha última modificación: 04/04/2025

(setq xi 20) ; posicion x donde empieza a pintar
(setq yi 0) ; posicion y donde empieza a pintar 
(setq m 14)  ; tamaño de un cuadrado
(setq px 5) ; posicion inicial x en la matriz del jugador   
(setq py 5) ; posicion inicial y en la matriz del jugador
(setq lWidth 25) ; anchura en cuadrados del laberinto
(setq lHeight 25) ; altura en cuadrados del laberinto

(putprop 'colors '(0 0 0) 'paret)

(putprop 'colors '(255 255 255) 'cami)

(putprop 'colors '(255 0 0) 'sortida)

(putprop 'colors '(0 255 0) 'jugador)

(putprop 'colors '(0 0 255) 'entrada)

(setq l (llegeixMatriu (llegeixLaberint "laberints_exemple/25x25_1.txt")))

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
            (apply 'color (get 'colors (car l)))
            (moverel m 0)
            (quadrat m m)
            ; pintar (o no) al jugador
            (cond
                ((and (= px 0) (= py 0)) 
                    (apply 'color (get 'colors 'jugador))
                    (moverel 2 3)
                    (quadrat (- m 5) (- m 5))
                    (moverel -2 -3)
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
            (moverel (- (* m lWidth)) (- m))
            (pinta (cdr l) px (- py 1))
        )
    )
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
    (move xi (+ yi (* m lHeight)))
    (pinta l px py)
    (setq tecla (get-key))
    (cond
        ; si la tecla pulsada es A o flecha izquierda
        ((and (or (= 65 tecla) (= 97 tecla) (= 331 tecla)) (not (equal (obtenir-posicio l (esquerra px) py) 'paret))) (passa l (esquerra px) py))
        ; si la tecla pulsada es D o flecha derecha
        ((and (or (= 68 tecla) (= 100 tecla) (= 333 tecla)) (not (equal (obtenir-posicio l (dreta px) py) 'paret))) (passa l (dreta px) py))
        ; si la tecla pulsada es W o flecha arriba (irá abajo porque se pinta al revés)
        ((and (or (= 87 tecla) (= 119 tecla) (= 328 tecla)) (not (equal (obtenir-posicio l px (abaix py)) 'paret))) (passa l px (abaix py)))
        ; si la tecla pulsada es S o flecha abajo (irá arriba porque se pinta al revés)
        ((and (or (= 83 tecla) (= 115 tecla) (= 336 tecla)) (not (equal (obtenir-posicio l px (amunt py)) 'paret))) (passa l px (amunt py)))
        ; si la tecla pulsada es ESC
        ((= 27 tecla) l)
        ; si no, se llama recursivamente tal cual
        (t (passa l px py))
    )
)

(defun llegeixLaberint (nom) ; lee el laberinto 
    (let* ((fp (open nom)) (contingut (llegeix-intern fp))) (close fp) contingut)
)

(defun llegeix-intern (fp)
    (let   ((c (read-char fp nil nil)))
        (cond   
            ((null c) '())
            (t (cons c (llegeix-intern fp)))) 
    )
)

(defun llegeixMatriu (l)
    (cond
        ((null l) '())
        (t (cons (llegeixMatriuFila l) (llegeixMatriu (salta-fila l))))
    )
)

(defun llegeixMatriuFila (f)
    (cond
        ((null f) '())
        ((equal (car f) #\Newline) '())
        ((equal (car f) #\#) (cons 'paret (llegeixMatriuFila (cdr f))))
        ((equal (car f) #\.) (cons 'cami (llegeixMatriuFila (cdr f))))
        ((equal (car f) #\e) 
            ; almacenar posición inicial jugador y asignar valor a las variables px y py
            (cons 'entrada (llegeixMatriuFila (cdr f)))
        )
        ((equal (car f) #\s) (cons 'sortida (llegeixMatriuFila (cdr f))))
        (t (cons '? (llegeixMatriuFila (cdr f))))
    )
)

(defun salta-fila (l)
    (cond
        ((null l) '())
        ((equal (car l) #\Newline) (cdr l))
        (t (salta-fila (cdr l)))
    )
)

(defun crea-fila (x y)
    (cond
        ((= 0 y) '())
        ((and (= entradaX x) (= entradaY y)) (cons 'entrada (crea-fila x (- y 1))))
        (t (cons 'paret (crea-fila x (- y 1))))
    )  
)

(passa l px (- lHeight (+ py 1)))