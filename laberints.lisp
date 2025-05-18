; Título: Juego Laberinto LISP
; Autores: Marcos Socías Alberto y Hugo Valls Sabater
; Fecha última modificación: 21/04/2025
; Asignatura: Lenguajes de Programación
; Grupo: 102
; Profesor teoría: Antoni Oliver Tomàs
; Profesor prácticas: Francesc Xavier Gaya Morey
; Convocatoria: Ordinaria
; PONER MÁS TEXTOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

(setq xi 20) ; posicion x donde empieza a pintar
(setq yi 0) ; posicion y donde empieza a pintar 
(setq m 14)  ; tamaño de un cuadrado

(putprop 'colors '(0 0 0) 'paret)

(putprop 'colors '(255 255 255) 'cami)

(putprop 'colors '(255 0 0) 'sortida)

(putprop 'colors '(0 255 0) 'jugador)

(putprop 'colors '(0 0 255) 'entrada)

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

(defun passa (l px py n nom)
    (cls)
    (move xi (+ yi (* m lHeight)))
    (pinta l px py)
    (cond
        ((equal (obtenir-posicio l px py) 'sortida) (cls) (ordenarScoreboard nomJugador nom n) (format t "HAS GANADO, ~A, EN TAN SOLO ~A PASOS~%" nomJugador n) (mostra-scoreboard nom) l)
        (t 
            (setq tecla (get-key)) 
            (cond
                ; si la tecla pulsada es A o flecha izquierda
                ((and (or (= 65 tecla) (= 97 tecla) (= 331 tecla)) (not (equal (obtenir-posicio l (esquerra px) py) 'paret))) (passa l (esquerra px) py (+ n 1) nom))
                ; si la tecla pulsada es D o flecha derecha
                ((and (or (= 68 tecla) (= 100 tecla) (= 333 tecla)) (not (equal (obtenir-posicio l (dreta px) py) 'paret))) (passa l (dreta px) py (+ n 1) nom))
                ; si la tecla pulsada es W o flecha arriba (irá abajo porque se pinta al revés)
                ((and (or (= 87 tecla) (= 119 tecla) (= 328 tecla)) (not (equal (obtenir-posicio l px (abaix py)) 'paret))) (passa l px (abaix py) (+ n 1) nom))
                ; si la tecla pulsada es S o flecha abajo (irá arriba porque se pinta al revés)
                ((and (or (= 83 tecla) (= 115 tecla) (= 336 tecla)) (not (equal (obtenir-posicio l px (amunt py)) 'paret))) (passa l px (amunt py) (+ n 1) nom))
                ; si la tecla pulsada es ESC
                ((= 27 tecla) (cls) l)
                ; si no, se llama recursivamente tal cual
                (t (passa l px py n nom))
            )
        )
    )
)

(defun ordenarScoreboard (nomJugador nomLaberint pasos)
  (let* ((fitxer (concatenate 'string (quita-extension nomLaberint) "SB.txt"))
         (llista-original
           (let ((fp (open fitxer :direction :input :if-does-not-exist nil)))
             (cond
               (fp
                (let ((resultat (llegeix-scoreboard fp)))
                  (close fp)
                  resultat))
               (t '()))))
         (llista-ordenada (inserta-ordenat (list nomJugador pasos) llista-original))
         (fp (open fitxer :direction :output :if-exists :supersede)))
    (escriu-scoreboard fp llista-ordenada)
    (close fp)))

(defun quita-extension (nom)
  (quita-extension-aux nom 0 (- (length nom) 4)))

(defun quita-extension-aux (nom i fin)
  (cond
    ((= i fin) "")
    (t (concatenate 'string
                    (string (char nom i))
                    (quita-extension-aux nom (+ i 1) fin)))))

(defun inserta-ordenat (nou llista)
  (cond
    ((null llista) (list nou))
    ((< (cadr nou) (cadr (car llista))) (cons nou llista))
    (t (cons (car llista) (inserta-ordenat nou (cdr llista))))))

(defun llegeix-scoreboard (fp)
  (let ((entrada (read fp nil nil)))
    (cond
      (entrada (cons entrada (llegeix-scoreboard fp)))
      (t '()))))

(defun escriu-scoreboard (fp llista)
  (cond
    ((null llista) nil)
    (t
     (format fp "~S~%" (car llista))
     (escriu-scoreboard fp (cdr llista)))))

(defun mostra-scoreboard (nomLaberint)
  (let* ((fitxer (concatenate 'string (quita-extension nomLaberint) "SB.txt"))
         (fp (open fitxer :direction :input)))
    (cond
      (fp
       (format t "~%--- SCOREBOARD ---~%")
       (mostra-scoreboard-aux (llegeix-scoreboard fp) 1)
       (close fp))
      (t (format t "No hay scoreboard disponible.~%")))))

(defun mostra-scoreboard-aux (llista pos)
  (cond
    ((or (null llista) (> pos 10)) nil)  ; para si llegamos a 10 o a fin de lista
    (t
     (let ((entrada (car llista)))
       (format t "~D. ~A - ~A pasos~%" pos (car entrada) (cadr entrada)))
     (mostra-scoreboard-aux (cdr llista) (+ pos 1)))))

(defun explora (nom)
    ; Pedir nombre al usuario
    (print "Escribe tu nombre, jugador")
    (setq nomJugador (read))
    (passa (llegeixMatriu (llegeixLaberint nom) 0 0 0) px py 0 nom)
    (creaFitxer nom) 
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

(defun llegeixMatriu (l x y c)
    (cond
        ((null l) (setq lWidth c) (setq lHeight c) '())
        (t (cons (llegeixMatriuFila l x y) (llegeixMatriu (salta-fila l) x (+ y 1) (+ c 1))))
    )
)

(defun llegeixMatriuFila (f x y)
    (cond
        ((null f) '())
        ((equal (car f) #\Newline) '())
        ((equal (car f) #\#) (cons 'paret (llegeixMatriuFila (cdr f) (+ x 1) y)))
        ((equal (car f) #\.) (cons 'cami (llegeixMatriuFila (cdr f) (+ x 1) y)))
        ((equal (car f) #\e) 
            ; almacenar posición inicial jugador y asignar valor a las variables px y py
            (setq px x) ; posicion inicial x en la matriz del jugador   
            (setq py y) ; posicion inicial y en la matriz del jugador
            (cons 'entrada (llegeixMatriuFila (cdr f) (+ x 1) y))
        )
        ((equal (car f) #\s) (cons 'sortida (llegeixMatriuFila (cdr f) (+ x 1) y)))
        (t (cons '? (llegeixMatriuFila (cdr f) (+ x 1) y)))
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

(defun creaFitxer (nomLaberint)
    
)

(explora "pruebanuevanuevanueva.txt")