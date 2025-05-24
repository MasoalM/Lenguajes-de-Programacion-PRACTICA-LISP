; Título: Laberints (Programa que ejecuta laberintos y permite jugar)
; Autores: Marcos Socías Alberto y Hugo Valls Sabater
; Fecha última modificación: 24/05/2025
; Asignatura: Lenguajes de Programación
; Grupo: 102
; Profesor teoría: Antoni Oliver Tomàs
; Profesor prácticas: Francesc Xavier Gaya Morey
; Convocatoria: Ordinaria
; Instrucciones generales: Es importante tener en cuenta que hay que usar cada método justo después de haber cargado su respectivo archivo. No se puede generar mapas
;                          después de haber cargado "laberints.lisp" a no ser que se vuelva a cargar "creacioLaberints.lisp".
; Instrucciones de "creacioLaberints.lisp": Después de cargar el archivo con (load "creacioLaberints.lisp"), para generar un laberinto hay que
;                                           ejecutar el comando (genera "nombreFichero.txt" anchura altura), siendo "nombreFichero" el nombre que
;                                           se le quiera designar al nuevo laberinto a crear, "anchura" la cantidad de bloques de largo que tendrá
;                                           el laberinto y "altura" la cantidad de bloques de alto que tendrá el laberinto.
; Instrucciones de "laberints.lisp": Después de cargar el archivo con (load "laberints.lisp"), para empezar la partida hay que escribir
;                                    el comando (explora "nombreFichero.txt") y el juego comenzará. Hay que tener en cuenta que el fichero
;                                    seleccionado para cargar, tiene que existir.
; Aspectos opcionales "creacioLaberints.lisp": 
;  ->  "Millor gestió dels cantons a les parets interiors: evitar que es generin parets diagonals": Como se puede comprobar, hemos evitado que se generen
;       paredes diagonales, esto ha sido gracias a crear una condición extra para utilizar en nuestra función "crea-cami", es decir, ahora es más restrictivo
;       a la hora de crear caminos ya que hay más condiciones. Esta función es "contador-esquines-veines", básicamente primero comprueba hacia dónde nos queremos mover
;       y una vez lo sabe, mira que no haya camino en las dos esquinas en la dirección hacia donde se mueve (las que tiene atrás no las comprueba ya que no es necesario). 
;       Si no son pared, suma 1 y por tanto ya no es 0 el número de caminos cercanos prohibidos y no crea el camino.
;  ->  "Generació de laberints de mides superiors (per exemple, fins a 50x50).": Se puede comprobar que el comando (genera "nomFitxer.txt" 50 50) funciona y crea
;       un fichero de texto correcto. Antes de implementar la mejora de generar las paredes diagonales no funcionaba ya que teníamos demasiados caminos de posibilidades 
;       abiertas a la vez, pero gracias a podar más ramas con contador-esquines-veines ya funciona.
; Aspectos opcionales "laberints.lisp":
;  ->  "Gestió de les diferents mides dels laberints que s’hagin generat.": En llegeixMatriu, ejecutamos las dos siguientes instrucciones "(setq lWidth c) (setq lHeight c)",
;        estas nos permiten adaptar la lectura del laberinto a las dimensiones que tenga el fichero ya que la almacenamos con setq.
; Explicación diseño "creacioLaberints.lisp": Todo se basa en la función "genera" que se encarga de generar todo el fichero. Empieza asignando valores constantes (no los modificaremos)
;                                             y luego llama a funciones que están unas dentro de otras. Empieza creando la matriz con crea-matriu, esa matriz la utiliza para crearle
;                                             el camino con crea-cami (algoritmo DFS), luego crea la salida en otra posición random con crea-sortida. Seguidamente, se llama a generaContingut 
;                                             para que traduzca el laberinto de nuestra matriz a los símbolos que queremos meter en nuestro archivo de texto y finalmente se llama a escriuLaberint
;                                             para que se guarde el laberinto en un .txt. Durante este proceso se utilizan funciones auxiliares que están explicadas con sus correspondientes
;                                             comentarios y por otro lado también hemos explicado los extras anteriormente en este bloque de comentarios.
; Explicación diseño "laberints.lisp": La función inicial es "explora", que pide el nombre al jugador y llama a la función "passa" pasándole la matriz leída gracias a llegeixLaberint
;                                      (lee el fichero) y llegeixMatriu (adapta el formato del fichero y crea una matriz con valores). "Passa", se actualiza cada vez que el jugador hace
;                                      un movimiento, además de limpiar y pintar la pantalla en cada iteración. Finalmente, passa comprueba si el jugador está en la casilla final
;                                      para finalizar la partida, ordenar y mostrar el scoreboard con sus respectivas funciones. Durante este proceso se utilizan funciones auxiliares que 
;                                      están explicadas con sus correspondientes comentarios y por otro lado también hemos explicado los extras anteriormente en este bloque de comentarios.                                           

(setq xi 20) ; posición x donde empieza a pintar
(setq yi 0) ; posición y donde empieza a pintar 
(setq m 12)  ; tamaño de un cuadrado

(putprop 'colors '(0 0 0) 'paret)

(putprop 'colors '(255 255 255) 'cami)

(putprop 'colors '(255 0 0) 'sortida)

(putprop 'colors '(0 255 0) 'jugador)

(putprop 'colors '(0 0 255) 'entrada)

;------------------------------------------------------------------------
;FUNCIÓN : quadrat
;DESCRIPCIÓN: dibuja un cuadrado de dimensiones m^2 pixeles que tomarán el 
;            papel de cada celda del laberinto.
;PARAM: X : control de repeticiones dentro de la función
;       M : tamaño del lado del cuadrado 
;
;------------------------------------------------------------------------
(defun quadrat (x m)
    (drawrel 0 (+ m 1))
    (moverel 1 (- (+ m 1)))
    (cond
        ((> x 0) (quadrat (- x 1) m))
        (t (moverel (- (+ m 1)) 0))
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : fila-quadrats
;DESCRIPCIÓN: Dibuja una fila de cuadrados, 
;            representando cada celda del laberinto
;PARAM: N : número de celdas a pintar en la fila
;       L : contenido de la celda
;       PX y PY : coordenadas del jugador
;------------------------------------------------------------------------
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

;------------------------------------------------------------------------
;FUNCIÓN : pinta
;DESCRIPCIÓN: pinta el laberinto fila a fila
;            
;PARAM: L : matriz del laberinto
;       PX y PY : coordenadas del jugador
;
;------------------------------------------------------------------------
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

;------------------------------------------------------------------------
;FUNCIÓN : obtenir-posicio
;DESCRIPCIÓN: devuelve la posición referenciada de la matriz
;            
;PARAM: L : matriz del laberinto
;       X : columna
;       Y : fila
;------------------------------------------------------------------------
(defun obtenir-posicio (l x y)
    (cond
        ((= y 0) (obtenir-posicio-exacta (car l) x))
        (t (obtenir-posicio (cdr l) x (- y 1)))
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : obtenir-posicio-exacta 
;DESCRIPCIÓN: devuelve el elemento número x de una lista
;            
;PARAM: L : lista  (que viene a ser la fila de la matriz)
;       X : la posición que se desea obtener.
;
;------------------------------------------------------------------------
(defun obtenir-posicio-exacta (l x)
    (cond
        ((= x 0) (car l))
        (t (obtenir-posicio-exacta (cdr l) (- x 1)))
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : dreta, esquerra, amunt y abaix
;DESCRIPCIÓN: calcula la posición adyacente (cada una en el sentido indicado)
;            
;PARAM: PX o PY : posición x o y actual del jugador
;       
;
;------------------------------------------------------------------------
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

;------------------------------------------------------------------------
;FUNCIÓN : passa
;DESCRIPCIÓN: controla el movimiento del jugador en cada acción que toma
;            y gestiona la victoria.
;PARAM: L  : matriz del laberinto
;       PX y PY : posición actual jugador 
;       N : número de pasos que ha dado hasta ese momento
;       nom : nombre que se le ha dado al laberinto
;------------------------------------------------------------------------
(defun passa (l px py n nom)
    (move xi (+ yi (* m lHeight)))
    (pinta l px py)
    (cond
        ((equal (obtenir-posicio l px py) 'sortida) (cls) (ordenarScoreboard nomJugador nom n) (format t "HAS GANADO, ~A, EN TAN SOLO ~A PASOS~%" nomJugador n) (mostra-scoreboard nom))
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
                ((= 27 tecla) (cls))
                ; si no, se llama recursivamente tal cual
                (t (passa l px py n nom))
            )
        )
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : ordenarScoreboard
;DESCRIPCIÓN: ordena e inserta el resultado del jugador en el fichero de ScoreBoard
;            
;PARAM: nomJugador: nombre del jugador actual
;       nomLaberint: nombre del fichero del laberinto jugado
;       pasos: cantidad de pasos que tardó el jugador en completar el laberinto
;------------------------------------------------------------------------
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

;------------------------------------------------------------------------
;FUNCIÓN : quita-extension
;DESCRIPCIÓN: "devuelve" el nombre del fichero que se le pasa sin el .txt final,
;            para poder crear otro con el mismo nombre pero con el SB.txt
;PARAM: nom : nombre del fichero original
;       
;
;------------------------------------------------------------------------
(defun quita-extension (nom)
  (quita-extension-aux nom 0 (- (length nom) 4)))

;------------------------------------------------------------------------
;FUNCIÓN : quita-extension-aux
;DESCRIPCIÓN: auxiliar para guardar el nombre original sin la extensión
;            
;PARAM: nom : nombre original
;       i   : índice actual
;       fin : índice hasta donde quieres copiar 
;------------------------------------------------------------------------  
(defun quita-extension-aux (nom i fin)
  (cond
    ((= i fin) "")
    (t (concatenate 'string
                    (string (char nom i))
                    (quita-extension-aux nom (+ i 1) fin)))))

;------------------------------------------------------------------------
;FUNCIÓN : inserta-ordenat
;DESCRIPCIÓN: inserta un nuevo elemento (nombre del jugador y los pasos realizados)
;            en la lista ordenada del SB
;PARAM: nou : lista con el nombre del jugador y los pasos
;       llista : lista ya existente de resultados previos
;
;------------------------------------------------------------------------  
(defun inserta-ordenat (nou llista)
  (cond
    ((null llista) (list nou))
    ((< (cadr nou) (cadr (car llista))) (cons nou llista))
    (t (cons (car llista) (inserta-ordenat nou (cdr llista))))))

;------------------------------------------------------------------------
;FUNCIÓN : llegeix-scoreboard
;DESCRIPCIÓN: lee el contenido del fichero SB
;            
;PARAM: fp : file pointer del archivo abierto       
;
;------------------------------------------------------------------------  
(defun llegeix-scoreboard (fp)
  (let ((entrada (read fp nil nil)))
    (cond
      (entrada (cons entrada (llegeix-scoreboard fp)))
      (t '()))))

;------------------------------------------------------------------------
;FUNCIÓN : escriu-scoreboard
;DESCRIPCIÓN: Escribe las puntuaciones ordenadas en el fichero SB
;            
;PARAM: fp : file pointer del archivo abierto 
;       llista : lista ordenada con los datos de los jugadores
;
;------------------------------------------------------------------------  
(defun escriu-scoreboard (fp llista)
  (cond
    ((null llista) nil)
    (t
     (format fp "~S~%" (car llista))
     (escriu-scoreboard fp (cdr llista)))))

;------------------------------------------------------------------------
;FUNCIÓN : mostra-scoreboard
;DESCRIPCIÓN: muestra los 10 mejores resultados del archivo SB por pantalla
;            (anuncia que los va a imprimir, realmente 
;            los imprime su función aux)
;PARAM: nomLaberint : nombre del archivo del laberinto en cuestión
;       
;
;------------------------------------------------------------------------  
(defun mostra-scoreboard (nomLaberint)
  (let* ((fitxer (concatenate 'string (quita-extension nomLaberint) "SB.txt"))
         (fp (open fitxer :direction :input)))
    (cond
      (fp
       (format t "~%--- SCOREBOARD ---~%")
       (mostra-scoreboard-aux (llegeix-scoreboard fp) 1)
       (close fp))
      (t (format t "No hay scoreboard disponible.~%")))))

;------------------------------------------------------------------------
;FUNCIÓN : mostra-scoreboard-aux
;DESCRIPCIÓN: imprime por pantalla los 10 mejores resultados o en su defecto
;            todos los que haya en ese momento
;PARAM: llista : lista de jugadores y pasos
;       pos : posición en el ranking del 1 al 10
;
;------------------------------------------------------------------------ 
(defun mostra-scoreboard-aux (llista pos)
  (cond
    ((or (null llista) (> pos 10)) nil)  ; para si llegamos a 10 o a fin de lista
    (t
     (let ((entrada (car llista)))
       (format t "~D. ~A - ~A pasos~%" pos (car entrada) (cadr entrada)))
     (mostra-scoreboard-aux (cdr llista) (+ pos 1)))))

;------------------------------------------------------------------------
;FUNCIÓN : explora
;DESCRIPCIÓN: le pide el nombre al jugador e inicia el juego
;            
;PARAM: nom : nombre del fichero de laberinto
;       
;
;------------------------------------------------------------------------
(defun explora (nom)
    ; Pedir nombre al usuario
    (print "Escribe tu nombre, jugador")
    (setq nomJugador (read))
    (cls)
    (passa (llegeixMatriu (llegeixLaberint nom) 0 0 0) px py 0 nom)
)

;------------------------------------------------------------------------
;FUNCIÓN : llegeixLaberint
;DESCRIPCIÓN: lee el fichero del laberinto 
;            
;PARAM: nom : nombre del fichero del laberinto
;       
;
;------------------------------------------------------------------------
(defun llegeixLaberint (nom) ; lee el laberinto 
    (let* ((fp (open nom)) (contingut (llegeix-intern fp))) (close fp) contingut)
)

;------------------------------------------------------------------------
;FUNCIÓN : llegeix-intern
;DESCRIPCIÓN: lee caracter a caracter un fichero y lo guarda en una lista
;            
;PARAM: fp : file pointer
;       
;
;------------------------------------------------------------------------
(defun llegeix-intern (fp)
    (let   ((c (read-char fp nil nil)))
        (cond   
            ((null c) '())
            (t (cons c (llegeix-intern fp)))) 
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : llegeixMatriu
;DESCRIPCIÓN: crea una matriz de símbolos con los caracteres del fichero
;            
;PARAM: l : lista de caracteres
;       x : contador para las columnas 
;       y : contador para las filas
;       c : contador contador de tamaño del laberinto
;------------------------------------------------------------------------
(defun llegeixMatriu (l x y c)
    (cond
        ((null l) (setq lWidth c) (setq lHeight c) '())
        (t (cons (llegeixMatriuFila l x y) (llegeixMatriu (salta-fila l) x (+ y 1) (+ c 1))))
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : llegeixMatriuFila
;DESCRIPCIÓN: traduce las listas de caracteres del fichero 
;            en símbolos del laberinto
;            
;PARAM: f : lista de caracteres
;       x : posición actual x
;       y : posición actual y
;------------------------------------------------------------------------
(defun llegeixMatriuFila (f x y)
    (cond
        ((null f) '())
        ((equal (car f) #\Newline) '())
        ((equal (car f) #\#) (cons 'paret (llegeixMatriuFila (cdr f) (+ x 1) y)))
        ((equal (car f) #\.) (cons 'cami (llegeixMatriuFila (cdr f) (+ x 1) y)))
        ((equal (car f) #\e) 
            ; almacenar posición inicial jugador y asignar valor a las variables px y py
            (setq px x) ; posición inicial x en la matriz del jugador   
            (setq py y) ; posición inicial y en la matriz del jugador
            (cons 'entrada (llegeixMatriuFila (cdr f) (+ x 1) y))
        )
        ((equal (car f) #\s) (cons 'sortida (llegeixMatriuFila (cdr f) (+ x 1) y)))
        (t (cons '? (llegeixMatriuFila (cdr f) (+ x 1) y)))
    )
)

;------------------------------------------------------------------------
;FUNCIÓN : salta-fila
;DESCRIPCIÓN: salta al inicio de la siguiente fila del fichero
;            
;PARAM: l : lista de caracteres
;       
;
;------------------------------------------------------------------------
(defun salta-fila (l)
    (cond
        ((null l) '())
        ((equal (car l) #\Newline) (cdr l))
        (t (salta-fila (cdr l)))
    )
)

;(explora "prueba.txt")