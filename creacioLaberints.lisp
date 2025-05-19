; Título: Creació Laberints (Se encarga de crear los ficheros de laberintos)
; Autores: Marcos Socías Alberto y Hugo Valls Sabater
; Fecha última modificación: 19/05/2025
; Asignatura: Lenguajes de Programación
; Grupo: 102
; Profesor teoría: Antoni Oliver Tomàs
; Profesor prácticas: Francesc Xavier Gaya Morey
; Convocatoria: Ordinaria
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

(setq rs (make-random-state t))

(defun genera (nom x y)
    (setq lWidth x)
    (setq lHeight y)
    (setq entradaX (+ (random (- lWidth 2) rs) 1))
    (setq entradaY (+ (random (- lHeight 2) rs) 1))
    (setq actualX entradaX)
    (setq actualY entradaY)
    (escriuLaberint nom (generaContingut (crea-sortida (crea-cami (crea-matriu 0 lWidth lHeight) actualX actualY (crea-llista-random '())))))
)

; algoritmo DFS
(defun crea-cami (m x y r)
    (cond
        ((null r) m)
        ((and (= (car r) 0) (= (contador-camins-veins m (+ x 1) y) 1) (= (contador-esquines-veines m (+ x 1) y 0) 0) (equal (obtenir-posicio m (+ x 1) y) 'paret) (limites (+ x 1) y))
            (crea-cami (crea-cami (actualitza-posicio m (+ x 1) y 'cami) (+ x 1) y (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 1) (<= (contador-camins-veins m (- x 1) y) 1) (= (contador-esquines-veines m (- x 1) y 1) 0) (equal (obtenir-posicio m (- x 1) y) 'paret) (limites (- x 1) y))
            (crea-cami (crea-cami (actualitza-posicio m (- x 1) y 'cami) (- x 1) y (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 2) (<= (contador-camins-veins m x (+ y 1)) 1) (= (contador-esquines-veines m x (+ y 1) 2) 0) (equal (obtenir-posicio m x (+ y 1)) 'paret) (limites x (+ y 1)))
            (crea-cami (crea-cami (actualitza-posicio m x (+ y 1) 'cami) x (+ y 1) (crea-llista-random '())) x y (cdr r)))
        ((and (= (car r) 3) (<= (contador-camins-veins m x (- y 1)) 1) (= (contador-esquines-veines m x (- y 1) 3) 0) (equal (obtenir-posicio m x (- y 1)) 'paret) (limites x (- y 1)))
            (crea-cami (crea-cami (actualitza-posicio m x (- y 1) 'cami) x (- y 1) (crea-llista-random '())) x y (cdr r)))
        (t (crea-cami m x y (cdr r)))
    )
)

(defun crea-sortida (m)
    (setq randomX (+ (random (- lWidth 2) rs) 1))
    (setq randomY (+ (random (- lHeight 2) rs) 1))
    (cond
        ((equal (obtenir-posicio m randomX randomY) 'cami) (actualitza-posicio m randomX randomY 'sortida)) 
        (t (crea-sortida m))
    )
)

; Función que verifica si una posición está dentro de los límites del laberinto
(defun limites (x y)
    (and (> x 0) (> y 0) (< x lWidth) (< y lHeight))
)

; Función que actualiza el tipo de una celda del laberinto en la posición dada
(defun actualitza-posicio (l x y nou-valor)
    (cond
        ((null l) nil)
        ((= y 0) (cons (actualitza-posicio-fila (car l) x nou-valor) (cdr l)))
        (t (cons (car l) (actualitza-posicio (cdr l) x (- y 1) nou-valor)))
    )
)
; Función que actualiza el tipo de una celda en una fila
(defun actualitza-posicio-fila (fila x nou-valor)
    (cond
        ((null fila) nil)
        ((= x 0) (cons nou-valor (cdr fila)))
        (t (cons (car fila) (actualitza-posicio-fila (cdr fila) (- x 1) nou-valor)))
    )
)
; genera una lista de direcciones aleatorias para el recorrido
(defun crea-llista-random (l)
    (cond
        ((and (pertany 0 l) (pertany 1 l) (pertany 2 l) (pertany 3 l)) (fer-conjunt l))
        (t (crea-llista-random (cons (random 4 rs) l)))
    )
)
; verifica si un elemento pertenece a una lista
(defun pertany (x l)
    (cond 
        ((null l) nil)
        ((equal x (car l)) t)
        (t (pertany x (cdr l)))
    )
)
;  elimina los elementos duplicados en una lista (lo usamos para la lista random)
(defun fer-conjunt (l)
    (cond 
        ((null l) nil)
        ((pertany (car l) (cdr l)) (fer-conjunt (cdr l)))
        (t (cons (car l) (fer-conjunt(cdr l))))
    )
)
; comprueba si estamos en un camino legal, mirando el tipo de casillas vecinas
; MEJORAR PORQUE SOLO USA UN COND Y SALE
(defun contador-camins-veins (l x y)
    (+  (cond ((not (equal (obtenir-posicio l (+ x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l (- x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (+ y 1)) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (- y 1)) 'paret)) 1) (t 0))
    )
)

(defun contador-esquines-veines (l x y dir)
    (cond
        ; DERECHA (0)
        ((= dir 0) (+
            (cond ((not (equal (obtenir-posicio l (+ x 1) (+ y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (+ x 1) (- y 1)) 'paret)) 1) (t 0))
        ))
        ; IZQUIERDA (1)
        ((= dir 1) (+
            (cond ((not (equal (obtenir-posicio l (- x 1) (+ y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (- x 1) (- y 1)) 'paret)) 1) (t 0))
        ))
        ; ARRIBA (2)
        ((= dir 2) (+  
            (cond ((not (equal (obtenir-posicio l (+ x 1) (+ y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (- x 1) (+ y 1)) 'paret)) 1) (t 0))
        ))
        ; ABAJO (3)
        ((= dir 3) (+
            (cond ((not (equal (obtenir-posicio l (- x 1) (- y 1)) 'paret)) 1) (t 0))
            (cond ((not (equal (obtenir-posicio l (+ x 1) (- y 1)) 'paret)) 1) (t 0))
        ))
    )
)


; Obtiene el valor de una posición en el laberinto
(defun obtenir-posicio (l x y)
    (cond
        ((<= y 0) (obtenir-posicio-exacta (car l) x))
        (t (obtenir-posicio (cdr l) x (- y 1)))
    )
)
; Obtiene el valor exacto de una posición en una fila
(defun obtenir-posicio-exacta (l x)
    (cond
        ((<= x 0) (car l))
        (t (obtenir-posicio-exacta (cdr l) (- x 1)))
    )
)
; Crea una matriz de tamaño lHeight con filas vacías
(defun crea-matriu (y anchura altura)
    (cond
        ((= y altura) '())
        (t (cons (crea-fila 0 anchura y) (crea-matriu (+ y 1) anchura altura)))
    )
)
; Crea una fila de celdas llena de paredes
(defun crea-fila (x anchura y)
    (cond
        ((= x anchura) '())
        ((and (= entradaX x) (= entradaY y)) (cons 'entrada (crea-fila (+ x 1) anchura y)))
        (t (cons 'paret (crea-fila (+ x 1) anchura y)))
    )
)
; Función que genera el contenido del laberinto 
(defun generaContingut (m)
  (cond
    ((null m) '())
    (t (append (generaContingutFila (car m)) (generaContingut (cdr m))))
  )
)
; Según el tipo de casilla añadimos una serie de caracteres al archivo
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
; Escribe el contenido generado en un archivo especificado
(defun escriuLaberint (nom contingut)  
    (let ((fp (open nom :direction :output)))
        (escriu-intern fp contingut)
        (close fp)
    )
)

(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp) (escriu-intern fp (cdr contingut)))
    )
)

;(print (genera "prueba.txt" 25 25))