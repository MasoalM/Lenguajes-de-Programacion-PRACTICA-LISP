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

;------------------------------------------------------------------------
;FUNCION : genera 
;DESCRIPCIO: genera el laberinto completo
;            
;PARAM: nom : nombre del archivo donde se guardara el laberinto
;       x : ancho del laberinto
;       y : alto del laberinto
;------------------------------------------------------------------------

(defun genera (nom x y)
    (setq lWidth x)
    (setq lHeight y)
    (setq entradaX (+ (random (- lWidth 2) rs) 1))
    (setq entradaY (+ (random (- lHeight 2) rs) 1))
    (setq actualX entradaX)
    (setq actualY entradaY)
    (escriuLaberint nom (generaContingut (crea-sortida (crea-cami (crea-matriu 0 lWidth lHeight) actualX actualY (crea-llista-random '())))))
)

;------------------------------------------------------------------------
;FUNCION : crea-cami
;DESCRIPCIO: crea el camino principal del laberinto utilizando DFS.
;            comprueba las 4 posiciones posibles sobre las que poder 
;            realizar el proximo movimiento y lo elige segun criterios  
;            que dependen de funciones explicadas mas adelante.
;PARAM: m : matriz actual del laberinto
;       x , y : posicion actual
;       r : lista aleatoria de direcciones a explorar
;------------------------------------------------------------------------
(defun crea-cami (m x y r)
    (cond
        ((null r) m)
        ;en caso de ser r ==0  ( derecha ) [funciona igual para los  movimientos posibles]
        ;comprobamos si el camino por el que venimos es el unico camino que hay en las 4 posiciones que comprobamos,
        ;comprobamos que no generariamos una pared diagonal
        ;comprobamos que la posicion en la que nos encontramos se trata de una pared
        ;comprobamos que no nos estamos saliendo del laberinto
        ; si todo eso se cumple se cambia la pared por camino y seguimos con el dfs desde esa posicion
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

;------------------------------------------------------------------------
;FUNCION : crea-sortida
;DESCRIPCIO: coloca la salida en una celda aleatoria que sea un "cami"
;            
;PARAM: m : matriz del laberito (ya generado)
;       
;
;------------------------------------------------------------------------
(defun crea-sortida (m)
    (setq randomX (+ (random (- lWidth 2) rs) 1))
    (setq randomY (+ (random (- lHeight 2) rs) 1))
    (cond
        ((equal (obtenir-posicio m randomX randomY) 'cami) (actualitza-posicio m randomX randomY 'sortida)) 
        (t (crea-sortida m))
    )
)

;------------------------------------------------------------------------
;FUNCION : limites
;DESCRIPCIO: funcion que verifica si una posicion esta dentro de 
;            los limites del laberinto
;            
;PARAM: x, y : coordenadas a comprobar 
;       
;
;------------------------------------------------------------------------
(defun limites (x y)
    (and (> x 0) (> y 0) (< x lWidth) (< y lHeight))
)

;------------------------------------------------------------------------
;FUNCION : actualizar-posicio
;DESCRIPCIO: funcion que actualiza el valor de una celda en una posicion 
;            dada dentro del laberinto
;PARAM: l : lista de listas del laberinto
;       x , y : coordenadas de la celda a actualizar
;       nou-valor : nuevo valor por el que se debe cambiar
;------------------------------------------------------------------------
(defun actualitza-posicio (l x y nou-valor)
    (cond
        ((null l) nil)
        ((= y 0) (cons (actualitza-posicio-fila (car l) x nou-valor) (cdr l)))
        (t (cons (car l) (actualitza-posicio (cdr l) x (- y 1) nou-valor)))
    )
)

;------------------------------------------------------------------------
;FUNCION : actualizar-posicion-fila
;DESCRIPCIO: actualizamos una celda dentro de una fila
;            
;PARAM: fila : lista que representa una fula dentro del laberinto
;       x : indice de la celda a modificar
;       nou-valor : nuevo alor por el que se debe cambiar
;------------------------------------------------------------------------
(defun actualitza-posicio-fila (fila x nou-valor)
    (cond
        ((null fila) nil)
        ((= x 0) (cons nou-valor (cdr fila)))
        (t (cons (car fila) (actualitza-posicio-fila (cdr fila) (- x 1) nou-valor)))
    )
)

;------------------------------------------------------------------------
;FUNCION : crea-llista-random
;DESCRIPCIO: crea una lista aleatoria de direcciones que usamos apra que 
;            el dfs sea lo mas aleatorio posible.
;PARAM: l : lista actual de firecciones ( se llama recursivamente 
;           hasta contener los 4 elementos posibles )
;       
;
;------------------------------------------------------------------------
(defun crea-llista-random (l)
    (cond
        ((and (pertany 0 l) (pertany 1 l) (pertany 2 l) (pertany 3 l)) (fer-conjunt l))
        (t (crea-llista-random (cons (random 4 rs) l)))
    )
)

;------------------------------------------------------------------------
;FUNCION : pertany
;DESCRIPCIO: comprueba si un elemento pertenece a una lista
;            
;PARAM: x : elemento que buscamos
;       l : lista donde buscamos
;
;------------------------------------------------------------------------
(defun pertany (x l)
    (cond 
        ((null l) nil)
        ((equal x (car l)) t)
        (t (pertany x (cdr l)))
    )
)

;------------------------------------------------------------------------
;FUNCION : fer-conjunt
;DESCRIPCIO: elimina los sobrantes de la lista de tal forma que tenemos
;            un orden aleatorio de los 4 elementos iniciales.
;PARAM: l : lista de elementos
;       
;
;------------------------------------------------------------------------
(defun fer-conjunt (l)
    (cond 
        ((null l) nil)
        ((pertany (car l) (cdr l)) (fer-conjunt (cdr l)))
        (t (cons (car l) (fer-conjunt(cdr l))))
    )
)

;------------------------------------------------------------------------
;FUNCION : contador-camins-veins 
;DESCRIPCIO: comprueba si es posible avanzar en ciertas direcciones 
;            contando cuantos elementos de al rededor no son pared de 
;            cierta posicion
;PARAM: l : matriz del laberinto (en proceso)
;       x , y : coordenadas a analizar
;
;------------------------------------------------------------------------
(defun contador-camins-veins (l x y)
    (+  (cond ((not (equal (obtenir-posicio l (+ x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l (- x 1) y) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (+ y 1)) 'paret)) 1) (t 0))
        (cond ((not (equal (obtenir-posicio l x (- y 1)) 'paret)) 1) (t 0))
    )
)

;------------------------------------------------------------------------
;FUNCION : contador-esquines-veines
;DESCRIPCIO: comprueba si es posible avanzar en ciertas direcciones 
;            dependiendo de cuantos elementos de al rededor son paredes
;            o no ( mejora para evitar paredes diagonales)
;PARAM: l : matriz del laberinto (en proceso)
;       x , y : coordenadas a analizar
;       dir : direccion que se esta evaluando 
;------------------------------------------------------------------------
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

;------------------------------------------------------------------------
;FUNCION : obtenir-posicio
;DESCRIPCIO: obtiene el valor de una celda del laberinto dada su posicion
;                       
;PARAM: l : matriz del laberinto
;       x , y : coordenadas de la posicion
;       
;------------------------------------------------------------------------
(defun obtenir-posicio (l x y)
    (cond
        ((<= y 0) (obtenir-posicio-exacta (car l) x))
        (t (obtenir-posicio (cdr l) x (- y 1)))
    )
)

;------------------------------------------------------------------------
;FUNCION : obtenir-posicio-exacta
;DESCRIPCIO: obtenre el valor dentro de una fila
;                        
;PARAM: l : fila del laberinto
;       x : indice de la celda a obtener dentro de la fila
;       
;------------------------------------------------------------------------
(defun obtenir-posicio-exacta (l x)
    (cond
        ((<= x 0) (car l))
        (t (obtenir-posicio-exacta (cdr l) (- x 1)))
    )
)

;------------------------------------------------------------------------
;FUNCION : crea-matriu
;DESCRIPCIO: crea una matriz segun los valores de entrada
;                        
;PARAM: altura : altura que debe tener la matriz
;       anchura : anchura que debe tener la matriz
;       y : contador de filas creadas
;------------------------------------------------------------------------
(defun crea-matriu (y anchura altura)
    (cond
        ((= y altura) '())
        (t (cons (crea-fila 0 anchura y) (crea-matriu (+ y 1) anchura altura)))
    )
)

;------------------------------------------------------------------------
;FUNCION : crea-fila
;DESCRIPCIO:  crea una fila de paredes de longitud anchura , a menos que
;             la posicion sea la de entrada           
;PARAM: x : contador de columnas creadas
;       y : indice de la fila actual
;       anchura : numero de columnas en la fila
;------------------------------------------------------------------------
(defun crea-fila (x anchura y)
    (cond
        ((= x anchura) '())
        ((and (= entradaX x) (= entradaY y)) (cons 'entrada (crea-fila (+ x 1) anchura y)))
        (t (cons 'paret (crea-fila (+ x 1) anchura y)))
    )
)

;------------------------------------------------------------------------
;FUNCION : generaContingut
;DESCRIPCIO: convierte la amtriz generada en una lista de caracteres 
;            para almacenar.            
;PARAM: m : matriz del laberinto
;       
;       
;------------------------------------------------------------------------ 
(defun generaContingut (m)
  (cond
    ((null m) '())
    (t (append (generaContingutFila (car m)) (generaContingut (cdr m))))
  )
)

;------------------------------------------------------------------------
;FUNCION : generaContingutFila
;DESCRIPCIO: se traduce el simbolo a los caracteres que seran almacenados
;                        
;PARAM: fila : lista de simbolos a traducir
;       
;       
;------------------------------------------------------------------------
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

;------------------------------------------------------------------------
;FUNCION : escriuLaberint
;DESCRIPCIO: escribe el contenido del labeirnto en un archivo de nombre "nom"
;                        
;PARAM: nom : nombre del archivo donde almacenamos el contenido
;       contingut : lista de caracteres a escribir
;       
;------------------------------------------------------------------------
(defun escriuLaberint (nom contingut)  
    (let ((fp (open nom :direction :output)))
        (escriu-intern fp contingut)
        (close fp)
    )
)

;------------------------------------------------------------------------
;FUNCION : escriu-intern
;DESCRIPCIO: escribe la lista de caracteres en un fichero
;                        
;PARAM: fp : file pointer
;       contingut : lista de caracteres a escribir
;       
;------------------------------------------------------------------------
(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp) (escriu-intern fp (cdr contingut)))
    )
)

(print (genera "prueba.txt" 25 25))