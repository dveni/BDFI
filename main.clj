;; Escribir un predicado FIRSTP que tome dos argumentos, el primero un símbolo y el segundo una lista, y que devuelva T si el primer argumento (el símbolo) es igual al primer elemento del segundo argumento (la lista) y NIL en caso contrario. 

;; Resultado:
;; (firstp 1 '())
;; => false
;; (firstp 1 '(2 3))
;; => false
;; (firstp 1 '(1 2 3))
;; => true

(defn firstp [s L]
  (cond
    (empty? L) false
    (= s (first L)) true
    :else false
  )
)

;; Escribir una función DUPLICAR que toma como argumento una lista y devuelve la lista cuyos elementos son pares (lista de dos elementos) compuestos por los elementos de la primera. Tenga en cuenta que los elementos de la lista resultante son, a su vez, listas de dos elementos.

;; Resultado:
;; (duplicar ())
;; => ()
;; (duplicar '(1))
;; => ((1 1))
;; (duplicar '(1 2 3))
;; => ((1 1) (2 2) (3 3))

(defn duplicar [L]
  (cond
    (empty? L) L
    :else (cons (list(first L)(first L))(duplicar(rest L)))
  )
)

;; Escribir una función recursiva COUNTDOWN que toma como argumento un número N positivo y genera una lista de enteros desde N hasta 1. Si se le pasa un número negativo o cero, debe devolver la lista vacia (NIL).

;; Resultado:
;; (countdown 10)
;; => (10 9 8 7 6 5 4 3 2 1)
;; (countdown -10)
;; => nil

(defn countdown [N]
  (cond
    (> N 0)(cons N (countdown(- N 1)))
    :else nil
  )
)


;; Escribir una función REVERSE que toma como argumento una lista y devuelve la lista invertida, es decir, con los elementos en el orden inverso que aparecen en el original.

;; Resultado:   
;; (reverse '(1 2 3))
;; => (3 2 1)
;;    (reverse ())
;; => ()


(defn reverse[L]
  (cond
    (empty? L) L
    :else (concat (reverse(rest L))(list(first L)))
  )
)  

;; Escribir una función recursiva SUBSTITUTE que toma tres argumentos, los dos primeros son símbolos X e Y y el tercero una lista L. La función contruye una nueva lista en la que todas las apariciones del elemento Y es sustituido por X. 

;; Resultado: 
;; (substitute 0 5 '(1 2 3 4 5 6 7 8 9))
;; => (1 2 3 4 0 6 7 8 9)
;; (substitute 0 5 '())
;; => ()
;; (substitute 0 5 '( 6 7 8 9))
;; => (6 7 8 9)

(defn substitute [x y L]
  (cond
    (not(some #{y} L)) L
    (= y (first L))(cons x (substitute x y (rest L)))
    :else (cons (first L)(substitute x y (rest L)))
  )

)

;;Escribir un predicado SETEQUAL que tome dos listas como argumentos y que devuelva T si todos los elementos de la primera lista estan incluidos en la segunda y a la inversa. En caso contrario debe devolver NIL.

;; Resultado:
;; (setequal '( 3 4) '(3 3 4 4 4 4))
;; => true
;; (setequal '( 3 3 3 3  4) '(3 3 4 4 4 4))
;; => true
;; (setequal '( 3 3 3 3 4 4 4  4) '(3 3 4 4 4 4))
;; => true
;; (setequal '() '(3 3 4 4 4 4))
;; => nil
;; (setequal '() '())
;; => true
;; (setequal '(3 3 3 4 5) '(1 2 3 4 5))
;; => nil

(defn setequal [L1 L2]
  
  (cond 
    (and (= (count L1)(count(intersection L1 L2)))
        (= (count L2)(count(intersection L2 L1)))
    ) true
    :else nil
  )
)

;; Función auxiliar que calcula la intersección de dos listas, en sentido de intersección de multiconjuntos

(defn intersection[L1 L2]
  (cond
    (empty? L1) L1
    (some #{(first L1)} L2)(cons (first L1)(intersection(rest L1) L2))
    :else (intersection(rest L1)L2) 
  )
)


;; Escribir una función IMPARES que toma como argumento una lista y devuelve una lista formada por los elementos en las posiciones impares de la lista original

;; Resultado:
;; (impares '())
;; => ()
;; (impares '(1))
;; => (1)
;; (impares '(1 2 3 4 5 6))
;; => (1 3 5)
;; (impares '(1 1 2 2 3 3 4 4 5 5))
;; => (1 2 3 4 5)

(defn impares [L]
  (cond
    (or (empty? L)(empty? (rest L))) L
    :else (cons(first L)(impares(nthrest L 2)))
  )
)


