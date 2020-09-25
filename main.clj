(defn firstp [s L]
  (cond
    (empty? L) false
    (= s (first L)) true
    :else false
  )
)


(defn duplicar [L]
  (cond
    (empty? L) L
    :else (cons (list(first L)(first L))(duplicar(rest L)))
  )
)