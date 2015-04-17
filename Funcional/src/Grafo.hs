module Grafo (Grafo, vacio, nodos, vecinos, agNodo, sacarNodo, agEje, lineal, union, clausura) where

data Grafo a = G [a] (a -> [a])

instance (Show a) => Show (Grafo a) where
  show (G n e) = "[\n" ++ concat (map (\x -> " " ++ show x ++ " -> " ++ show (e x) ++ "\n") n) ++ "]"



--Grafo prueba

grafo1 = G [1,2,3] (\n -> [n])
grafo2 = G [9,8,7] (\n -> [n])
grafoa = G ['a','b','c'] (\n -> [n])
grafob = G ['d','e','f'] (\n -> [n])


-- ---------------------------------Sección 3--------- Grafos ---------------------------

-- Ejercicio 1
vacio :: Grafo a
vacio = G [] (\n -> [])

-- Ejercicio 2
nodos :: Grafo a -> [a]
nodos (G ns _) = ns

-- Ejercicio 3
vecinos :: Grafo a -> a -> [a]
vecinos (G ns fv) nodo = (fv nodo)

-- Ejercicio 4
--agNodo :: a -> Grafo a -> Grafo a
--agNodo nodo (G ns fv) = (G (nodo:ns) fv)

--Version que no agrega repetidos
agNodo :: Eq a => a -> Grafo a -> Grafo a
agNodo nodo (G ns fv) 	| not(elem nodo ns) = (G (nodo:ns) fv)
						| otherwise = (G ns fv)


-- Ejercicio 5
sacarNodo :: Eq a => a -> Grafo a -> Grafo a
sacarNodo nodo (G ns fv) = (G  (filter (nodo /=) ns) (\n -> if n==nodo then [] else fv n))


-- Ejercicio 6
agEje :: Eq a => (a,a) -> Grafo a -> Grafo a
agEje (a1,a2) (G ns fv) = (G ns (\n -> if n==a1 then a2:(fv n) else fv n))


-- Ejercicio 7
lineal :: Eq a => [a] -> Grafo a
lineal ns = (G ns (\n -> concat( zipWith (\x y -> if x==n then [y] else []) ns (tail ns) ) ))


-- Ejercicio 8
union :: Eq a => Grafo a -> Grafo a -> Grafo a
union (G ns1 fv1) (G ns2 fv2) 	=	 (G (sacarRepetidos (ns1++ns2)) (\n -> sacarRepetidos((g n ns1 fv1)++(g n ns2 fv2)))) where g x ls f = if elem x ls then f x else []

sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos ls = filter ((flip elem) ls) ls

-- Ejercicio 9
clausura :: Grafo a -> Grafo a
clausura = undefined





