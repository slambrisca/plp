module Lomoba where
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> b) ->  (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (b -> b) -> Exp -> b
foldExp fV fNot fOr fAnd fD fB exp = let foldExpRec = (foldExp fV fNot fOr fAnd fD fB) in
										 case exp of
											(Var p) -> (fV p)
											(Not e) -> (fNot (foldExpRec e))
											(Or e1 e2) -> (fOr (foldExpRec e1) (foldExpRec e2))
											(And e1 e2) -> (fAnd (foldExpRec e1) (foldExpRec e2))
											(D e) -> (fD (foldExpRec e))
											(B e) -> (fB (foldExpRec e))


sacarRepetidos :: Eq a => [a] -> [a]
sacarRepetidos ls = filter ((flip elem) ls) ls

-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad = foldExp (\p -> 0) id (+) (+) (+1) (+1)

exp1 = (Var "p")

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer e = (foldExp (\p -> [p]) id (++) (++) id id e)
-- Falta asegurarse que no devuelva repetidos

-- Ejercicio 13
eval :: Modelo -> Mundo -> Exp -> Bool
eval = undefined

-- Ejercicio 14
valeEn :: Exp -> Modelo -> [Mundo]
valeEn = undefined

-- Ejercicio 15
quitar :: Exp -> Modelo -> Modelo
quitar = undefined

-- Ejercicio 16
cierto :: Modelo -> Exp -> Bool
cierto = undefined

