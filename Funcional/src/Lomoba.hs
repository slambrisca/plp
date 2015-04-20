module Lomoba where
import qualified Data.List as List
import Grafo
import Tipos


-- ---------------------------------Sección 6--------- Lomoba ---------------------------

-- Ejercicio 10
foldExp :: (Prop -> b) -> (b -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (b -> b) -> Exp -> b
foldExp fV fNot fOr fAnd fD fB exp = let foldExpRec = (foldExp fV fNot fOr fAnd fD fB) in
    case exp of
      (Var p)     -> fV p
      (Not e)     -> fNot (foldExpRec e)
      (Or e1 e2)  -> fOr (foldExpRec e1) (foldExpRec e2)
      (And e1 e2) -> fAnd (foldExpRec e1) (foldExpRec e2)
      (D e)       -> fD (foldExpRec e)
      (B e)       -> fB (foldExpRec e)

-- Ejercicio 11
visibilidad :: Exp -> Integer
visibilidad = foldExp (\p -> 0) id (max) (max) (+1) (+1)

-- Ejercicio 12
extraer :: Exp -> [Prop]
extraer exp = List.nub $ foldExp (\p -> [p]) id join_props join_props id id exp
  where join_props = \e1 e2 -> e1 ++ e2

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
cierto (K grafo f) exp = all (\w -> eval model w exp) (nodos grafo)
  where model = K grafo f

