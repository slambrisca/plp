import Grafo
import Tipos
import Lomoba
import Parser
import Test.HUnit

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [
  "parser" ~: testsParser,
  "grafo" ~: testsGrafo,
  "lomoba" ~: testsLomoba
  ]

testsParser = test [
  (Var "p")                       ~=? (parse "p"),
  (And (Var "p") (Var "q"))       ~=? (parse "p && q"),
  (Or (Var "p") (Var "q"))        ~=? (parse "p || q"),
  (Or (Not (Var "p")) (Var "q"))  ~=? (parse "!p || q"),
  (And (D (Var "p")) (Var "q"))   ~=? (parse "<>p && q"),
  (And (B (Var "p")) (Var "q"))   ~=? (parse "[]p && q"),
  (D (And (Var "p") (Var "q")))   ~=? (parse "<>(p && q)"),
  (B (And (Var "p") (Var "q")))   ~=? (parse "[](p && q)")
  ]




testsGrafo = test [
  -- Test Nodos --
  -- [] ~~? (nodos vacio), -- No se puede testear por un problema de tipos
  [1] ~~? (nodos (agNodo 1 vacio)),
  [1,2] ~~? (nodos (agNodo 2 (agNodo 1 vacio))),

  -- Test Vecinos --
  [] ~~? (vecinos (agNodo 1 vacio) 1),
  [] ~~? (vecinos (agEje (2,1) (agNodo 1 vacio)) 1),
  [] ~~? (vecinos (agEje (1,1) (agNodo 1 vacio)) 2),
  [1] ~~? (vecinos (agEje (1,1) (agNodo 1 vacio)) 1),

  -- Test sacarNodo --
  [] ~~? (nodos (sacarNodo 1 vacio)),
  [] ~~? (nodos (sacarNodo 1 (agNodo 1 vacio))),
  [1..4] ~~? (nodos (sacarNodo 5 (lineal [1..5]))),
  [] ~~? (vecinos (sacarNodo 5 (lineal [1..5])) 4),

  -- Test lineal --
  [9] ~~? (nodos (lineal [9])),
  [1..4] ~~? (nodos (lineal [1..4])),
  [2] ~~? (vecinos (lineal [1..4]) 1),
  [] ~~? (vecinos (lineal [1..4]) 4),

  -- Test union --
  [1] ~~? (nodos (union vacio (lineal [1]))),
  [1,2] ~~? (nodos (union (lineal [2]) (lineal [1]))),
  [1..10] ~~? (nodos (union (lineal [5..10]) (lineal [1..5]))),
  [] ~~? (vecinos (union vacio (lineal [1])) 1),
  [] ~~? (vecinos (union (lineal [2]) (lineal [1])) 1),
  [6] ~~? (vecinos (union (lineal [5..10]) (lineal [1..5])) 5)

  -- Test Clausura --
  ]



testsLomoba = test [
  -- Test Visibilidad --
  0 ~=? visibilidad (parse "p"),
  1 ~=? visibilidad (parse "<>p"),
  2 ~=? visibilidad (parse "<>!<>p"),
  2 ~=? visibilidad (parse "<><>p || <><>q"),
  3 ~=? visibilidad (parse "<>(<>p || <><>q)"),
  3 ~=? visibilidad (parse "[](<>p || <>[]q)"),

  -- Test Extraer --
  ["p"]      ~~? extraer (parse "p"),
  ["p"]      ~~? extraer (parse "p || !p"),
  ["p", "q"] ~~? extraer (parse "p && q")

  ]

---------------
--  helpers  --
---------------

-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
  where
    sort = foldl (\r e -> push r e) []
    push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)
