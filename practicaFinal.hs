data Var = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
    deriving (Show, Eq, Ord)

data Formula = Atom Var
             | Neg Formula
             | Formula :&: Formula
             | Formula :|: Formula
             | Formula :=>: Formula
             | Formula :<=>: Formula
    deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

{-Ejercicio 1-}

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y /= x]

variables :: Formula -> [Var]
variables (Atom var) = [var]
variables (Neg formula) = conjunto (variables formula)
variables (formula1 :&: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :|: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :=>: formula2) = conjunto (variables formula1 ++ variables formula2)
variables (formula1 :<=>: formula2) = conjunto (variables formula1 ++ variables formula2)

{-Ejercicio 2-}

negacion :: Formula -> Formula
negacion (Atom var) = Neg (Atom var)
negacion (Neg formula) = formula
negacion (formula1 :&: formula2) = Neg formula1 :|: Neg formula2
negacion (formula1 :|: formula2) = Neg formula1 :&: Neg formula2
negacion (formula1 :=>: formula2) = negacion (Neg formula1 :|: formula2)
negacion (formula1 :<=>: formula2) = negacion ((formula1 :=>: formula2) :&: (formula2 :=>: formula1))



{-Ejercicio 3-}

equivalencia :: Formula -> Formula
equivalencia (Atom var) = Atom var
equivalencia (Neg formula) = Neg (equivalencia formula)
equivalencia (formula1 :&: formula2) = equivalencia formula1 :&: equivalencia formula2
equivalencia (formula1 :|: formula2) = equivalencia formula1 :|: equivalencia formula2
equivalencia (formula1 :=>: formula2) = Neg (equivalencia formula1) :|: equivalencia formula2
equivalencia (formula1 :<=>: formula2) =
    (Neg (equivalencia formula1) :|: equivalencia formula2) :&: (Neg (equivalencia formula2) :|: equivalencia formula1)

{-Ejercicio 4-}

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

combinacionesAux :: Int -> [[(Var, Bool)]]
combinacionesAux 0 = [[]]  
combinacionesAux n = 
  let combinacionesPrevias = combinacionesAux (n - 1)  
  in  [ (var, False) : xs | xs <- combinacionesPrevias ] ++ [ (var, True) : xs | xs <- combinacionesPrevias ]

combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones formula = combinacionesAux (longitud (variables formula))  

{-Ejercicio 5-}

combinaciones :: Formula -> [[(Var, Bool)]]
combinaciones formula =
    let vars = variables formula
    in sequence [[(var, False), (var, True)] | var <- vars]

{-Ejercicio 6-}

tablaDeVerdadCom :: Formula -> [[(Var, Bool)]] -> [([(Var, Bool)], Bool)]
tablaDeVerdadCom formula [] = []
tablaDeVerdadCom formula (x:xs) = (x, interpretacion formula x)
(tablaDeVerdadCom formula xs)

tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad formula = tablaDeVerdadCom formula (combinaciones formula)
