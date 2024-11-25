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

negar :: Bool -> Bool
negar False = True
negar True = False

conjuncion :: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion  _ _ = False


disyuncion :: Bool -> Bool -> Bool
disyuncion False False = False
disyuncion _ _ = True


condicional :: Bool -> Bool -> Bool
condicional True False = False
condicional _ _ = True

bicondicional :: Bool -> Bool -> Bool
bicondicional False True = False
bicondicional True False = False
bicondicional _ _ = True

buscarInterpretacionVariable :: Var -> [(Var,Bool)] -> Bool
buscarInterpretacionVariable v ((x,b):xs) = if v == x then b  else buscarInterpretacionVariable v xs

interpretacion :: Formula  -> [(Var, Bool)]-> Bool
interpretacion (Atom v) xs = buscarInterpretacionVariable v xs 
interpretacion (Neg  t) xs = negar (interpretacion t xs)
interpretacion (p :|: q) xs = disyuncion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :&: q ) xs = conjuncion (interpretacion p xs) (interpretacion q xs)
interpretacion (p :=>: q) xs= condicional (interpretacion p xs) (interpretacion q xs)
interpretacion (p :<=>: q) xs = bicondicional (interpretacion p xs) (interpretacion q xs)

{-Ejercicio 5-}

agregar :: a -> [[a]] -> [[a]]
agregar x [] = []
agregar x (y:ys) =  ((x:y):agregar x ys)

aux :: [Var] -> [[(Var,Bool)]]
aux [x] = [[(x,True)], [(x,False)]]
aux (x:xs) = (agregar (x,True) (aux xs)) ++ (agregar (x,False) (aux xs))


combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones p  = aux(variables p) 

{-Ejercicio 6-}

tablaDeVerdadCom :: [[(Var, Bool)]] ->  Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdadCom [] formula = []
tablaDeVerdadCom (x:xs) formula = [(x,interpretacion formula x)] ++ tablaDeVerdadCom xs formula

tablaDeVerdad :: Formula -> [([(Var, Bool)], Bool)]
tablaDeVerdad formula = tablaDeVerdadCom (combinaciones formula) formula
