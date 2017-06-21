--  ^
type Polinomio =[Float]

grado :: Polinomio -> Integer
grado p = gradoAux p (-1) False

gradoAux :: Polinomio -> Integer -> Bool -> Integer
gradoAux p i b | length p == 0 = i
               | head p == 0 && b == False = gradoAux  (tail p) i False
               | otherwise = gradoAux (tail p) (i+1) True


evaluar :: Polinomio -> Float -> Float
evaluar p x =  evaluarAux p x 0

evaluarAux :: Polinomio -> Float -> Integer -> Float
evaluarAux p x i | (grado p) == 0 || length p == 0 = 0.0
                 | otherwise = (x^(grado p))*((head p)) + (evaluarAux (tail p) x i)

-- derivada :: Polinomio -> Polinomio
-- derivada p | length  p =< 1 = []
--            | otherwise = ((head p)*(fromIntegral ((grado p)-1)):(derivada (tail p))

suma :: Polinomio -> Polinomio -> Polinomio
suma p1 p2 = limpiarpolinomio (sumaAux p1 p2)

sumaAux :: Polinomio -> Polinomio -> Polinomio
sumaAux p1 p2 | length p1 == 0 = []
           | grado p1 < grado p2 = (head p2):(sumaAux p1 (tail p2))
           | grado p1 > grado p2 = (head p1):(sumaAux (tail p1) p2)
           | grado p1 == grado p2 = ((head p1)+(head p2)):(sumaAux (tail p1) (tail p2))

limpiarpolinomio :: Polinomio -> Polinomio
limpiarpolinomio p | length p == 0 = []
                   | head p == 0 = limpiarpolinomio (tail p)
                   | otherwise = (head p):limpiarpolinomio (tail p)

productoPorEscalar :: Float -> Polinomio -> Polinomio
productoPorEscalar n p = limpiarpolinomio (productoPorEscalarAux n p)

productoPorEscalarAux :: Float -> Polinomio -> Polinomio
productoPorEscalarAux n p | n == 0 || length p == 0 = []
                          | otherwise = (n*(head p)):(productoPorEscalarAux n (tail p))

productoPorMonomio :: Float -> Integer -> Polinomio -> Polinomio
productoPorMonomio coef n p = calcularGrado (productoPorMonomioAux coef n p) n


productoPorMonomioAux :: Float -> Integer -> Polinomio -> Polinomio
productoPorMonomioAux coef n p
  | length p == 0 = []
  | otherwise = (coef*(head p)): (productoPorMonomioAux coef n (tail p))


calcularGrado :: Polinomio -> Integer -> Polinomio
calcularGrado p n
      | n == 0 = p
      | otherwise = calcularGrado (p++[0]) (n-1)


productoAux :: Polinomio -> Polinomio -> [Polinomio]
productoAux p1 p2
      | length p1 == 0 = [[]]
      | otherwise =  (productoPorMonomio (head p1) p2):(productoAux (tail p1) p2)

producto :: Polinomio -> Polinomio -> [Polinomio]
producto p1 p2 = productoAux p1 p2





           ---
