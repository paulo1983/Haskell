-- Clase 11 de polinomios

type Polinomio = [Float]
-- type Monomio = (Escalar, Integer)


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
--            | otherwise = ((head p)*(fromIntegral (grado p)-1)):(derivada (tail p))

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


-- productoAux :: Polinomio -> Polinomio -> [Polinomio]
-- productoAux p1 p2
--       | length p1 == 0 = [[]]
--       | otherwise =  (productoPorMonomio (head p1) p2):(productoAux (tail p1) p2)
--
-- producto :: Polinomio -> Polinomio -> [Polinomio]
-- producto p1 p2 = productoAux p1 p2


division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division [] q = ([], [])
division p q | grado p < grado q = ([], p)
             | otherwise = ((head p / head q):(fst resultado), (snd resultado))
              where resultado = division (resta p (productoPorMonomio (head p/ head q) (grado p - grado q) q)) q

resta :: Polinomio -> Polinomio -> Polinomio
resta p q = suma p (productoPorEscalar (-1) q)


mcdp :: Polinomio -> Polinomio -> Polinomio
mcdp p [] = productoPorEscalar (1.0/head p) p
mcdp p q = mcdp q (snd (division p q))

multiplicidad :: Float ->  Polinomio -> Integer
multiplicidad x p | not (esRaiz x p) = 0
                  | esRaiz x p = 1 + multiplicidad x (fst (division p [1, -x]))

esRaiz :: Float -> Polinomio -> Bool
esRaiz n pd = (evaluar poli n == 0)

raicesMultiples :: Polinomio -> Bool
raicesMultiples p = mcdp p (derivada p) /= 1








 ---
