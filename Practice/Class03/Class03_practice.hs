-- Clase 3  update: 07/04/2017

--función inversa
inv::Float->Float
inv x | x /= 0 = 1/x

-- función que devuelve la cantidad de dígitos de un número
digitos :: Int-> Int
digitos x | (div x 10) == 0 = 1 |otherwise = (digitos (div x 10)+1)

-- otra función digitos
digitos2::Float->Int
digitos2 n = (truncate(logBase 10 n))+ 1

--función unidades dado un entero, devuelve el dígito de las unidades del número
unidades::Int->Int
unidades n = mod n 10

--función sumaUnidades3 suma de los dígitos de las unidades de 3 números
sumaUnidades3::Int->Int->Int->Int
sumaUnidades3 a b c = unidades a + unidades b + unidades c

--función todosImpares, determina de 3 números si son todos impares
todosImpares::Int->Int->Int->Bool
todosImpares a b c = (not (esPar a)) && (not (esPar b)) && (not (esPar c))

--función alMenosUnImpar, determina si al menos uno es impar de tres
alMenosUnImpar::Int->Int->Int->Bool
alMenosUnImpar a b c = (not (esPar a)) || (not (esPar b)) || (not (esPar c))

--función alMenosDosImpares, determina si al menos uno es impar de tres
alMenosDosImpares::Int->Int->Int->Bool
alMenosDosImpares a b c | (not (esPar a)) && (not (esPar b)) = True
                        | (not (esPar b)) && (not (esPar c)) = True
                        | (not (esPar a)) && (not (esPar c)) = True
                        | otherwise = False

--función alMenosDosPares, determina si al menos uno es impar de tres
alMenosDosPares::Int->Int->Int->Bool
alMenosDosPares a b c = not (alMenosDosImpares a b c)

--función r1 determina si a b tienen la misma paridad
r1::Int->Int->Bool
r1 a b | (esPar a) && (esPar b) = True
       | not (esPar a) && not (esPar b) = True
       | otherwise = False

-- función r2 si 2a + 3b es divisible por 5
r2::Int->Int->Bool
r2 a b = (mod (2*a + 3*b) 5) == 0

--función r3 determina si los digitos de a b y ab son todos distintos
r3::Int->Int->Bool
r3 a b = ((unidades a) /= (unidades b)) && ((unidades (a*b)) /= (unidades a)) && ((unidades (a*b)) /= (unidades b))

-- función r4 determine si x ∼ y en la partición: R = (−∞,3) ∪ [3,+∞)
r4::Float->Float->Bool
r4 x y | (x < 3) && (y < 3) = True
       | (x >= 3) && (y >= 3) = True
       | otherwise = False

-- función r5 determine si x ∼ y en la partición: R = (−∞,3) ∪ [3,7) ∪ [7,+∞)
r5::Double->Double->Bool
r5 x y | (x < 3) && (y < 3) = True
       | (x >= 3) && (y >= 3) && (x < 7) && (y < 7) = True
       | (x >= 7) && (y >= 7) = True
       | otherwise = False

--función vectoresAlineados
vectoresAlineados ::(Int,Int)->(Int,Int)->Bool
vectoresAlineados (a,b) (p,q) = ((mod a p) == 0) && ((mod b q) == 0) && (div a p) == (div b q)

--función vectoresAlineados2
vectoresAlineados2 ::(Double,Double)->(Double,Double)->Bool
vectoresAlineados2 (a,b) (p,q) = (a/p) == (b/q)

--función es par
esPar:: Int -> Bool
esPar x = mod x 2 == 0

--sumaC suma dos números complejos
sumaC :: (Float,Float) -> (Float, Float) -> (Float, Float)
sumaC z1 z2 = ((fst z1 + fst z2),(snd z1 + snd z2))

-- función productoC producto entre dos números pcomplejos
productoC :: (Float,Float) -> (Float,Float) -> (Float, Float)
productoC (a,b) (c,d) = (a*c-b*d,a*d+b*c)

--función productoPorRealC producto de un n complejo por un real
productoPorRealC::(Float, Float)->Float->(Float, Float)
productoPorRealC (a,b) k = (k*a,k*b)

--función conjugadoC conjugado de un n complejo
conjugadoC::(Float, Float)->(Float, Float)
conjugadoC (a,b) = (a,-b)

--función inversoC inverso de un n complejo
inversoC::(Float, Float)->(Float, Float)
inversoC (a,b) | a==0 && b==0 =(0,0)
               | otherwise = (productoPorRealC (conjugadoC (a,b)) (1/(a^2+b^2)))

--función raices, encuentra las raices de un polinomio de grado 2
raices:: Double -> Double -> Double -> ((Double, Double), (Double, Double))
raices a b c |a == 0 && b == 0 && c == 0 = ((0,0),(0,0))
             | (b^2-4*a*c) >= 0 = ((((-b)+sqrt(b^2-4*a*c))/(2*a),0),(((-b)-sqrt(b^2-4*a*c))/(2*a),0))
             | (b^2-4*a*c) < 0 = (((-b)/(2*a),sqrt((b^2-4*a*c)*(-1))/(2*a)),((-b)/(2*a),-(sqrt((b^2-4*a*c)*(-1))/(2*a))))
