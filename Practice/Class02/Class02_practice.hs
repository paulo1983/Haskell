-- Clase 2  update: 07/04/2017

--función signo determina si un número es positivo, negativo o cero
signo::Float->Float
signo n | n>0 = 1
        | n==0 = 0
        | otherwise = (-1)

--función absoluto que calcula el valor absoluto de número
absoluto::Float->Float
absoluto x | x>0 = x
           | x==0 = 0
           | otherwise = x*(-1)

-- otra forma de calcular el valor absoluto de número
absoluto2::Float->Float
absoluto2 x = (signo x)*x

-- función máximo devuelve el mayor de dos números
maximo::Float->Float->Float
maximo x y | x >= y = x
           | otherwise = y

--  función máximo3 devuelve el mayor de tres números
maximo3::Float->Float->Float->Float
maximo3 x y z | z >= (maximo x y) = z
              | otherwise = (maximo x y)

--  otra función máximo3b devuelve el mayor de tres números
maximo3b::Float->Float->Float->Float
maximo3b x y z = maximo z (maximo x y)

-- funcion doble devuelve un n*2
doble::Num a=>a->a
doble x = x+x

-- funcion cuadruple devuelve un n*4
cuadruple::Num a=>a->a
cuadruple x = doble (doble x)

-- función normaVectorial devuelve la norma de un vector (a,b)
normaVectorial::(Float, Float)->Float
normaVectorial v = sqrt((fst v)^2 + (snd v)^2)

-- función dist devuelve la distancia del primero con
dist :: Float -> Float -> Float -> Float -> Float
dist a b c d = normaVectorial ((a-c),(b-d))

-- función que determina la paridad de un número
esPar::Int->Bool
esPar n = (mod n 2)==0

-- función esCuadradoPerfecto determina si un valor es un cuadrado perfecto.
esCuadradoPerfecto::Float -> Bool
esCuadradoPerfecto n = (sqrt n) == fromIntegral (truncate (sqrt n))

--función crearPar, devuelve una tupla de dos elementos (a,b)
crearPar:: a->b->(a,b)
crearPar a b = (a,b)

-- función invertir, invierte el orden de una tupla
invertir:: (a,b) -> (b,a)
invertir p = (snd p,fst p)

-- función distancia, devuelve la distancia entre dos vectores
distancia::(Float,Float)->(Float,Float)->Float
distancia v1 v2 = sqrt ((fst v1 - fst v2)^2+(snd v1 - snd v2)^2)

-- función f1 contruye una tripla de una valor real
f1::Float->(Float,Float,Float)
f1 x = (2*x,x^2,x-7)

-- función f2 toma un entero si es par lo divide en 2 sino le suma 1
f2::Int->Int
f2 n | esPar n = div n 2 | otherwise = n+1

--función f si n es divisible por 6 hace el cuadrado y lo divide por 2
-- sino los triplica y le suma 1
f::Int->Int
f n | (mod n 6) == 0 = (div (n^2) 2) | otherwise = (3*n + 1)

-- función g toma el primer elemento de una tupla numerica
-- y lo multiplaca por el segundo elemento + 1
g::(Int,Int)->Int
g p = (fst p) * ((snd p) + 1)

--función h (f o g)
h::(Int,Int)->Int
h n = f(g(fst n, snd n))
