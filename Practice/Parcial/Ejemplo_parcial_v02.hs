hayMultiplo::(Integer,Integer)->Integer->Bool
hayMultiplo (a,b) n  = ( (mod a n) == 0) || ( (mod b n) == 0)


--f::Integer->Integer
--f n = (product [1..n])^2 - (sum [1..(n-1)])

f2::Integer->Integer
f2 n = prodAux n - sumaAux n (n-1)

prodAux::Integer->Integer
prodAux 0 = 0
prodAux 1 = 1
prodAux i = i^2 * (prodAux (i-1))

sumaAux::Integer->Integer->Integer
sumaAux 0 _ = 0
sumaAux n 0 = 0
sumaAux n i = n^i + sumaAux n (i-1)

numFibonacci::Integer->Integer
numFibonacci n | n == 0 = 0
               | n == 1 = 1
               | otherwise = numFibonacci (n-1) + numFibonacci (n-2)

sumaFibonacci::Integer->Integer
sumaFibonacci n | n == 0 = 0
                | otherwise =  numFibonacci n + sumaFibonacci (n-1)

fibo::Integer->Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1)+ fibo (n-2)

sumaFibo::Integer->Integer
sumaFibo n | n == 0 = 0
           | otherwise = fibo n + sumaFibo (n-1)

g3::Integer->Integer->Integer
g3 n m = (auxSumatoria n)*(auxSumatoria2 n m)

auxSumatoria::Integer->Integer
auxSumatoria i | i==0 = 0
               | i==1 = 1
               | otherwise = i + (auxSumatoria (i-1))

auxSumatoria2::Integer->Integer->Integer
auxSumatoria2 n j | j<n = 0
                  | j==n = n
                  | otherwise = j + (auxSumatoria2 n (j-1))

g n m = (sumatoria n 1)*(sumatoria m n)

sumatoria n j | n<j = 0
              | n ==j = n
              | otherwise = n +  sumatoria (n-1) j

prodDig::Integer->Integer
prodDig n | n<10 = n
          | otherwise = (mod n 10) * prodDig (div n 10)

-- quita un elemento existente en una lista
filtro::[Integer]->Integer->[Integer]
filtro lista elemento | length lista == 0 = []
                      | head lista == elemento = filtro(tail lista) elemento
                      | head lista /= elemento = (head lista):filtro(tail lista) elemento

juntarIndices::Integer->Integer->[Integer]->[Integer]
juntarIndices i e l
    | length l == 0 = []
    | head l == e = i:juntarIndices (i + 1) e (tail l)
    | head l /= e = juntarIndices (i + 1) e (tail l)

posiciones::Integer->[Integer]->[Integer]
posiciones e l = juntarIndices 1 e l

inserta::Integer->[Integer]->[Integer]
inserta e l | length l == 0 = e:l
            | e > head l = (head l):(inserta e (tail l))
            | otherwise = e:l

ordenaPorInsercion::[Integer]->[Integer]
ordenaPorInsercion l | length l == 0 = []
                     | otherwise = inserta (head l) (ordenaPorInsercion (tail l))


esFibo::Integer->Bool
esFibo n = auxEsFibo n 0

auxEsFibo::Integer->Integer->Bool
auxEsFibo n j | (fibo j) == n = True
              | (fibo j) > n = False
              | otherwise = auxEsFibo n (j+1)

mezcla::[Integer]->[Integer]->[Integer]
mezcla xs ys = ordenaLista xs ys

ordenaLista::[Integer]->[Integer]->[Integer]
ordenaLista xs ys | (length xs == 0) = ys
                  | (length ys == 0) = xs
                  | (head xs) <= (head ys) = (head xs):(ordenaLista (tail xs) ys)
                  | otherwise = (head ys):(ordenaLista  xs  (tail ys))

longitudCamino::[(Integer,Integer)]->Float
longitudCamino lista | length lista == 1 = 0.0
                     | otherwise = (longitudCaminoAux (head lista) (head (tail lista))) + longitudCamino (tail lista)

longitudCaminoAux::(Integer,Integer)->(Integer,Integer)->Float
longitudCaminoAux (a,b) (c,d) = sqrt(fromIntegral((c-a)^2 + (d-b)^2))


incmin::[Integer]->[Integer]
incmin lista = sumaEnteroALista lista (auxMin lista (head lista))

sumaEnteroALista::[Integer]->Integer->[Integer]
sumaEnteroALista lista n | length lista == 0 = []
                         | otherwise = ((head lista)+ n):(sumaEnteroALista (tail lista) n)

auxMin::[Integer]->Integer->Integer
auxMin lista m | (length lista == 0) = m
               | (head lista) <= m = auxMin (tail lista) (head lista)
               | otherwise = auxMin (tail lista) m

kesimo::[Int]->Int->Int
kesimo xs k | (k == 1) = (head xs)
            | k <= (length xs) = kesimo (tail xs) (k-1)

domina::[Integer]->[Integer]->Bool
domina xs ys  | (length xs == 0) = False
              | (length xs == 1) = (head xs) > (head ys)
              | otherwise = ((head xs) > (head ys)) && (domina (tail xs) (tail ys))

esTipoFibo::[Int]->Bool
esTipoFibo xs | length xs < 3 = False
              | length xs == 3 = ((auxEsTipoFibo xs 3)==(auxEsTipoFibo xs 2)+(auxEsTipoFibo xs 1))
              | otherwise = ((auxEsTipoFibo xs (length xs))==(auxEsTipoFibo xs ((length xs)-1))+(auxEsTipoFibo xs ((length xs)-2))) && (esTipoFibo (tail xs))

auxEsTipoFibo::[Int]->Int->Int
auxEsTipoFibo xs n | n==1 = head xs
                   | otherwise = auxEsTipoFibo (tail xs) (n-1)

esNCreciente::Integer->Bool
esNCreciente n | n<100 && n>10 = ((mod n 10)>(mod (div n 10) 10))
               | (n>=100) = ((mod n 10)>(mod (div n 10) 10)) && (esNCreciente (div n 10))


-------------------------------------------------------------------
-------------------------------------------------------------------
-- EJERCICIOS DEL PARCIAL DE TALLER
-------------------------------------------------------------------
-------------------------------------------------------------------

esPotenciaDe::Integer->Integer->Bool
esPotenciaDe x y = esPotenciaDeAux x y 1

esPotenciaDeAux::Integer->Integer->Integer->Bool
esPotenciaDeAux x y n | y == 1 = True
                      | (x<y) && (x^n==y) = True
                      | x^n>y = False
                      | otherwise = esPotenciaDeAux x y (n+1)

pitagoras::Integer->Integer->Integer->Integer
pitagoras m n r = (pitagorasAux m n r 0)

pitagorasAux::Integer->Integer->Integer->Integer->Integer
pitagorasAux p q r i | ((iteracionP p)^2+(iteracionQ q)^2)==0 = (i+1)
                     | ((iteracionP p)^2 + (iteracionQ q)^2)<= (r^2) = (i+1) + (pitagorasAux p q r i)

iteracionP::Integer->Integer
iteracionP p | p == 0 = 0
             | otherwise = iteracionP (p-1)

iteracionQ::Integer->Integer
iteracionQ q | q == 0 = 0
             | otherwise = iteracionQ (q-1)


sumaDistintosPaulo::Integer->Integer->Integer->Integer
sumaDistintosPaulo a b c | (a==b && a==c)= a
                         | a==b = a+c
                         | a==c = a+b
                         | b==c = a+c
                         | otherwise = a+b+c

--1
sumaDistintos::Integer->Integer->Integer->Integer
sumaDistintos a b c | (a/=b) && (b/=c) && (a/=c) = (a+b+c)
                    | (a/=b) && (b/=c) && (a==c) = (a + b)
                    | (a/=b) && (b==c) && (a/=c) = (a + b)
                    | (a==b) && (b/=c) && (c/=a) = (a + c)
                    | otherwise = a

--2
suma::Integer->Integer
suma n = (auxSumaCuadrado (2*n)) + (auxSumaDoble (2*n))

auxSumaCuadrado::Integer->Integer
auxSumaCuadrado i | i == 0 = 0
                  | i == 1 = 1
                  | otherwise = (i^2) + (auxSumaCuadrado (i-1))

auxSumaDoble::Integer->Integer
auxSumaDoble i | i==0 = 0
               | i == 1 = 2
               | otherwise = (2*i) + (auxSumaDoble (i-1))

--3
esPotenciaDeLucas::Integer->Integer->Bool
esPotenciaDeLucas x y = auxEsPotenciaDe x y 0

auxEsPotenciaDe::Integer->Integer->Integer->Bool
auxEsPotenciaDe x y i | (x^i) == y = True
                      | (x^i) > y = False
                      | otherwise = auxEsPotenciaDe x y (i+1)

-- ejercicio 24, todo n se puede escribir como suma distintas potencias de 2
potenciasDeDos::Integer->[(Integer,Integer)]
potenciasDeDos n = potenciasDeDosAux n 0 False

potenciasDeDosAux::Integer->Integer->Bool->[(Integer,Integer)]
potenciasDeDosAux n k v  | n==0 = []
                         | (2^k)==n = (2,k):[]
                         | v == True = (2,k):potenciasDeDosAux (n-2^k) 0 False
                         | (2^k)<n && v==False = potenciasDeDosAux n (k+1) False
                         | (2^k)>n && v==False = potenciasDeDosAux n (k-1) True










-- pitagoras2::Integer->Integer->Integer->Integer
-- pitagoras2 m n r = auxPitagoras m 0 n 0 r False

-- auxPitagoras::Integer->Integer->Integer->Integer->Integer->Bool->Integer
-- auxPitagoras m p n q r b | m==p && n==q && ((p^2)+(q^2)) <= (r^2) = 1
--   | m==p && n==q && ((p^2)+(q^2)) > (r^2) = 0
--   | m==p && n/=q && ((p^2)+(q^2)) <= (r^2) = 1 + (auxPitagoras m p n (q+1) r)
--   | m==p && n/=q && ((p^2)+(q^2)) > (r^2) = 0 + (auxPitagoras m p n (q+1) r)
--   | m/=p && n==q && ((p^2)+(q^2)) <= (r^2) = 1 + (auxPitagoras m (p+1) n q r)
--   | m/=p && n==q && ((p^2)+(q^2)) > (r^2) = 0 + (auxPitagoras m (p+1) n q r)
--   | m/=p && n/=q && ((p^2)+(q^2)) <= (r^2) = 1 + (auxPitagoras m (p+1) n q r)
--   | m/=p && n/=q && ((p^2)+(q^2)) > (r^2) = 0 + (auxPitagoras m (p+1) n q r)
--

--Blanck
