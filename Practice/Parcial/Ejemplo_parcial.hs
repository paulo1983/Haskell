--hayMultiplo∷(Integer,Integer)->Integer->Bool
--hayMultiplo (a,b) n  = ( mod a n == 0) || ( mod b n == 0) = True

f::Integer->Integer
f n = (product [1..n])^2 - (sum [1..(n-1)])

f2 ::Integer->Integer
f2 n = prodAux n - sumaAux n (n-1)

prodAux::Integer->Integer
prodAux 0 = 1
prodAux 1 = 1
prodAux n = n^2 * (prodAux (n-1))

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

fibo::Int->Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1)+ fibo (n-2)

sumaFibo::Int->Int
sumaFibo n | n == 0 = 0
           | otherwise = fibo n + sumaFibo (n-1)

--g::Integer->Integer->Integer
--g n m | (n == 0) && (m == 0) = 0
--      | n<m = sumaHastaN * sumaHastaM

--sumaHastaN::Integer->Integer
--sumaHastaN 0 = 0
--sumaHastaN n = n + sumaHastaN (n-1)

--sumaHastaM::Integer->Integer
--sumaHastaM 0 = 0
--sumaHastaM m = m + sumaHastaM (m-1)

g n m = (sumatoria n 1)*(sumatoria m n)

sumatoria n j | n<j = 0
              | n ==j = n
              | otherwise = n +  sumatoria (n-1) j

prodDig::Integer->Integer
prodDig n | n<10 = n
          | otherwise = (mod n 10) * prodDig (div n 10)

          


--Blanck
