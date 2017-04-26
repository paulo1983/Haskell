--Pattern Matching
--funcion clase 6

yLogico::Bool->Bool->Bool
yLogico True True = True
yLogico _ _ = False


oLogico::Bool->Bool->Bool
oLogico False False = False
oLogico _ _ = True


implica::Bool->Bool->Bool
implica True False = False
implica _ _ = True


sumaGaussiana :: Integer->Integer
sumaGaussiana 0 = 0
sumaGaussiana n = n + sumaGaussiana (n-1)

algunoEsCero ::(Integer,Integer,Integer)->Bool
algunoEsCero (0,_,_) = True
algunoEsCero (_,0,_) = True
algunoEsCero (_,_,0) = True
algunoEsCero (_,_,_) = False

algunoEsCero2 ::(Integer,Integer,Integer)->Bool
algunoEsCero2 (x,y,z) = (x*y*z)==0

--productoInterno::(Float,Float)->(Float,Float)->(Float,Float)
--productoInterno (a,b) (c,d) = a*c + b*d


division::Integer->Integer->(Integer,Integer)
division x y | x < y && x >= 0 = (0,x)
             | x >= y = (fst (division (x-y) y) + 1, snd (division (x-y) y))
             | x < 0 = (fst (division (x+y) y) - 1,  snd (division (x+y) y))

menorDivisor::Integer->Integer
menorDivisor n = menorDivisorAuxiliar n 2

menorDivisorAuxiliar::Integer-> Integer->Integer
menorDivisorAuxiliar n divisor | n == 0 = 0
                               | (mod n divisor == 0 ) = divisor
                               | abs n == 1 = 1
                               | otherwise = menorDivisorAuxiliar n (divisor+1)

esPrimo::Integer->Bool
esPrimo n | n==0 = False
          | otherwise = menorDivisor n == 1 || menorDivisor n == n

sumaDeADos::Integer->Integer->Bool
sumaDeADos n q | n <= q = False
               | esPrimo q 1 && esPrimo (n-q) = True
               | otherwise = sumaDeADos n (q+1)

esSumaDeDosPrimos::Integer->Bool
esSumaDeDosPrimos n = sumaDeADos n 2


-- función comb
comb::Integer->Integer->Integer
comb n m | m == 0 = 1
         | n == 0 = 0
         | m > n  = 0
         | otherwise = (comb (n-1) (m-1)) + (comb (n-1) m)

--goldbach::Integer->Bool




                  --  | otherwise = esPrimo (esSumaDeDosPrimos (n-1)) && esPrimo (esSumaDeDosPrimos n)


  --  k ==0 = False
  --  4= k +4-k= esPrimo k && esPrimo (4-k) == True




























--Blanck
