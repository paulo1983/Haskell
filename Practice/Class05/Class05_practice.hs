--función signo determina si un número es positivo, negativo o cero
signo::Float->Float
signo n | n>0 = 1
        | n==0 = 0
        | otherwise = (-1)


--función fact
fact::Integer->Integer
fact n | n == 0 = 1
       | n == 1 = 1
       | otherwise = n*(fact (n-1))


--funcion eAprox
eAprox::Integer->Double
eAprox n | n == 0 = 0.0
         | otherwise =   1/(fromIntegral(fact n)) + eAprox (n-1)

--función e constante
e :: Double
e = eAprox 100

menorDivisor::Integer->Integer
menorDivisor n = menorDivisorAuxiliar n 2

menorDivisorAuxiliar::Integer-> Integer->Integer
menorDivisorAuxiliar n divisor | n == 0 = 0
                               | (mod n divisor == 0 ) = divisor
                               | abs n == 1 = 1
                               | otherwise = menorDivisorAuxiliar n (divisor+1)


--funcion divisionEnteros extendida para los enteros
division::Integer->Integer->(Integer,Integer)
division x y | x < y && x >= 0 = (0,x)
             | x >= y = (fst (division (x-y) y) + 1, snd (division (x-y) y))
             | x < 0 = (fst (division (x+y) y) - 1,  snd (division (x+y) y))



--función sumaDivisores
sumaDivisores::Integer->Integer
sumaDivisores n  = sumaDivisoresHasta n n

sumaDivisoresHasta::Integer->Integer->Integer
sumaDivisoresHasta n x | x==1 = 1
                       | (mod n x) == 0 = (sumaDivisoresHasta n (x-1)) + x
                       | (mod n x) /= 0 = (sumaDivisoresHasta n (x-1))



--funcion parteEntera
parteEntera::Float->Integer
parteEntera n | (n<1) && (n>(-1)) = 0
              | n>=1 = parteEntera (n-1) + 1
              | n<0 = parteEntera (n+1)-1







--Blanck
