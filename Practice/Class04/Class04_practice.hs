--función fact
fact::Integer->Integer
fact n | n == 0 = 1
       | n == 1 = 1
       | otherwise = n*(fact (n-1))

--función doblefact
doblefact::Integer->Integer
doblefact n | n == 0 = 1
            | n == 1 = 0
            | otherwise = n*doblefact(n-2)

-- función par
par::Integer->Bool
par n | n == 0 = True
      | abs(n) == 1 = False
      | otherwise = par ((abs(n))-2)

-- función par2
par2::Integer->Bool
par2 n | n == 0 = True
       | otherwise = not (par (n-1))

-- función sumaImpares
sumaImpares::Integer->Integer
sumaImpares n | n == 0 = 0
              | otherwise = (2*n-1) + (sumaImpares(n-1))

-- función esMultiploDe3
esMultiploDe3::Integer->Bool
esMultiploDe3 n | n == 0 = True
                | abs(n) == 1 = False
                | otherwise = esMultiploDe3 (abs(n)-3)

-- función comb
comb::Integer->Integer->Integer
comb n m | m == 0 = 1
         | n == 0 = 0
         | m > n  = 0
         | otherwise = (comb (n-1) (m-1)) + (comb (n-1) m)

--función fib
fib::Integer-> Integer
fib n | n == 0 = 1
      | n == 1 = 1
      | otherwise = (fib (n-1)) + (fib (n-2))

--funcion  sumaImparesCuyoCuadSeaMenorQue n
sumaImparHasta::Integer->Integer
sumaImparHasta n | n == 0 = 0
                 | par n = 2*(div n 2)-1 + (sumaImpares ((div n 2)-1))
                 | otherwise =  2*(div (n-1) 2)-1 + (sumaImpares ((div (n-1) 2)-1))

cuadMenorQue::Float->Integer
cuadMenorQue n | n == 0 = 0
               | otherwise = sumaImparHasta((truncate(sqrt(n)))) + (truncate(sqrt(n)))
