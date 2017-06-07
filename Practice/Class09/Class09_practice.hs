mcd::Integer->Integer->Integer
mcd a b | b==0 = a
        | otherwise = (mcd b (mod a b))


emcd::Integer->Integer->(Integer,Integer,Integer)
emcd a 0 = (a,1,0)
emcd a b = (g, t, s - (div a b)*t)
            where (g, s, t) = emcd b (mod a b)


mayorDivisor::Integer->Integer
mayorDivisor n = mayorDivisorAuxiliar n (n-1)

mayorDivisorAuxiliar::Integer-> Integer->Integer
mayorDivisorAuxiliar n divisor | n == 0 = 0
                               | (mod n divisor == 0 ) = divisor
                               | abs n == 1 = 1
                               | otherwise = mayorDivisorAuxiliar n (divisor-1)


--funcion divisionEnteros extendida para los enteros
division::Integer->Integer->(Integer,Integer)
division x y | x < y && x >= 0 = (0,x)
             | x >= y = (fst (division (x-y) y) + 1, snd (division (x-y) y))
             | x < 0 = (fst (division (x+y) y) - 1, snd (division (x+y) y))

tieneSolucion::Integer->Integer->Integer->Bool
tieneSolucion m a b = (mod b (mcd m a))== 0

solucionParticular::Integer->Integer->Integer->Integer
solucionParticular a b m = s *(div b g)
where (g,s,t) = emcd a m





--Blanck
