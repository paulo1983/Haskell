
type Set a = [a]
incluido::Set Integer -> Set Integer -> Bool
incluido a b | length a == 0 = True
             | length a == 1 && (elem (head a) b) = True
             | otherwise = elem (head a) b && incluido (tail a) b

iguales::Set Integer->Set Integer -> Bool
iguales a b | length a ==0 && length b == 0 = True
            | length a == 0 && length b /=0 = False
            | pertenece (head a)  b = iguales (quitar (head a) a) (quitar (head a) b)
            | otherwise = False


pertenece::Integer->[Integer]->Bool
pertenece n l | length l == 0 = False
              | head l == n = True
              | otherwise = pertenece n (tail l)


quitar::Integer->[Integer]->[Integer]
quitar n l | length l == 0 = []
           | (head l) == n && n/=0 = (quitar 0 (tail l))
           | otherwise = (head l):(quitar n (tail l))

agregarATodas::Integer->[[Integer]]->[[Integer]]
agregarATodas n l | length l == 0 = []
                  | otherwise = (auxAgregarN n (head l)):(agregarATodas n (tail l))

auxAgregarN::Integer->[Integer]->[Integer]
auxAgregarN n l | length l == 0 = n:[]
                | otherwise = (head l):(auxAgregarN n (tail l))

-- partes::Integer->Set (Set Integer)
-- partes n | n == 0 = []:[]
--          | otherwise = (auxPartes [1..n] (head [1..n])):(partes (n-1))

auxPartes::[Integer]->Integer->[[Integer]]
auxPartes l n | length l ==0 = []:[]
              | otherwise = l:[1..n]:(auxPartes (tail l) (n-1))

-- subsequences  :: [a] -> [[a]]
-- subsequences xs         =  [] : nonEmptySubsequences xs

sumatoriaL :: Integer -> Integer
sumatoriaL n = sum [ i^n | i <- [1..n]]

sumatoria::Integer->Integer
sumatoria n = auxSumatoria n n

auxSumatoria::Integer->Integer->Integer
auxSumatoria n i | i==1=1
                 |otherwise = i^n + auxSumatoria n (i-1)


--insertarEn::[Integer]->Integer->Integer->[Integer]
insertarEn l n i | length l ==0 && i<=(length l+1) = n:[]
                 | i<=(length l+1) = (take (i-1) l)++(n:(drop (i-1) l))


--insertarEn [1,2] 5 2 => [1,5,2]
