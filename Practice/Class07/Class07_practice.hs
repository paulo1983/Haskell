--función listar
listar::a->a->a->[a]
listar a b c = [a,b,c]

listar2::a->a->a->[a]
listar2 a b c = a:(b:(c:[]))

listaDecreciente::[Integer]
listaDecreciente = 1:[0,(-1)..(-100)]

sumatoria::[Integer]->Integer
sumatoria l | length l == 0 = 0
            | otherwise = head l + sumatoria (tail l)

pertenece::Integer->[Integer]->Bool
pertenece n l | length l == 0 = False
              | head l == n = True
              | otherwise = pertenece n (tail l)

productoria::[Integer]->Integer
productoria l | length l == 0 = 1
              | otherwise = head l * productoria (tail l)

sumarN::Int->[Int]->Int
sumarN n l = n*(length l) + sum l

sumarElUltimo::[Int]->[Int]
sumarElUltimo l = sumaUltimoElemento l []

sumaUltimoElemento::[Int]->[Int]->[Int]
sumaUltimoElemento l p | (length l) == 0 = []
                       | otherwise = ((head l) + last l):(sumaUltimoElemento (tail l) p)

sumarElPrimero::[Int]->[Int]
sumarElPrimero l = reverse (sumaPrimerElemento l [])

sumaPrimerElemento::[Int]->[Int]->[Int]
sumaPrimerElemento l p | (length l) == 0 = []
                       | otherwise = ((last l) + head l):(sumaPrimerElemento (init l) p)

pares::[Integer]->[Integer]
pares l | length l == 0 = []
        | (mod (head l) 2)== 0 = (head l):pares(tail l)
        | otherwise = pares (tail l)

multiplosDeN::Integer->[Integer]->[Integer]
multiplosDeN n l | length l == 0 = []
                 | (mod (head l) n) == 0 = (head l):(multiplosDeN n (tail l))
                 | otherwise = multiplosDeN n (tail l)

quitar::Integer->[Integer]->[Integer]
quitar n l | length l == 0 = []
           | (head l) == n && n/=0 = (quitar 0 (tail l))
           | otherwise = (head l):(quitar n (tail l))

hayRepetidos::[Integer]->Bool
hayRepetidos l | (length l == 0) = False
               | otherwise = (auxHayRepetidos (head l)  l)>1 || hayRepetidos (tail l)

auxHayRepetidos::Integer->[Integer]->Integer
auxHayRepetidos h p | length p == 0 = 0
                    | h == head p = 1 + auxHayRepetidos h (tail p)
                    | otherwise = auxHayRepetidos h (tail p)

eliminarRepetidos::[Integer]->[Integer]
eliminarRepetidos l | length l == 0 = []
                    | (auxHayRepetidos (head l) l)>1 = eliminarRepetidos (tail l)
                    | otherwise = (head l):eliminarRepetidos (tail l)

maximo::[Integer]->Integer
maximo l | length l == 1 = head l
         | head l > maximo (tail l) = head l
         | otherwise = maximo (tail l)

ordenarDec::[Integer]->[Integer]
ordenarDec l | length l == 0 = []
             | otherwise = (maximo l):(ordenarDec (quitar (maximo l) l))

ordenarCre::[Integer]->[Integer]
ordenarCre l = reverse (ordenarDec l)

longitud::[a]->Integer
longitud [] = 0
longitud (x:xs) = longitud xs + 1

pertenece2::Integer->[Integer]->Bool
pertenece2 y [] = False
pertenece2 y (x:xs) = pertenece2 y xs || (y == x)

tieneDosElementos::[a] -> Bool
tieneDosElementos ( _: _: [] ) = True
tieneDosElementos  _  =  False



--Blanck
