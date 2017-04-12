-- Clase 3  update: 05/04/2017

-- funcion doble devuelve un n*2
doble::Num a=>a->a
doble x = 2*x

--funcion suma devuelve la suma de dos n
suma::Num a=>a->a->a
suma x y = x+y

-- función normaVectorial devuelve la norma de un vector (a,b)
normaVectorial::(Float, Float)->Float
normaVectorial v = sqrt((fst v)^2 + (snd v)^2)

--función constante 8
constante8::Num a => a -> a
constante8 x = 8

-- función que responde a todo el valor 42
respuestaATodo::Num a=>a
respuestaATodo = 42
