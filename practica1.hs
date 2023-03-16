-- 2-1-a

sucesor :: Int -> Int
sucesor x = x + 1

-- 2-1-b
sumar :: Int -> Int -> Int
sumar n m = n + m

-- 2-1-c
-- Dado dos números, devuelve un par donde la primera componente es la división del
-- primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
-- para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
-- provista por Haskell.
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n m, mod n m)

-- 2-1-d
-- Dado un par de números devuelve el mayor de estos
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if (n > m) then n 
                   else m 

-- 2-2
-- De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
-- Ejemplo: maxDelPar (divisionYResto (sumar 5 5), (sucesor 0))










