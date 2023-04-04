--1-1
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--1-2
-- Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--1-3
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
-- ej: sucesores [1,2,3]
sucesores :: [Int] -> [Int]
sucesores  [] = []
sucesores (n : ns)  = n + 1 : sucesores ns

-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
-- ej: conjuncion [True, True, True] 
--1-4 
conjuncion :: [Bool] -> Bool
conjuncion [] = error "La lista no puede ser vacia"
conjuncion (b: bs) = b == True && conjuncion bs   
-- nose porque me dice que la lista es vacia luego de hacer la conjuncion

--1-5
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
-- ej: conjuncion [True, False, False] 
disyuncion :: [Bool] -> Bool
disyuncion [] = error "La lista no puede ser vacia"
disyuncion (b: bs) = b == True || disyuncion bs   


--1-6
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
-- ej aplanar [[1,2], [2,3]]
-- aplanar :: [[a]] -> [a]
-- aplanar [(x:xs), _]     =
-- aplanar [[(l:ls)]]   =
-- aplanar (x:xs) = x aplanar xs ???

--1-7
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual a e.
-- ej: pertenece a -> [a] -> Bool
pertenece :: Eq a => a -> [a] -> Bool
pertenece k []     = False
pertenece k (x:xs) = x == k || pertenece k xs

--1-8
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs
-- ej: apariciones 4 [4,4,5]
apariciones :: Eq a => a -> [a] -> Int
apariciones _   []   = 0
apariciones e (x:xs) = if e == x 
                       then 1 + apariciones e xs
                       else apariciones e xs

--1-9
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
-- ej: losMenoresA 4 [1,2,3,4]
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA o []     = []
losMenoresA o (n:ns) = if n < o 
                       then n : losMenoresA o ns
                       else losMenoresA o ns

-- 10. lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos.  ???

--1-11
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] _      = []
agregarAlFinal (x:xs) e  =  : agregarAlFinal xs e





















