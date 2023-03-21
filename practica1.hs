------
----
--

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
maxDelPar (n, m) =
  if (n > m)
    then n
    else m

-- 2-2
-- De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.
-- Ejemplo: maxDelPar (divisionYResto (sumar 5 5), (sucesor 0))


--------------------------------------------------------------------------------------------------

-- 3 - Tipos Enumerativos

--1
data Dir = Norte | Este | Sur | Oeste
     deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

--1-b
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales _ _ = False

--1-c
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

--2
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
        deriving Show

--2--a
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

--2--b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

--2--c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False

--2--d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True



--3-3-a
negar :: Bool -> Bool
negar True = False
negar False = True

--3-3-b
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

--3-3-c
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

--3-3-d
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False

--4-1
data Persona = P String Int  String
              -- Nombre Edad DNI
     deriving Show

nombre :: Persona -> String
nombre (P n _ _) = n

edad :: Persona -> Int
edad (P _ e _) = e

crecer :: Persona -> Persona
crecer (P n e d) = P n (e+1) d

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre a (P n e d) = P a e d

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e d) (P m f r) = e>f

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n e d) (P m r f) = if(e>r)
    then (P n e d)
    else (P m r f)


--4--2
-- Definir tipo de dato Pokemon
data Pokemon = K TipoDePokemon Int
              -- Tipo          Energia 
     deriving Show

data TipoDePokemon = Agua | Fuego | Planta
     deriving Show

data Entrenador = E String Pokemon Pokemon
                --  nombre P1      P2
     deriving Show
    
superaA :: Pokemon -> Pokemon -> Bool
superaA (K Agua _) (K Fuego _) = True
superaA (K Fuego _) (K Planta _) = True
superaA (K Planta _) (K Agua _) = True
superaA (K _ _) (K _ _) = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
-- cantidadDePokemonDe 








