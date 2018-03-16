
------------------------------------------
--	Hecho por Ricardo Villagrana	--
------------------------------------------

-- Imports
import Data.List

-- Raiz cuadrada de equación cuadrática
-- sqrtEc(a, b, c)
-- @Params 
--  a = Término independiente
--  b = Término dependiente
--  c = Término cuadrático

sqrtEc :: (Float, Float, Float) -> (Float, Float)  
sqrtEc (a,b,c) = (r1,r2) where
    r1 = ((-b + sqrt((b * b) - 4 * a * c)) / (2 * a))
    r2 = ((-b - sqrt((b * b) - 4 * a * c)) / (2 * a))

-- F° -> C°
-- ftoc(F)
-- @Params
--  F = Grados en Farenheit
-- @Returns
--  C = Grados en Celsius

ftoc :: Float -> Float
ftoc f = c where
    c = 5 * (f - 32) / 9

-- Inversa de una cadena
-- inverseOf(str)
-- @Params str
--  str = Cadena a invertir ("cadena")
-- @Returns
--  strInv = Cadena invertida ("anedac")


inverseOf :: String -> String 
inverseOf str = rts where
    rts = 
        if length str > 1 then
            inverseOf(tail str) ++ take 1 str
        else
            str

-- Fibonacci
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x-1) + fibonacci(x-2)

fibList a = [fibonacci x | x <- [0..a-1]]

-- Factorial
factorial :: Int -> Int
factorial n = product [1..n]

-- Insertar en medio de una lista
inmid :: Num a => [a] -> a -> [a]
inmid y x = ((take ((length y) `div` 2) y) ++ [x] ++ (reverse (take ((length y) `div` 2) (reverse y))))

-- Insertar en lugar seleccionado de una lista
inpos :: Num a => [a] -> a -> Int -> [a]
inpos y x pos = (take pos y ++ [x] ++ reverse (take (length y - pos) (reverse y)))

-- Definición de clase de tipos
lucky :: (Integral a) => a -> String
lucky 7 = "Suerte"
lucky x = "No suerte"

weekDay :: (Integral x) => x ->String
weekDay 1 = "Domingo"
weekDay 2 = "Lunes"
weekDay 3 = "Martes"
weekDay 4 = "Miércoles"
weekDay 5 = "Jueves"
weekDay 6 = "Viernes"
weekDay 7 = "Sábado"
weekDay x = "Inválido"


-- Rotación izquierda
rotacionIZQ :: Num a => [a] -> [a]
rotacionIZQ a = tail a ++ [head a]

-- Rotación derecha
rotacionDER :: Num a => [a] -> [a]
rotacionDER a = last a : init a

-- Rotación con movimiento y dirección
rotar lista cantidad movimiento
    | movimiento == 'I' || movimiento == 'i' = drop n1 lista ++ take n1 lista
    | movimiento == 'D' || movimiento == 'd' = drop n2 lista ++ take n2 lista
    | otherwise = error "Movimiento no válido"
    where   n2 = if(cantidad <= length lista) then (length lista - cantidad) else (length lista - (cantidad `mod` length lista))
            n1 = if(cantidad <= length lista) then cantidad else (cantidad `mod` length lista) 

-- Eliminar la primera incidencia de un número de una lista
-- Usa la librería de listas
eliminaItem lista num = (lista \\ [num])
-- Sugerido, pero menos óptimo (pero pensamos más)
eliminaItem' lista num = takeWhile(/=num) lista ++ tail (dropWhile(/=num) lista)

-- Verificar si es palíndromo
palindromo palabra = if (palabra == reverse palabra) then True else False

-- Eliminar todas las incidencias
eliminaAll lista caracter = [ x | x <- lista, x /= caracter ]

-- Calcular los factores 
factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

-- Será primo solo si sus factores son 1 y él mismo
esPrimo :: Int -> Bool
esPrimo n = factores n == [1, n]

-- Números primos
primos n = [x | x <- [2..n], esPrimo x]


primos' (x:xs) y = 
    if (x^2 < y) then
        [x] ++ primos' [b | b <- xs, mod b x /= 0] y
    else [x] ++ xs

-- Números perfectos

perfectos:: Integral a => a -> [a]
perfectos n = [x | x <- [1..n], sum (init (fac x)) == x]

-- Mínimos múltiplos
mcd a b
    | b == 0 = a
    | b >= 1 = mcd b (a `mod` b)
    | otherwise = error "Número inválido"

mcm x y = ((x * y) `div` (mcd x y))

-- Factores de número
fac n = [x | x <- [1..n], n `mod` x == 0]

-- Ternas pitagóricas
terna n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Mezclar listas ordenadas
merge :: Integral a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = 
    if x == y
        then x : y : merge xs ys
    else if x < y
        then x : merge xs (y:ys)
    else y : merge (x:xs) ys

quickSort [] = []
quickSort (x:xs) =  quickSort (filter (< x) xs) 
                    ++ [x] ++ 
                    quickSort (filter (>= x) xs)

mergeSort :: Integral a => [a] -> [a] -> [a]
mergeSort x y = merge (quickSort x) (quickSort y)
    