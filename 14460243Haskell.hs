
------------------------------------------
--	Hecho por Ricardo Villagrana	--
------------------------------------------


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
