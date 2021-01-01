--1

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) = if x < y then (y:x:ys)
	               else y:(insert' x ys)



--2

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Nothing:xs) = catMaybes' xs
catMaybes' (Just a:xs) = a:(catMaybes' xs)



--3

{-
data Exp a = Const a
           | Var String 
           | Mais (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show (Exp a) where
	 show (Const a) = show a
	 show (Var a) = show a
	 show (Mais x y) = "( " ++ (show x) ++ " + " ++ (show y) ++ " )"
	 show (Mult x y) = "( " ++ (show x) ++ " * " ++ (show y) ++ " )"
-}



--4 

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f x = sortOnAux f x []

sortOnAux :: Ord b => (a -> b) -> [a] -> [a] -> [a]
sortOnAux f [] x = x
sortOnAux f (x:xs) [] = sortOnAux f xs [x]
sortOnAux f (x:xs) (y:ys) = sortOnAux f xs (aux2 f x (y:ys))

aux2 :: Ord b => (a -> b) -> a -> [a] -> [a]
aux2 f x [] = [x]
aux2 f x (y:ys) | f x < f y = (x:y:ys)
                | otherwise = y:(aux2 f x ys)

--5

--a)

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude (x:xs) = pares (x:xs) (x,x)

pares :: [Int] -> (Int,Int) -> Int
pares [] (a,b) = b-a
pares (x:xs) (a,b) | x < a = pares xs (x,b)
                   | x > b = pares xs (a,x)
                   | otherwise = pares xs (a,b)

--b)

parte :: [Int] -> ([Int],[Int])
parte [] = ([],[])
parte (x:xs) = parte2 (x:xs) 



--6

--a)

data Imagem = Quadrado Int
            | Mover (Int,Int) Imagem
            | Juntar [Imagem]

conta :: Imagem -> Int
conta (Quadrado x) = 1
conta (Mover (a,b) x) = conta x 
conta (Juntar []) = 0
conta (Juntar (x:xs)) = conta x + conta (Juntar xs)

--b)

{-

apaga :: Imagem -> IO Imagem
apaga x = do
	       t <- randomRIO (0, conta x)
	       return (aux3452 x t)


aux3452 :: Imagem -> IO Int -> IO Imagem
aux3452 (Quadrado x) 0 = Juntar []
aux3452 (Mover a x) y = Mover a (aux3452 x y)
aux3452 (Juntar (x:xs)) y = aux356673 (Juntar (x:xs)) (conta (x:xs)) y


aux356673 :: Imagem -> Int -> IO Int -> IO Imagem
aux356673 (Juntar (x:xs)) y z | y >= z  

-}