module PF50 where

import Data.Char

--1)
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y | (x < y) = x : myenumFromTo (x+1) y
                 | (x > y) = x : myenumFromTo (x-1) y
                 | otherwise = [x]

--2)
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z | (x==z) || (x<z)&&(y>z) || (x>z) && (y<z) = [x]
                       | (x<z) = x : myenumFromThenTo y (2*y-x) z
                       | (x>z) = x : myenumFromThenTo (x-(x-y)) (y-(x-y)) z

--3)
junta :: [a]->[a]->[a]
junta [] l = l
junta l [] = l
junta x y = x ++ y

--4)
procura :: [a] -> Int -> a
procura (h:t) 0 = h
procura (h:t) x = procura t (x-1)

--5)
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]

--6)
mytake :: Int -> [a] -> [a]
mytake 0 l = []
mytake x (h:t) = h : mytake (x-1) t

--7)
mydrop :: Int -> [a] -> [a]
mydrop 0 l = l
mydrop x (h:t) = mydrop (x-1) t

--8)
myzip :: [a] -> [b] -> [(a,b)]
myzip l [] = []
myzip [] l = []
myzip (h:t) (x:y) = (h,x) : myzip t y

--9)
myelem :: Eq a => a -> [a] -> Bool
myelem x [] = False
myelem x (h:t) = if(x==h) then True
                          else myelem x t

--10)
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate x y = y : myreplicate (x-1) y

--11)
myintersperse :: a -> [a] -> [a]
myintersperse a [x] = [x]
myintersperse a (h:t) = h : a : myintersperse a t

--12)
mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup (h:t) = (fazgroup h (h:t)) : mygroup (pegaresto h (h:t))

fazgroup :: Eq a => a -> [a] -> [a]
fazgroup _ [] = []
fazgroup x (h:t) = if (x==h) then h : fazgroup x t
                             else []

pegaresto :: Eq a => a -> [a] -> [a]
pegaresto _ [] =[]
pegaresto x (h:t) = if (x==h) then pegaresto x t
                              else (h:t)

--13)
myconcat :: [[a]] -> [a]
myconcat [x] = x
myconcat (h:t) = h ++ myconcat t

--14)
myinits :: [a] -> [[a]]
myinits [] = [[]]
myinits l = myinits (init l) ++ [l]                                                                             

--15)
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = [l] ++ mytails (tail l)

--16)
myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _ = True
myisPrefixOf _ [] = False
myisPrefixOf (h:t) (x:y) = if h == x then True && myisPrefixOf t y
                                            else False

--17)
myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _ = True
myisSuffixOf _ [] = False
myisSuffixOf (h:t) (x:y) = if h == (head y) then True && myisSuffixOf t y
                                            else False

--18)
myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] l = False
myisSubsequenceOf l [] = True
myisSubsequenceOf (h:t) (x:y) = if (h==x) then myisSubsequenceOf t y
                                          else myisSubsequenceOf (h:t) y

--19)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x l = if (x == last l) then (elemIndices x (init l)) ++ [(length l) -1]
                                   else elemIndices x (init l)

--20)
nub :: Eq a => [a] -> [a]
nub [] = []
nub [x] = [x]
nub (h:t) = h : nub (remover h (h:t))

remover :: Eq a => a -> [a] -> [a]
remover x [] = []
remover x (h:t) = if (x==h) then remover x t 
                            else h : remover x t

--21)
mydelete :: Eq a => a -> [a] -> [a]
mydelete _ [] = []
mydelete x (h:t) = if (x==h) then t
                             else h : (mydelete x t)

--22)
removePrimeiros :: Eq a => [a] -> [a] -> [a]
removePrimeiros l [] = l
removePrimeiros (h:t) (x:y) = if (h==x) then removePrimeiros t y
                                        else h : removePrimeiros t (x:y)

--23)
myunion :: Eq a => [a] -> [a] -> [a]
myunion l [] = l
myunion [] l = []
myunion l (h:t) = if (elem h l) then myunion l t
                                else myunion (l ++ [h]) t

--24)
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect l [] = []
myintersect [] l = []
myintersect (h:t) (x:y) = if (elem x (h:t)) then x : myintersect t (x:y)
                                           else myintersect (h:t) y

--25)
myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert x (h:t) | (x <= h) = x:(h:t)
                 | otherwise = h : myinsert x t

--26)
myunwords :: [String] -> String
myunwords [x] = x
myunwords (h:t) = h ++ " " ++ myunwords t

--27)
myunlines :: [String] -> String
myunlines [] = [] 
myunlines (h:t) = h ++ "\n" ++ myunlines t

--28)
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = listapaInt(elemIndices (maxLista (h:t)) (h:t))

maxLista :: Ord a => [a] -> a
maxLista [x] = x
maxLista (x:y) = max x (maxLista y)

listapaInt :: [Int] -> Int
listapaInt [x] = x

--29)
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [x] = False
temRepetidos (h:t) = (elem h t) || temRepetidos t

--30)           
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) = if (ord '0' <= ord h && ord h <= ord '9') then h : algarismos t
                                                             else algarismos t

--31)
posImpares :: [a] -> [a]
posImpares [x] = [x]
posImpares (h:t) = if ((mod (length (h:t)) 2) == 0) then posImpares t
                                                    else h : posImpares t

--32)
posPares :: [a] -> [a]
posPares [x] = [x]
posPares (h:t) = if ((mod (length (h:t)) 2) == 0) then h : posPares t
                                                    else posPares t 

--33)
isSorted :: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (h:t:r) | h<=t = isSorted (t:r)
                 | otherwise = False

--34)
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = myinsert h (iSort t)

--35)
menor :: String -> String -> Bool
menor [] l = True
menor l [] = False
menor (h:t) (x:y) | h < x = True && menor t y
                  | h == x = menor t y
                  | otherwise = False

--36)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet a [] = False
elemMSet x ((h,t):r) = (x==h) || elemMSet x r

--37)
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((h,t):r) = t + lengthMSet r

--38)
converteMSet :: [(a,Int)] -> [a]
converteMSet [(x,0)] = []
converteMSet ((h,t):r) | t>0 = h : converteMSet ((h,t-1):r)
                       | otherwise = converteMSet r

--39)
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet x ((h,t):r) = if (x==h) then ((h,t+1):r)
                                   else (h,t) : insereMSet x r

--40)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet a [(b,1)] = []
removeMSet a [] = []
removeMSet x ((h,t):r) = if (x==h) then ((h,t-1):r)
                                   else (h,t) : removeMSet x r

--41)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = (h, contaChar h (h:t)) : constroiMSet (elemina h t)

contaChar :: Eq a => a -> [a] -> Int
contaChar a [] = 0
contaChar x (h:t) | x==h = 1+contaChar x t
                  | otherwise = 0 + contaChar x t

elemina :: Eq a => a -> [a] -> [a]
elemina a [] = []
elemina x (h:t) = if (x == h) then elemina x t
                              else h : elemina x t

--42)
mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers [] = ([],[])
mypartitionEithers l = (left l, right l)

left :: [Either a b] -> [a]
left [] = []
left (Left a : t) = a : left t                                                                                                                         
left (Right b : t) = left t

right :: [Either a b] -> [b]
right [] = []
right (Left a : t) = right t                                                                                                                        
right (Right b : t) = b : right t

--43)
mycatMaybes :: [Maybe a] -> [a]
mycatMaybes [] = []
mycatMaybes (Just x : t) = x : mycatMaybes t
mycatMaybes (Nothing : t) = mycatMaybes t

--44)
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte : t) = posicao (x,y+1) t
posicao (x,y) (Este : t) = posicao (x-1,y) t
posicao (x,y) (Oeste : t) = posicao (x+1,y) t
posicao (x,y) (Sul : t) = posicao (x,y-1) t

{-  CASO ESTIVESSE "deriving (Show,Eq)", poderia se fazer desta forma!

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) | h == Norte = posicao (x,y+1) t
                    | h == Este = posicao (x-1,y) t
                    | h == Oeste = posicao (x+1,y) t
                    | h == Sul = posicao (x,y-1) t
-}

--45)
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | (x2 - x1) == 0 && (y2-y1) == 0 = []
                        | (x2 - x1) < 0 = Este : caminho (x1-1,y1) (x2,y2)
                        | (x2 - x1) > 0 = Oeste : caminho (x1+1,y1) (x2,y2)
                        | (x2 - x1) == 0 && (y2-y1) < 0 = Sul : caminho (x1,y1-1) (x2,y2)
                        | (x2 - x1) == 0 && (y2-y1) > 0 = Norte : caminho (x1,y1+1) (x2,y2)

--46)
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte : t) = True && vertical t
vertical (Sul : t) = True && vertical t                        
vertical (Oeste : t) = False && vertical t
vertical (Este : t) = False && vertical t

--47)
data Posicao = Pos Int Int
              deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [(Pos x y)] = (Pos x y)
maisCentral ((Pos x y):(Pos w z):r) | (x^2 + y^2) < (w^2 + z^2) = maisCentral ((Pos x y) : r)
                                    | otherwise = maisCentral ((Pos w z) : r)


--48)
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos a b) [] = []
vizinhos (Pos x y) ((Pos x1 y1) : t) | (-1<= (x-x1) && (x-x1)<=1) && (-1<= (y-y1) && (y-y1)<=1) = (Pos x1 y1) : vizinhos (Pos x y) t
                                     | otherwise = vizinhos (Pos x y) t

--49)
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada ((Pos x y):(Pos w z):r) = (y==z) && mesmaOrdenada ((Pos w z):r)

--50)
data Semaforo = Verde | Amarelo | Vermelho
              deriving Show

interseccaoOk :: [Semaforo] -> Bool
interseccaoOk [] = False
interseccaoOk [x] = True
interseccaoOk l = (contaVermelho l 2)

contaVermelho :: [Semaforo] -> Int -> Bool
contaVermelho [] 0 = False
contaVermelho [] n = True
contaVermelho (Vermelho : t) n = contaVermelho t (n-1)
contaVermelho (Verde : t) n = contaVermelho t n
contaVermelho (Amarelo : t) n = contaVermelho t n                                             
