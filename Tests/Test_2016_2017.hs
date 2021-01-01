--1. Considere o tipo MSet a para representar multi-conjuntos de tipo a

type MSet a = [(a,Int)]

{-|Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja segunda
componente seja menor ou igual a zero. Para além disso, os multi-conjuntos estão organizados
por ordem decrescente da muiltplicidade. O multi-conjunto {’b’,’a’,’c’,’b’,’b’,’a’,’b’} é
representado pela lista [(’b’,4),(’a’,2),(’c’,1)], por exemplo.

     (a) Defina a função cardMSet :: MSet a -> Int que calcula a cardinalidade de um multi-
     conjunto. Por exemplo, cardMSet [(’b’,4),(’a’,2),(’c’,1)] devolve 7.-}

cardMSet' :: MSet a -> Int
cardMSet' [] = 0
cardMSet' ((m,i):ms) = i + (cardMSet' ms)

  {-|(b) Defina a função moda :: MSet a -> [a] que devolve a lista dos elementos com maior número
     de ocorrências.-}

moda :: MSet a -> [a]
moda [] = []
moda [(m,i),(s,e)] = if i==e then [m,s]
	                 else [m]
moda ((m,i):(s,e):ms) = if i==e then m:(moda ((s,e):ms))
	                    else [m]

  {-|(c) Defina a função converteMSet :: MSet a -> [a] que converte um multi-conjunto numa
     lista. Por exemplo, converteMSet [(’b’,4),(’a’,2),(’c’,1)] devolve ‘‘bbbbaac’’.-}

converteMSet' :: MSet a -> [a]
converteMSet' [] = []
converteMSet' ((m,i):ms) | i == 1 = m:(converteMSet' ms)
                         | otherwise = m:(converteMSet' ((m,(i-1)):ms))

  {-|(d) Defina a função addNcopies :: Eq a => MSet a -> a -> Int -> MSet a que faz a inserção
     de um dado número de ocorrências de um elemento no multi-conjunto, mantendo a ordenação
     por ordem decrescente da multiplicidade. Não use uma função de ordenação.-}

addNcopies' :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies' [] m i = [(m,i)]
addNcopies' ((x,e):xs) m i | i >= e = ((m,i):(x,e):xs)
                           | otherwise = (x,e):(addNcopies' xs m i)

--2. Considere o seguinte tipo de dados para representar subconjuntos de números reais (Doubles).

data SReais = AA Double Double 
            | FF Double Double
            | AF Double Double 
            | FA Double Double
            | Uniao SReais SReais

{-|(AA x y) representa o intervalo aberto ]x, y[, (FF x y) representa o intervalo fechado [x, y],
   (AF x y) representa ]x, y], (FA x y) representa [x, y[ e (Uniao a b) a união de conjuntos.

     (a) Defina a SReais como instância da classe Show, de forma a que, por exemplo, a apresentação
     do termo Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0) seja
     ((]4.2,5.5[ U ]3.1,7.0]) U [-12.3,30.0])-}

instance Show SReais where

    show (AA x y) = "]" ++ show x ++ ";" ++ show y ++ "["
    show (FF x y) = "[" ++ show x ++ ";" ++ show y ++ "]"
    show (AF x y) = "]" ++ show x ++ ";" ++ show y ++ "]"
    show (FA x y) = "[" ++ show x ++ ";" ++ show y ++ "["
    show (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

   --(b) Defina a função pertence :: Double-> SReais -> Bool que testa se um elemento pertence a um conjunto.

pertence :: Double -> SReais -> Bool 
pertence a (AA x y) | a > x && a < y  = True
                    | otherwise = False

pertence a (FF x y) | a >= x  && a <= y  = True
                    | otherwise = False

pertence a (AF x y) | a > x  && a <= y  = True
                    | otherwise = False

pertence a (FA x y) | a >= x  || a > y  = True
                    | otherwise = False

pertence a (Uniao x y) | (pertence a x) == True || (pertence a y) == True = True
                       | otherwise = False

   --(c) Defina a função tira :: Double -> SReais -> SReais que retira um elemento de um conjunto

tira :: Double -> SReais -> SReais
tira a (AA x y) | a > x && a < y  = Uniao (AA x a) (AA a y)
                | otherwise = (AA x y)

tira a (FF x y) | a >= x  && a <= y  = Uniao (FA x a) (AF a y)
                | otherwise = (FF x y)

tira a (AF x y) | a > x  && a <= y  = Uniao (AA x a) (AF a y)
                | otherwise = (AF x y)

tira a (FA x y) | a >= x  || a > y  = Uniao (FA x a) (AA a y)
                | otherwise = (FA x y)

tira a (Uniao x y) = Uniao (tira a x) (tira a y)

--3. Considere o seguinte tipo para representar árvores irregulares (rose trees).

data RTree a = R a [RTree a]

   {-|(a) Defina a função percorre :: [Int] -> RTree a -> Maybe [a] que recebe um caminho e
      uma árvore e dá a lista de valores por onde esse caminho passa. Se o caminho não for válido
      a função deve retornar Nothing. O caminho é representado por uma lista de inteiros (1 indica
      seguir pela primeira sub-árvore, 2 pela segunda, etc)-}

percorre :: [Int] -> RTree a -> Maybe [a]
percorre i r = percorre2 i r [] 

percorre2 :: [Int] -> RTree a -> [a] -> Maybe [a]
percorre2 [] (R a x) i = Just (i ++ [a])
percorre2 a (R x []) i = Nothing
percorre2 (i:is) (R x (r:rs)) a | i == 1 = percorre2 is r (a ++ [x])
                                | otherwise = percorre2 ((i-1):is) (R x rs) a

   {-|(b) Defina a função procura :: Eq a => a -> RTree a -> Maybe [Int] que procura um ele-
      mento numa árvore e, em caso de sucesso, calcula o caminho correspondente.-}

--procura :: Eq a => a -> RTree a -> Maybe [Int]