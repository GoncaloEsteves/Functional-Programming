{-| 1. Apresente uma definição recursiva das seguintes funções sobre listas:

      (a) nub :: Eq a => [a] -> [a] que calcula uma lista com os mesmos elementos da recebida, sem repetições.
      Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3].-}

nub2 :: Eq a => [a] -> [a]
nub2 x = aux [] x

aux :: Eq a => [a] -> [a] -> [a]
aux a [] = a
aux x (y:ys) = if (elem y x) == True then (aux x ys)
               else (aux (x++[y]) ys) 


   {-|(b) zipWith :: (a->b->c) -> [a] -> [b] -> [c] que combina os elementos de duas listas usando uma
      função especı́fica. Por exemplo, zipWith (+) [1,2,3,4,5] [10,20,30,40] corresponde a [11,22,33,44].-}

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 f [] _ = []
zipWith2 f _ [] = []
zipWith2 f (x:xs) (y:ys) = (f x y):(zipWith2 f xs ys)

-- 2. Considere o tipo MSet a para representar multi-conjuntos de elementos de a

type MSet a = [(a,Int)]

{-|Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja segunda componente
seja menor ou igual a zero.
      
      (a) Defina a função converte :: Eq a => [a] -> MSet a que converte uma lista para um multi-conjunto.
      Por exemplo, converte "bacaba" corresponde a [(’b’,2),(’a’,3),(’c’,1)]-}

converte :: Eq a => [a] -> MSet a
converte [] = []
converte (x:xs) = (converte2 x (converte xs))

converte2 :: Eq a => a -> MSet a -> MSet a
converte2 x [] = [(x,1)]
converte2 x ((m,i):ms) = if x == m then ((m,i+1):ms)
                         else (m,i):(converte2 x ms)   

   {-|(b) Defina a função intersect :: Eq a => MSet a -> MSet a ->MSet a que calcula a intersecção de dois
      multi-conjuntos. Por exemplo, intersect [(’a’,3),(’b’,5),(’c’,1)] [(’d’,5),(’b’,2)] corresponde a [(’b’,2)]-}  

intersect2 :: Eq a => MSet a -> MSet a -> MSet a
intersect2 [] a = []
intersect2 a [] = []
intersect2 (a:as) (b:bs) = (aux2 a (b:bs)) ++ (intersect2 as (b:bs))

aux2 :: Eq a => (a,Int) -> MSet a -> MSet a
aux2 (a,b) [] = []
aux2 (a,b) ((m,i):ms) = if a==m then [(m,e)] 
	                    else aux2 (a,b) ms

	                    where e = if b>=i then i
	                    	      else b

-- 3. Considere o seguinte tipo para representar expressões proposicionais:

data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop

p1 :: Prop
p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))

    {-(a) Declare Prop como instância da classe Show, de forma a que a expressão p1 seja apresentada da seguinte
      forma: -((-A /\ B) \/ C)-}

instance Show Prop where
	 
	show (Var x) = x
	show (Not p) = "(" ++ "-" ++ (show p) ++ ")"
	show (And x y) = "(" ++ (show x) ++ " and " ++ (show y) ++ ")"
	show (Or x y) = "(" ++ (show x) ++ " or " ++ (show y) ++ ")"

	{-(b) Defina a função eval :: [(String,Bool)] -> Prop -> Bool, que dada uma valoração (i.e., uma cor-
      respondência entre variáveis proposicionais e valores booleanos) calcula o valor lógico de uma expressão
      proposicional.-}