{-| 1. Apresente uma definição recursiva das seguintes funções (pré-definidas) sobre listas:

      (a) intersperse :: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em
      que o elemento fornecido é intercalado entre os elementos da lista fornecida.
      Por exemplo, intersperse 1 [10,20,30] corresponde a [10,1,20,1,30].-}

intersperse2 :: a -> [a] -> [a]
intersperse2 x [y] = [y]
intersperse2 x (y:ys) = y:x:(intersperse2 x ys) 

   {-|(b) inits :: [a]->[[a]] que calcula a lista dos prefixos de uma lista.
      Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].-}

inits2 :: [a] -> [[a]] 
inits2 [] = [[]]
inits2 x = (inits2 (aux x)) ++ [x]

aux :: [a] -> [a]
aux [] = []
aux [x] = []
aux (x:xs) = x:(aux xs)

{-| 2. Considere agora que armazenamos a informação sobre uma turma como uma árvore binária de
    procura ordenada por número de aluno, de acordo com a seguinte estrutura de dados:-}

type Aluno = (Numero, Nome, Classificacao)
type Numero = Int 
type Nome = String

data Classificacao = Aprov Int
                   | Rep
                   | Faltou

data Turma = Vazia
           | Nodo Aluno Turma Turma

   {-| Defina as seguintes funções:
      (a) inscNum :: Numero -> Turma -> Bool, que verifica se um aluno, com um dado número, está inscrito.-}

inscNum :: Numero -> Turma -> Bool 
inscNum x Vazia = False
inscNum x (Nodo (a,n,c) t1 t2) | x == a = True
                               | (inscNum x t1) == True || (inscNum x t2) == True = True
                               | otherwise = False

  {-| (b) aprovAv :: Turma -> Float, que calcula o rácio de alunos aprovados por avaliados.
      Implemente esta função fazendo apenas uma travessia da árvore.-}

aprovAv :: Turma -> Float
aprovAv t = divide (aprovAv2 t (0,0))

aprovAv2 :: Turma -> (Int,Int) -> (Int,Int)
aprovAv2 Vazia a = a
aprovAv2 (Nodo (x,y,(Aprov z)) t1 t2) (a,b) = aprovAv2   