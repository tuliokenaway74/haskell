{------------------------------------------ 
Prova 02 Programação Funcinonal 06/10/2020
-------------------------------------------
Nome completo 01: Isabelle Sgrignero
Matricula 01: 2019.1.08.030

Nome completo 02: Thiago Oliveira da Silva
Matrícula 02: 2019.1.08.028
-------------------------------------------}

import Data.Char
import Data.List

{--Questão 1 ------------------------------}

type Brinde = (String, Int, Int)

-- Item 1.a
mapa cod
    | cod == 01   = ("Natal", 21, 34)
    | cod == 02   = ("Bertioga", 17, 65)
    | cod == 03   = ("Rio de Janeiro", 9, 10)
    | cod == 04   = ("Curitiba", 3, 54)
    | cod == 05   = ("Petrolina", 2, 09)
    | cod == 06   = ("Salvador", 0, 01)
    | cod == 07   = ("Teresina", 21, 56)
    | otherwise   = ("Destino inexistente", 0, 0)

-- Item 1.b
cidade :: Brinde -> String
cidade (a,_,_) = a

nPassagens :: Brinde -> Int
nPassagens (_,b,_) = b

nHospedagens :: Brinde -> Int
nHospedagens (_,_,c) = c

-- Item 1.c
totalPassagem :: Int -> Int
totalPassagem x
    |x == 1     = nPassagens (mapa 1)
    |otherwise  = nPassagens (mapa x) + totalPassagem(x-1)

-- Item 1.d
totalHospedagem :: Int -> Int
totalHospedagem x
    |x == 1     = nHospedagens (mapa 1)
    |otherwise  = nHospedagens (mapa x) + totalHospedagem(x-1)

-- Item 1.f
verificaDisp :: Brinde -> Int -> Bool
verificaDisp (a,b,c) x
    | ((a == cidade(mapa x)) && (b <= nPassagens(mapa x)) && (c <= nHospedagens(mapa x))) = True
    | (x == 0) = False
    | otherwise = verificaDisp (a,b,c) (x-1)

funcQ1f :: Brinde -> Bool
funcQ1f (a,b,c) = verificaDisp (a,b,c) 7

{--Questão 2 ------------------------------

-- Item 2.a
A função "proximo" compara as duas primeiras tuplas da lista e retorna a com menor int (independente do char que o acompanhe), esse processo é repetido até que reste apenas uma tupla na lista (que será o resultado).

Exemplo:
Main> proximo [('x',5),('b',2),('n',0)]
('n',0)

-- Item 2.b
A função "realizado" remove tuplas com int duplicado, independente do char que o acompanhe.

Exemplo:
Main> realizado ('z', 60) [('f', 50),('s', 60)]
[('f', 50)]
Main> realizado ('p', 10) [('w', 10)]
[]

-- Item 2.c
A função "f01" ordena a entrada char das tuplas utilizando das entradas int para isso, as entradas int são organizadas de forma crescente.

Exemplo:
Main> f01 [('l',7),('h',1),('s',3),('a',2),('k',4),('l',6),('e',5)]
"haskell"

-- Item 2.d
Já existe a condição de lista vazia na função "f01" antes de chamar a função "proximo".

-- Item 2.e
t: (Char,Int)
a: Char
b: Int
c: Char
x: (Char,Int)

-------------------------------------------}

{--Questão 3 ------------------------------}

funcQ3 :: [[Int]] -> String -> [String]
funcQ3 _ [] = []
funcQ3 x (a:b) = transformacao x (numero a letras) ++ funcQ3 x b

letras :: [(Char,Int)]
letras = zip ['a'..'z'][0..25]

transformacao :: [[Int]] -> Int -> [String]
transformacao x y = transformaLista (filtraLista x y)

filtraLista :: [[Int]] -> Int -> [[Int]]
filtraLista [] _ = []
filtraLista (a:b) y 
    | filtra a y == True = (a):(filtraLista b y)
    | otherwise          = filtraLista b y

filtra :: [Int] -> Int -> Bool
filtra [] _ = False
filtra (a:b) y
    | a == y    = True
    | otherwise = filtra b y

transformaLista :: [[Int]] -> [String]
transformaLista x = [transforma a | a <- x]

transforma :: [Int] -> String
transforma i = [letra a letras | a <- i]

letra :: Int -> [(Char,Int)] -> Char
letra x ((a,b):s)
    | x == b    = a
    | otherwise = letra x s

numero :: Char -> [(Char,Int)] -> Int
numero x ((a,b):s)
    | x == a    = b
    | otherwise = numero x s

{--Questão 4 ------------------------------}

funcQ4 :: [String] -> [(Int, String)]
funcQ4 (x) = [cont a | a <- x]

cont :: String -> (Int,String)
cont a = (length a, a)

{--Questão 5 ------------------------------}

{--[1,2,3]--[1,3]--}

posicao :: [Int] -> Int -> [Int]
posicao [] _ = []
posicao (a:b) x 
    | mod x 2 == 0 = a:posicao b (x+1)
    | otherwise    = posicao b (x+1)

funcQ5 :: ([Int],Bool) -> [Int]
funcQ5 ((a,b)) 
    | b == False = (posicao a 0) 
    | otherwise  = a 

{--Questão 6 ------------------------------}

funcQ6 :: [Int] -> [Int]
funcQ6 [] = []
funcQ6 [x] = [x]
funcQ6 (a:b:c)
    | mod a 2 == 1 && mod b 2 == 0 = a:(funcQ6 c)
    | otherwise                    = a:funcQ6 (b:c)

{--Questão 7 ------------------------------}

retornaPosicao :: [Int] -> [Int] -> Int -> [Int]
retornaPosicao _ [] _ = []
retornaPosicao (a:b) (c:d) x
    | a /= c    = x:retornaPosicao b (c:d) (x+1)
    | otherwise = retornaPosicao b d (x+1)

funcQ7 :: [Int] -> [Int] -> ([Int], [Int])
funcQ7 x y = (x, retornaPosicao x y 0)

{--Questão 8 ------------------------------}

{--Questão 9 ------------------------------}

conte :: String -> Int
conte palavra = length $ filter isDigit palavra

{--Questão 10 -----------------------------}

funcQ10 :: [Int] -> Int -> Bool
funcQ10 a b = (posicaoL 0 a b)

posicaoL :: Int -> [Int] -> Int -> Bool
posicaoL i [] x = False
posicaoL i (a:b) x
    | a == x && x == i = True
    | otherwise        = posicaoL (i+1) b x

{--Questão 11 -----------------------------}

somaElem :: [(Int,Int)] -> Int -> [Bool] 
somaElem [] t = []
somaElem ((x,a):u) t
    | x + a > t = (True) :somaElem (u) t
    | otherwise = (False):somaElem (u) t

{--Questão 12 -----------------------------}

funcQ12 :: Int -> Int -> ([Int],[Int])
funcQ12 x y = (multiplos x (geraLista x y),naoMultiplos x (geraLista x y))

multiplos :: Int -> [Int] -> [Int]
multiplos x [] = []
multiplos x (a:b)
    | (mod a x) == 0 = a:multiplos x b
    | otherwise      = multiplos x b

naoMultiplos :: Int -> [Int] -> [Int]
naoMultiplos x [] = []
naoMultiplos x (a:b)
    | (mod a x) == 0 = naoMultiplos x b
    | otherwise      = a:naoMultiplos x b

geraLista :: Int -> Int -> [Int]
geraLista a b
    |a == b    = [a]
    |otherwise = a:geraLista (a+1) b

{--Questão 13 -----------------------------}

funcQ13 :: String -> String
funcQ13 [] = []
funcQ13 a  = sup a (length a)    

sup :: String -> Int -> String
sup _ 0 = []
sup x y = x ++ sup x (y-1)

{--Questão 14 -----------------------------

-- Item 14.a
Uma função de ordem superior é uma função que leva uma ou mais funções como argumentos, ou seja, parâmetros procedimentais, e também retorna uma função como seu resultado.

-- Item 14.b
Facilita o entendimento das funções, como também modificações (mudança na função de transformação), aumenta o reuso de definições/código e modularidade.

-- Item 14.c
A avaliação dos argumentos da função é atrasada o máximo possível, eles não são avaliados até que seja realmente necessário fazê-lo. Então, quando alguma expressão é fornecida como um
argumento para uma função, ela é simplesmente empacotada como uma expressão não avaliada sem fazer nenhum trabalho real. Por exemplo, ao avaliar f 5 (29^35792), o segundo argumento será
simplesmente empacotado em uma conversão sem fazer nenhum cálculo real e f será chamado imediatamente. Como f nunca usa seu segundo argumento, a conversão será simplesmente descartada 
pelo coletor de lixo.

-------------------------------------------}

{--Questão 15 -----------------------------}

{--Questão 16 -----------------------------}

funcQ16 :: [(Int,String)] -> String
funcQ16 [] = []
funcQ16 ((x,y):b) = (posicaoChar 0 x y):funcQ16 b

posicaoChar :: Int -> Int -> String -> Char
posicaoChar x y [] = ' '
posicaoChar x y (a:b)
    |x == y    = a
    |otherwise = posicaoChar (x+1) y b

{--Questão 17 -----------------------------}

naturais :: [Int]
naturais = [0,1..]

{--Questão 18 -----------------------------}

{--Questão 19 -----------------------------}

funcQ19 :: [Int] -> ([Int],[Int],[Int])
funcQ19 x = (divDois x,divTres x,resto x)

divDois :: [Int] -> [Int]
divDois [] = []
divDois (a:b)
    | (mod a 2) == 0 = a:divDois b
    | otherwise      = divDois b

divTres::[Int]->[Int]
divTres [] = []
divTres (a:b)
    | (mod a 3) == 0 = a:divTres b
    | otherwise      = divTres b

resto :: [Int] -> [Int]
resto [] = []
resto (a:b)
    | (mod a 3) == 0 = resto b
    | (mod a 2) == 0 = resto b
    | otherwise      = a:resto b

{--Questão 20 -----------------------------}

listaMaiores :: [[Int]] -> [Int]
listaMaiores [] = []
listaMaiores (x:xs)  = maior x:listaMaiores xs

maior :: [Int] -> Int
maior [a] = a
maior (a:x:resto)
    |a > x     = maior (a:resto)
    |otherwise = maior (x:resto)

{--Questão 21 -----------------------------}

boll:: Char -> [Char] -> [Bool]
boll x [] = []
boll x (y:yx)
    | x == y    = True :boll x yx
    | otherwise = False:boll x yx

{--Questão 22 -----------------------------}

funcQ22 :: [Int] -> Int -> [(Int,Int)]
funcQ22 x a = [(b,a) | b <- x, b > a]

verificaElem :: Int -> [Int] -> [Int]
verificaElem _ [] = []
verificaElem m (b:x)
  |m == b = x
  |otherwise = b:(verificaElem m x)

{--Questão 23 -----------------------------}

funcQ23 :: [Int] -> [Int]
funcQ23 [] = []
funcQ23 (a:b)
    | rep a b = a:funcQ23 (removeRep a b)
    | otherwise   = funcQ23 b

rep :: Int -> [Int] -> Bool
rep x [] = False
rep x (a:b)
    | x == a    = True
    | otherwise = rep x b

removeRep :: Int -> [Int] -> [Int]
removeRep _ [] = []
removeRep m (b:x)
    |m == b    = removeRep m x
    |otherwise = b:(removeRep m x)

{--Questão 24 -----------------------------}

funcQ24 :: (Int -> [Int]) -> [Int] -> [[Int]]
funcQ24 f lista = [f x | x <- lista]

terminaEm :: Int -> [Int]
terminaEm x
    | x < 101   = x:terminaEm (x+10)
    | otherwise = []

{--Questão 25 -----------------------------}

-- Item 25.a
(&&&) :: Int -> (Int,Int) -> Int
infixl 7 &&&
a &&& b = funcQ25a a b

funcQ25a :: Int -> (Int,Int) -> Int
funcQ25a x y = aproxima x x y

aproxima :: Int -> Int -> (Int,Int) -> Int
aproxima x y (a,b)
    | x == a    = a
    | x == b    = b
    | y == a    = a
    | y == b    = b
    | otherwise = aproxima (x+1) (y-1) (a,b)

-- Item 25.b
funcQ25b :: (Int -> (Int,Int) -> Int) -> Int -> [(Int, Int)]-> [Int]
funcQ25b f x s = [f x a | a <- s]

{--Item 25.c -----------------------------
Se a entrada for:
    funcQ25b (&&&) 7 [(9,0),(6,8),(80,2)]
    ela retornará:
    [9,8,2]

    A função funcQ25b chama a função (&&&) e aplica a condição em cada dupla da minha lista. Ex:
    funcQ25 (&&&) 7 (9,0)  | (9,0)  <- [(9,0),(6,8),(80,2)]
    [9]
    funcQ25 (&&&) 7 (6,8)  | (6,8)  <- [(6,8),(80,2)]
    [8]
    funcQ25 (&&&) 7 (80,2) | (80,2) <- [(80,2)]
    [2]
    
    Ao final retornando uma lista com o resultado de todas as verificações:
    [9,8,2]
-------------------------------------------}

{--Questão 26 -----------------------------}

mescla :: String -> String -> [(Char,Int)]
mescla s f = [(a, charRep f a 0) | a <- excluiRep s]

excluiRep :: String -> String
excluiRep "" = ""
excluiRep (a:b)
    | charRep (a:b) a 0 > 1 = excluiRep b
    | otherwise                         = a:excluiRep b

charRep :: String -> Char -> Int -> Int
charRep [] _ y = y
charRep (a:b) x y
    | a == x    = charRep b x (y+1)
    | otherwise = charRep b x y

{--Questão 27 -----------------------------}

infix 3 -*-
(-*-) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(a,b) -*- (c,d) =  (max (max a b)(max c d), min (min a b)(min c d))

{--Questão 28 -----------------------------}

-- Item 28.a
ocorrencia :: Int -> [Int] -> (Int, Int)
ocorrencia x a = (x, contaRepetidos a x 0)

contaRepetidos :: [Int] -> Int -> Int -> Int
contaRepetidos [] _ y = y
contaRepetidos (a:b) x y
    | a == x    = contaRepetidos b x (y+1)
    | otherwise = contaRepetidos b x y

-- Item 28.b
aplica :: (Int -> [Int] -> (Int,Int)) -> [Int] -> [(Int, Int)]
aplica f k = [f  a k | a <- k]

{--Item 28.c -----------------------------
    A entrada deve ser:
    aplica ocorrencia [3,4,5,2,3,3,2,1,3,5,6]

    A função "aplica" irá verificar todos os elementos da lista retornando a "ocorrencia" de cada um deles. Ou seja, uma dupla (x, quantidade de x na lista)
para todos os elemento existente na lista. O retorno será:
    [(3,4),(4,1),(5,2),(2,2),(3,4),(3,4),(2,2),(1,1),(3,4),(5,2),(6,1)]
-------------------------------------------}

{--Questão 29 -----------------------------}

funny :: Int -> Int -> Int -> Bool
funny  x y z
    | (x > z && y <= z) = True
    | otherwise         = False

{--Questão 30 -----------------------------}

excluir :: Int->[Int]->[Int]
excluir _ [] = []
excluir x (a:b)
    | a == x        = b
    | otherwise     = a:excluir x b