{- | Chegando à Tarefa 3, esta foi uma tarefa com pouca ou nenhuma relevância no resultado final do projeto. O objectivo 
desta tarefa é, dado um labirinto válido, convertê-lo numa sequência de instruções de modo a recriá-lo num formato mais 
compacto para leitura. Assim, decidimos usar funções com recursividade e várias funções com acumulador. A recursividade 
foi importante para aplicar um conjunto de regras a um corredor e assim fazer isso a todos os corredores do Labirinto
o que até foi bastante lógico pelo que não existiram grandes dificuldades a realizar esta Tarefa. As funções com acumuladores
foram importantes para verificar os padrões verticais que existiam no Labirinto ou seja, se existiam corredores iguais
uns aos outros.
-}

module Tarefa3 where

import Tarefa1
import Types


{- | The purpose of the function compactCorridor is to transform a corridor
in an Instruction type making horizontal patterns, that is, if you have
identical and consequential pieces the function joins them into a single Tuple,
checking this through its auxiliary function with a accumulator. -}
compactCorridor :: Corridor -> Instruction
compactCorridor [] = Instruct []
compactCorridor (x:xs) = Instruct (aux 1 x xs)
 where
   aux i a [] = [(i,a)]
   aux i a (h:t) = if a == h then aux (i+1) h t else (i,a) : aux 1 h t


{- | A função compactMaze realiza um processo muito semelhante
à compactCorridor mas faz isso para Labirintos completos utilizando
a função compactCorridor como base. -}
compactMaze :: Maze -> Instructions
compactMaze [] = []
compactMaze (x:xs) = compactCorridor x : compactMaze xs


{- | A função posElem funciona como função auxiliar
da função verticalPatterns. -}
posElem :: Instructions -> Int -> Instruction
posElem (x:xs) 0 = x
posElem (x:xs) a = posElem xs (a-1)


{- | Esta função é a base da 3ª parte desta
tarefa, ou seja, a parte dos padrões verticais.
Assim, a função verticalPatterns tem como objetivo
verificar se, para uma lista restringida a começar por
um elemento numa certa posição, tem corredores iguais
a esse, se sim, então faz a respetiva substituição da Instruction. -}
verticalPatterns:: Int -> Instructions -> Instructions
verticalPatterns i [] = []
verticalPatterns i [x] = [x]
verticalPatterns i l@(x:y:ys) = if (posElem l i) == posElem l (i+1) then (take (i+1) l) ++ (verticalPatterns2 i (drop i l))
                                else (take (i+1) l) ++ (verticalPatterns2 i (drop i l))


{- | A função verticalPatterns2 é uma função auxiliar da função
verticalPatterns1, que tem como finalidade a mesma da função
acima definida. -}
verticalPatterns2 :: Int -> Instructions -> Instructions
verticalPatterns2 i [x,y] = if x == y then [Repeat i] else [y]
verticalPatterns2 i (x:y:ys) = if x == y then (Repeat i) : verticalPatterns2 i (x:ys) else y : verticalPatterns2 i (x:ys)


{- | A função count é a função responsável
por simplificar todo o labirinto utilizando diretamente
as funções verticalPatterns e posElem. -}
count :: Int -> Instructions -> Instructions
count i [] = []
count 0 list = verticalPatterns 0 list
count i list | i >= ((length list)-1) = list
count i list = case (posElem list i) of Instruct x -> count (i+1) (verticalPatterns i list)
                                        Repeat x -> count (i+2) (verticalPatterns i list)


{- | A  função geral é a que organiza e comanda a parte
dos padrões verticais visto que utiliza a função
count para fazer as simplificações necessárias
até chegar à forma mais compacta do Labirinto. -}
geral :: Instructions -> Instructions
geral [] = count 0 []
geral list = count 0 list


{- | Esta é a função principal da Tarefa 3, que utilizando
todas as anteriores, umas de forma mais notável que outras,
concede ao utilizador a forma mais simplificada possivel
de um Labirinto. -}
principal :: Maze -> Instructions
principal [] = []
principal m = geral $ compactMaze m



-- | Testing

printResults3 :: Show a => [a] -> IO()
printResults3 m = mapM_ (\a -> putStrLn ("test3 >>> \n" ++ show a)) m

testCasescompactCorridor :: [Corridor]
testCasescompactCorridor = [[Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Big],
                            [Wall,Wall,Wall,Wall,Wall,Food Little,Food Little,Food Little,Food Big,Food Big]]

testcompactCorridor :: [Corridor] -> [Instruction]
testcompactCorridor [] = []
testcompactCorridor (c:cs) = compactCorridor c : testcompactCorridor cs

-- $ printResults3 $ testcompactCorridor testCasescompactCorridor

testCasescompactMaze :: [(Int, Int, Int)]
testCasescompactMaze = [(15, 10, 1),(41, 34, 22),(30, 33, 22)]

testcompactMaze :: [(Int, Int, Int)] -> [Instructions]
testcompactMaze [] = []
testcompactMaze ((x,y,z):h) = compactMaze (generateMaze x y z) : testcompactMaze h

-- $ printResults3 $ testcompactMaze testCasescompactMaze
