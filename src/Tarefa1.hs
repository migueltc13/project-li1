{- | Descrição do problema

Para a realização desta UC foi-nos proposto a realização do jogo do Pacman. Isto aconteceu à medida em que íamos aprendendo a
linguagem de programação utilizada o que é bom no sentido de ajudar a praticar e a utilizar conceitos que de outra forma não
utilizaríamos. No entanto, isso também leva a uma maior atenção ao trabalho tendo em conta os tempos em que tudo acontece. 
Para tal, este projeto foi dividido em duas fases com três tarefas cada uma. Assim é expectável que se refizessemos o trabalho
a partir de hoje outra vez, usaríamos certamente métodos diferentes de forma a ganhar tempo e a otimizar o código.
 
 Tarefa 1 

O objetivo desta tarefa é implementar um mecanismo de geração de labirintos aleatórios mas com uma série de condições para
sereem labirintos válidos e com dimensões escolhidas pelo utilizador. Esta Tarefa teve como base um ficheiro generator.hs
que continha algumas funções, no entanto incompletas pelo que completamos na aula e com recurso a funções recursivas.
Tendo em conta o contexto da altura em que fizemos esta Tarefa, optámos muitas vezes por utilizar funções não recursivas 
sobre listas. Por isso é expectável que o código seja um pouco exagerado em termos de tamanho e que esteja deveras extenso. 
O objetivo desta Tarefa foi construir labirintos aleatórios. Apesar de não ser utilizada diretamente no resultado 
final do projeto, teve uma grande importância em termos de contextualização e introdução aos estudantes desta linguagem.
-}

module Tarefa1 where

import System.Random
import Types

-- | Given a seed returns a list of n integer randomly generated
genRandoms :: Int -> Int -> [Int]
genRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                    in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-99

-- | Given a seed returns an integer randomly generated
numRandom :: Int -> Int
numRandom seed = head $ genRandoms 1 seed

-- | Converts list into a list of list of size n
subList :: Int -> [a] -> [[a]]
subList _ [] = []
subList n l = take n l : subList n (drop n l)

-- | Converts an integer number into a Piece
convertPiece :: Int -> Piece
convertPiece n | n==3 = Food Big
               | 0<=n && n<70 = Food Little -- [0,70[\3 <=> Food Little
               | otherwise = Wall           -- [70,99]  <=> Wall

-- | Converts a Corridor to a string
printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (c:cs) = show c ++ printCorridor cs

-- | Converts a Maze to a string
printMaze' :: Maze -> String
printMaze' [] = "\n"
printMaze' (c:cs) = printCorridor c ++ printMaze' cs

-- | Converts a list of integers into a Corridor
convertCorridor :: [Int] -> Corridor
convertCorridor [] = []
convertCorridor (p:ps) = convertPiece p : convertCorridor ps

-- | Converts a list of lists of integers into a Maze
convertMaze :: [[Int]] -> Maze
convertMaze [] = []
convertMaze (n:ns) = convertCorridor n : convertMaze ns



-- | Changes the first and last element of a corridor to a Wall piece
changeCorridor :: Corridor -> Corridor
changeCorridor [] = []
changeCorridor (x:xs) = [Wall] ++ take (length(xs)-1) (xs) ++ [Wall]

-- | Changes the first and last element of every Corridor in a inputed Maze to a Wall piece.
changeMaze :: Maze -> Maze
changeMaze [] = []
changeMaze ((x:xs):ys) = [changeCorridor (x:xs)] ++ changeMaze ys

-- | Converts a given maze to a maze surrounded by Wall pieces.
makeWalls :: Maze -> Maze
makeWalls [] = []
makeWalls ((x:xs):ys) = [take (length (x:xs)) (repeat Wall)]
                        ++ changeMaze (take (length (ys)-1) ys)
                        ++ [take (length (x:xs)) (repeat Wall)]



-- | Divides the the height of maze by 2 that returns the middle height of the maze.
middleHeight :: Maze -> Int
middleHeight [] = 0
middleHeight maze = div (length maze) 2

-- | Function that defines length of the middle corridors for even and odd cases.
middleCorridors :: Maze -> [Corridor]
middleCorridors maze | even (length maze) =  take 2 (drop ((middleHeight maze)-1) maze)
                     | otherwise = take 1 (drop (middleHeight maze) maze)

-- | Changes the first and last Wall of a [Corridor] to an Empty piece.
breakWall :: [Corridor] -> [Corridor]
breakWall [] = []
breakWall (c:cs) = ([Empty] ++ (init (tail c)) ++ [Empty]) : breakWall cs

-- | Converts a Maze to a Maze with space for the tunnel.
openTunnel :: Maze -> Maze
openTunnel maze | even (length maze) = (take (midH - 1) maze) ++ tunnel ++ (drop (midH + 1) maze)
                | otherwise = (take midH maze) ++ tunnel ++ (drop (midH + 1) maze)
 where
   midH = middleHeight maze
   tunnel = breakWall $ middleCorridors maze



-- | Sample for the ghost's house even and odd.
ghostHouseEven :: Maze
ghostHouseEven = [(take 10 (repeat Empty)),
                  ([Empty] ++ (take 3 (repeat Wall)) ++ (take 2 (repeat Empty))++ (take 3 (repeat Wall)) ++ [Empty]),
                  ([Empty] ++ [Wall] ++ (take 6 (repeat Empty)) ++ [Wall] ++ [Empty]),
                  ([Empty] ++ (take 8 (repeat Wall)) ++ [Empty]),
                  (take 10 (repeat Empty))]

ghostHouseOdd :: Maze
ghostHouseOdd = [(take 11 (repeat Empty)),
                 ([Empty] ++ (take 3 (repeat Wall)) ++ (take 3 (repeat Empty))++ (take 3 (repeat Wall)) ++ [Empty]),
                 ([Empty] ++ [Wall] ++ (take 7 (repeat Empty)) ++ [Wall] ++ [Empty]),
                 ([Empty] ++ (take 9 (repeat Wall)) ++ [Empty]),
                 (take 11 (repeat Empty))]

-- | Replaces the middle pieces in a corridor given by the second input by the middle pieces of the first input.
auxCorridor :: Corridor -> Corridor -> Corridor
auxCorridor x l = take b l ++ x ++ drop ((length x) + b ) l
 where b = div ((length l)-(length x)) 2

-- | Replaces the middle pieces in all corridores of the second inputed maze.
auxMaze :: Maze -> Maze -> Maze
auxMaze [] l = l
auxMaze (x:xs) (l:ls) = [auxCorridor x l] ++ auxMaze xs ls

-- | insertGhostHouse takes as first input a ghost house and as second a Maze and then places the ghost house in the middle of the maze.
insertGhostHouse :: Maze -> Maze -> Maze
insertGhostHouse [] l = []
insertGhostHouse l [] = []
insertGhostHouse x l = take a l ++ auxMaze x (drop a l)
 where a = div ((length l)-5) 2

-- | Depending on the length (even or odd) of a corridor inserts the ghost house (even or odd).
samplehouse :: Maze -> Maze
samplehouse [] = []
samplehouse l | odd (length (head l)) = insertGhostHouse ghostHouseOdd l
              | otherwise = insertGhostHouse ghostHouseEven l



-- | Generates a random Maze with x columns and y lines.
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s = if x>=15 && y>=10
                     then let random_nrs = genRandoms (x*y) s
                          in samplehouse $ openTunnel $ makeWalls $ convertMaze $ subList x random_nrs
                     else error("The dimension of the maze must be at least 15x10")

-- | Testing
testCasesgenerateMaze :: [(Int, Int, Int)]
testCasesgenerateMaze = [(15, 10, 1),(41, 34, 22),(30, 33, 22)]

testgenerateMaze :: [(Int, Int, Int)] -> [Maze]
testgenerateMaze [] = []
testgenerateMaze ((x, y, z):h) = (generateMaze x y z) : testgenerateMaze h

printResults :: Show a => [a] -> IO()
printResults m = mapM_ (\a -> putStrLn ("\ntest1 >>> " ++ show a)) m

-- $| printResults1 $ testgenerateMaze testCasesgenerateMaze
