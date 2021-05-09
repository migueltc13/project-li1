{- | Passando agora à tarefa 2, esta foi sem dúvida uma das Tarefas cruciais do jogo. O objetivo desta tarefa é, dada uma
descrição do estado do jogo e uma jogada de um dos jogadores, determinar o efeito dessa jogada no estado do jogo. Na primeira
fase do trabalho apenas foi pedido que fizessemos os casos de movimentação do Pacman e só na segunda parte foram incutidas
as condições de movimentação do Pacman.   Esta trata de todos os casos do jogo que na altura não fora nada fácil de testar
devido ao facto de ter sido trabalhoso visto que nesta altura nem o Pacman nem os Ghosts se moviam. No início as dificuldades
saltaram à vista tendo em conta os diferentes tipos que a tarefa integrava. Para esta Tarefa tomá-mos uma abordagem diferente
da tarefa anterior. A base desta Tarefa está sem dúvida alguma nas funções recursivas que ajudaram a tratar de todos os casos.
No entanto, e devido à nossa inexperiência neste setor, vai ser visível um código muito robusto e onde muitas vezes não era
fácil para detetar erros que apareciam posteriormente sendo este um ponto que certamente alteraría-mos.
-}

module Tarefa2 where

import Tarefa1
import Types
import Data.List
import FileUtils

{- | A função play é a função principal da tarefa 2 e é responsável
por todas as condições principais utilizando as outras funções para
a definir. -}

play :: Play -> State -> State
play move@(Move i o) state@(State maze pl l) | (getPacmanMode $ getPacman pl) == Dying = state
                                             | checkcoords state == False = error "Can't place player over a wall"
                                             | checkPacID pl i = movePacman move state
                                             | elem i (getPlayersID pl) = moveGhost move state
                                             | otherwise = state

{- | A função movePacman é responsável por tratar através
de várias funções auxiliares dos casos onde se quer mover
um player Pacman. -}

movePacman :: Play -> State -> State
movePacman move@(Move i o) state@(State maze pl l) =
  if getPlayerOrientation (getPacman pl) /= o
  then movePacMega (State maze (checkTimeMegazero (repSetGet pl o)) l)
  else if checkNextGhost move state
       then movePacMega $ moveNextGhost move state
       else movePacMega $ pieceMove move state

{- | A função moveGhost é responsável por tratar através
de várias funções auxiliares dos casos onde se quer mover
um player Ghost. -}

moveGhost :: Play -> State -> State
moveGhost move@(Move i o) state@(State maze pl l) =
  if (getPlayerOrientation $ getPlayerByID i pl) /= o
  then State maze (checkTimeMegazero ((setPlayerOrientation (getPlayerByID i pl) o):(rmGhostByID i pl))) l
  else if (getpiece (moveNewCoords move state) maze) == Wall
      then state
      else if getPacmanCoords pl == moveNewCoords move state
           then ghostVsPac move state
           else State maze (checkTimeMegazero ((setPlayerCoords (getPlayerByID i pl) (moveNewCoords move state)):(rmGhostByID i pl))) l

{- | A função ghostVsPac é responsável por
dar origem a todos os casos quando há um
confronto entre um Pacman e um GHost. -}

ghostVsPac :: Play -> State -> State
ghostVsPac move@(Move i o) state@(State maze pl l) = case getPacmanMode $ getPacman pl of
  Mega -> megacase move state
  _ -> othercase move state -- Normal and Dying

megacase :: Play -> State -> State
megacase m@(Move i o) s@(State maze pl l) = pacVsGhostDead m s
  -- State maze (checkTimeMegazero $ (addPlayerPoints 10) (switchMouth $ getPacman pl)
  -- :(setGhostsMode Alive $ [setPlayerCoords (getPlayerByID i pl) (getMiddleOfMaze maze)])
  -- ++(rmGhosts pl $ (getPacman pl):[getPlayerByID i pl])) l

othercase :: Play -> State -> State
othercase m@(Move i o) s@(State maze pl l) = pacVsGhostAlive m s
  -- State maze (checkTimeMegazero $ (setPlayerCoords (loseLife $ switchMouth $ getPacman pl) (initialCoordsPac maze))
  -- :[setPlayerCoords (getPlayerByID i pl) (moveNewCoords m s)]
  -- ++(rmGhosts pl ((getPacman pl):[getPlayerByID i pl]))) l

{- | A função piecex é uma função auxiliar da função
getpiece. Esta descobre a peça que está numa determinada
posição num corredor. -}
piecex :: Int -> Corridor -> Piece
piecex _ [] = Empty
piecex n (p:ps) | n==0 = p
                | otherwise = piecex (n-1) ps


{- | A função getpiece é responsável por encontar uma
peça que está numas determinadas coordenadas num
Labirinto. -}
getpiece :: Coords -> Maze -> Piece
getpiece (x,y) ((z:t):ts) | x==0 = piecex y (z:t)
                          | otherwise = getpiece (x-1,y) (take (length ((z:t):ts)-1) ts)


{- | A função getPacmanCoords é responsável por encontrar
de entre uma lista de quaisquer jogadores as coordenadas
do pacman. -}
getPacmanCoords :: [Player] -> Coords
getPacmanCoords pl = getPlayerCoords (getPacman pl)


{- | A função getPlayerlist é responsável por retirar de um
State a sua lista de players. -}
getPlayerlist :: State -> [Player]
getPlayerlist (State _ y _) = y


{- | A função getMaze é responsável por
retirar de um State o seu Maze. -}
getMaze :: State -> Maze
getMaze (State maze y level) = maze


{- | A função replaceMaze é responsável por inserir
um determinado Maze num State. -}
replaceMaze :: State -> Maze -> State
replaceMaze (State m x y) maze = State maze x y


{- | A função replacePiece é responsável por alterar o
Maze de um determinado State para outro (segundo os Movimentos
do Pacman simulando que ele esteja a comer essas Pieces). -}
replacePiece :: State -> Maze
replacePiece x = replaceElemInMaze (getPacmanCoords(getPlayerlist x)) Empty (getMaze x)


{- | A função getPacman é responsável por isolar o Pacman de entre
uma lista de Players. -}
getPacman :: [Player] -> Player
getPacman [x] = x
getPacman (x:xs) = case x of Ghost _ -> getPacman xs
                             Pacman _ -> x


{- | A função rmPacman é responsável por retirar o player Pacman
da lista com todos os players. -}
rmPacman :: [Player] -> [Player]
rmPacman [] = []
rmPacman (x:xs) = case x of Ghost _ -> x : rmPacman xs
                            Pacman _ -> xs


{- | A função setPlayerOrientation é responsável por colocar uma determinada
orientação num Player à escolha. -}
setPlayerOrientation :: Player -> Orientation -> Player
setPlayerOrientation (Pacman (PacState (i,c,v,n,p,l) x y z)) o = (Pacman (PacState (i,c,v,o,p,l) x y z))
setPlayerOrientation (Ghost (GhoState (i,c,v,n,p,l) x)) o = (Ghost (GhoState (i,c,v,o,p,l) x))


-- | The function is responsable for the removal of the timeMega each move made
rmTimeMega :: Player -> Player
rmTimeMega (Pacman (PacState a t c d)) = if t-0.25 <= 0
                                         then (Pacman (PacState a 0 c d))
                                         else (Pacman (PacState a (t-0.25) c d))

-- | If the PacMan Mode is Mega the function rmTimeMega its aplied otherwise its returned the same state
movePacMega :: State -> State
movePacMega state@(State maze pl l) =
  case pacMode of Mega -> State maze ((rmTimeMega pac):(rmPacman pl)) l
                  _ -> state
  where
    pac = getPacman pl
    pacMode = getPacmanMode $ pac

{- | A função pieceMove é responsável por traduzir uma jogada simples,
 ou seja, onde não há interferências de Ghosts. -}
pieceMove :: Play -> State -> State
pieceMove move state@(State maze pl l) = case (checkNextpiece move state) of Food Little -> replaceMaze (foodlittleMove move state) $ replacePiece state
                                                                             Food Big -> replaceMaze (foodBigMove move state) $ replacePiece state
                                                                             Empty -> replaceMaze (emptyMove move state) $ replacePiece state
                                                                             Wall -> switchMouthState state


{- | A função checkPacID é responsável por verificar se um número
é o valor do identificador do pacman. -}
checkPacID :: [Player] -> Int -> Bool
checkPacID l i | getPlayerID (getPacman l) == i = True
               | otherwise = False


{- | A função checkNextGhost é responsável por verificar se existe
algum ghost na posição seguinte à do pacman. É importante para
o caso de Pacman vs Ghost. -}
checkNextGhost :: Play -> State -> Bool
checkNextGhost move state@(State m pl l)
   | findGhosts pl (moveNewCoords move state) == [] = False
   | otherwise = True


{- | A função moveNextGhost é responsável por fazer o
confronto entre o pacman e os ghots através de
diferentes funções definidas anteriormente e postriormente. -}
moveNextGhost :: Play -> State -> State
moveNextGhost move state@(State maze pl l)
  | getpiece (moveNewCoords move state) maze == Food Big = pacVsGhostFB move state
  | elem Dead (nextmoveGhoModes move state) = pacVsGhostDead move state
  | elem Alive (nextmoveGhoModes move state) = pacVsGhostAlive move state

-- | Makes move and the changes in the state when the ghost meets a pacman in the mode Mega.
ghostVsPacMega :: Play -> State -> State
ghostVsPacMega m@(Move i o) state@(State maze pl l) = State maze (checkTimeMegazero ((setGhostsCoords [getPlayerByID i pl] (getMiddleOfMaze maze))
                                                      ++[addPlayerPoints 10 (getPacman pl)]
                                                      ++(rmGhostByID i pl))) l

-- | The nextmoveGhoModes is responsable for returning the ghosts modes that are in the next move of the pacman.
nextmoveGhoModes :: Play -> State -> [GhostMode]
nextmoveGhoModes move state@(State _ pl _) = getGhostsModes $ findGhosts pl $ moveNewCoords move state


-- | This function is used in the case where's a food big and one or more ghosts in the next move of the pacman.
pacVsGhostFB :: Play -> State -> State
pacVsGhostFB move state@(State maze pl l) = State (replacePiece state) (addPlayerPoints ((10*(numberOfGs move state))+5) (switchMouth $ getPacman pl)
                                            :(setGhostsMode Alive (setGhostsCoords (findGhosts pl $ moveNewCoords move state) (getMiddleOfMaze maze)))
                                            ++((\\) (rmPacman pl) (findGhosts pl $ moveNewCoords move state))) l

-- | pacVsGhostDead
pacVsGhostDead :: Play -> State -> State
pacVsGhostDead move state@(State maze pl l) = case (checkNextpiece move state) of
  Food Little -> State (replacePiece state) (checkTimeMegazero ((setPlayerCoords (addPlayerPoints ((10*(numberOfDeadGs move state))+1) (switchMouth $ getPacman pl)) (moveNewCoords move state))
                 :(setGhostsMode Alive (setGhostsCoords (findGhosts pl $ moveNewCoords move state) (getMiddleOfMaze maze)))
                 ++((\\) (rmPacman pl) (findGhosts pl $ moveNewCoords move state)))) l
  _ -> State (replacePiece state) (checkTimeMegazero ((setPlayerCoords (addPlayerPoints (10*(numberOfDeadGs move state)) (switchMouth $ getPacman pl)) (moveNewCoords move state))
           :(setGhostsMode Alive (setGhostsCoords (findGhosts pl $ moveNewCoords move state) (getMiddleOfMaze maze)))
           ++((\\) (rmPacman pl) (findGhosts pl $ moveNewCoords move state)))) l


-- | pacVsGhostAlive
pacVsGhostAlive :: Play -> State -> State
pacVsGhostAlive move state@(State maze pl l) =
  if getPacLifes pl <= 1
  then
    case (checkNextpiece move state) of
      Food Little -> State (replacePiece state) ((setPlayerCoords (addPlayerPoints 1 $ loseLife $ setPacMode Dying $ switchMouth $ getPacman pl) (initialCoordsPac maze)): rmPacman pl) l
      _ -> State (replacePiece state) ((setPlayerCoords (setPacMode Dying $ loseLife $ switchMouth $ getPacman pl) (initialCoordsPac maze)): rmPacman pl) l
  else
    case (checkNextpiece move state) of
      Food Little -> State (replacePiece state) ((setPlayerCoords (addPlayerPoints 1 $ loseLife $ switchMouth $ getPacman pl) (initialCoordsPac maze)): rmPacman pl) l
      _ -> State (replacePiece state) ((setPlayerCoords (loseLife $ switchMouth $ getPacman pl) (initialCoordsPac maze)): rmPacman pl) l


{- | A função numberOfGhosts é responsável por contar o número de ghosts com uma determinada
coordenada de entre uma lista de players. -}
numberOfGs :: Play -> State -> Int
numberOfGs move state@(State m pl l) = length (findGhosts pl $ moveNewCoords move state)


{- | A função numberOfDeadGs é responsável por contar o número de ghosts cujo Mode é Dead
e que se encontrem na coordenada seguinte à do pacman. -}
numberOfDeadGs :: Play -> State -> Int
numberOfDeadGs move state@(State m pl l) = length (getGhostsByMode Dead $ findGhosts pl $ moveNewCoords move state)


{- | A função findghosts é responsável por encontar numa lista de quaisquer players
os ghosts com uma certa coordenada. -}
findGhosts :: [Player] -> Coords -> [Player]
findGhosts [] _ = []
findGhosts ((Pacman x):xs) y = findGhosts xs y
findGhosts ((Ghost x):xs) y | (getPlayerCoords (Ghost x)) == y = (Ghost x) : findGhosts xs y
                            | otherwise = findGhosts xs y


{- | A setGhostsCoords é responsável por colocar colocar coordenadas num
conjunto de Ghosts de uma lista. -}
setGhostsCoords :: [Player] -> Coords -> [Player]
setGhostsCoords [] _ = []
setGhostsCoords (gho@(Ghost (GhoState (x,n,z,h,t,q) p)):xs) (a,b) = (setPlayerCoords gho (a,b)) : (setGhostsCoords xs (a,b))


{- | A função getPacLifes é responsável por dar de entre uma lista
de players o número de vidas do pacman. -}
getPacLifes :: [Player] -> Int
getPacLifes pl = getsoloPacLifes (getPacman pl)


{- | A função getsoloPacLifes é responsável por dar o valor das vidas
de um player. -}
getsoloPacLifes :: Player -> Int
getsoloPacLifes (Pacman (PacState (_,_,_,_,_,l) _ _ _)) = l


-- | A função loseLife é responsável por retirar uma vida ao pacman.
loseLife :: Player -> Player
loseLife pac@(Pacman (PacState (a,b,c,d,e,l) f g h)) = if l-1>=0
                                                       then (Pacman (PacState (a,b,c,d,e,l-1) f g h))
                                                       else (Pacman (PacState (a,b,c,d,e,0) f g h))

{- | A função rmGhots é responsável por remover uma lista de ghosts de
uma lista de ghots. -}
rmGhosts :: [Player] -> [Player] -> [Player] -- ^ Takes a playerlist and removes the same players of the second.
rmGhosts [] _ = []
rmGhosts l [] = l
rmGhosts (x:xs) l
   | elem x l = rmGhosts xs l
   | otherwise = x : rmGhosts xs l


{- | A função getGhostsModes utiliza a função getGhostMode como
auxiliar e é responsável por dar uma lista com todos  os modos
de uma lista de players Ghost. -}
getGhostsModes :: [Player] -> [GhostMode]
getGhostsModes [] = []
getGhostsModes (x:xs) = getGhostMode x : getGhostsModes xs


{- | A função getGhostMode é responsável por dar o mode
de ghost. -}
getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState a m)) = m


{- | A função getGhostsByMode é responsável por retirar de uma lista de players
os ghosts com um determinado GhostMode. -}
getGhostsByMode :: GhostMode -> [Player] -> [Player]
getGhostsByMode _ [] = []
getGhostsByMode gm (ghost@(Ghost (GhoState a m)):xs)
   | gm == getGhostMode ghost = (Ghost (GhoState a m)) : getGhostsByMode gm xs
   | otherwise = getGhostsByMode gm xs


{- | A função setGhostsMode é responsável por alterar o estado de fantasmas uma
qualquer lista de players, independentemente do tipo ghost ou do tipo pacman. -}
setGhostsMode :: GhostMode -> [Player] -> [Player]
setGhostsMode g [] = []
setGhostsMode g ((Ghost (GhoState a m)):xs) = (Ghost (GhoState a g)) : setGhostsMode g xs
setGhostsMode g ((Pacman (_)):xs) = setGhostsMode g xs


{- | A função setPacMode é responsável por alterar o estado de
um player Pacman por um qualquer. -}
setPacMode :: PacMode -> Player -> Player
setPacMode p (Pacman (PacState a b c d)) = (Pacman (PacState a b c p))


{- | A função initialCoordsPac dá as coordenadas do pacman quando
este morre. -}
initialCoordsPac :: Maze -> Coords
initialCoordsPac (x:xs) | even (length (x:xs)) = (((div (length (x:xs)) 2)-1),0)
                        | otherwise = ((div (length (x:xs)) 2),0)


{- | A getMiddleofMaze é responsável por dar as coordenadas do meio da casa
dos Ghots,importante para quando ocorre a morte de um ghost. -}
getMiddleOfMaze :: Maze -> Coords
getMiddleOfMaze (x:xs) | even (length (x:xs)) = ((div (length (x:xs)) 2)-1, (div (length x) 2))
                       | otherwise = ((div (length (x:xs)) 2), (div (length x) 2))


{- | A função foodlittleMove é responsável por tratar do movimento do pacman
quando a peça seguinte é Food Little e não há intervenção de fantasmas. Para
tal utiliza a função pacFLittle que altera alguns dos valores necessários no
pacman tais como os pontos e as coordenadas. -}
-- README added switchMouth to foodlittleMove, foodBigMove and emptyMove.
foodlittleMove :: Play -> State -> State
foodlittleMove move@(Move i o) state@(State maze pl l) = State maze (checkTimeMegazero (pacFLittle move state : rmPacman pl)) l
  where
    pacFLittle move state = switchMouth $ addPlayerPoints 1 $ setPlayerCoords (getPacman pl) $ moveNewCoords move state


{- | A função foodBigMove é responsável por tratar do movimento do pacman
quando a peça seguinte é Food Big e não há intervenção de fantasmas. Para
tal utiliza a função pacFBig que altera alguns dos valores necessários no
pacman mas também usa importantemente a função setMega definida mais abaixo. -}
foodBigMove :: Play -> State -> State
foodBigMove move@(Move i o) state@(State maze pl l) = State maze (setMega ((pacFBig move state):(rmPacman pl))) l
  where
    pacFBig move state = switchMouth $ setTimeMega $ addPlayerPoints 5 $ setPlayerCoords (getPacman pl) $ moveNewCoords move state


{- | A função emptyMove,através da sua auxiliar pacEmpty cujo objetivo
é alterar as coordenadas do pacman que correspondem à posição seguinte,
resultando disto o state final que é dado, isto quando se trata de um
 movimento onde a peça seguinte é Empty e não há intervenção de fantasmas. -}
emptyMove :: Play -> State -> State
emptyMove move state@(State maze pl l) = State maze (checkTimeMegazero (pacEmpty pl)) l
  where
    pacEmpty [] = []
    pacEmpty (x:xs) = case x of Pacman _ -> (switchMouth $ setPlayerCoords x (moveNewCoords move state)) : xs
                                Ghost _ -> x : pacEmpty xs


{- |A função addPlayerPoints é responsável por adicionar um qualquer
número de pontos a um player à escolha. -}
addPlayerPoints :: Int -> Player -> Player
addPlayerPoints p (Pacman (PacState (x,y,z,t,h,l) q c d )) = (Pacman (PacState (x,y,z,t,h+p,l) q c d ))


{- | A função setMega é responsável por várias alterações ao nível dos players que são
muito úteis, por exemplo na função foodBigMove. -}
setMega :: [Player] -> [Player]
setMega [] = []
setMega ((Pacman (PacState (x,y,v,t,h,l) q c m)):xs) = (Pacman (PacState (x,y,v,t,h,l) q c Mega)) : setMega xs
setMega ((Ghost (GhoState (x,y,v,t,h,l) m')):xs) = (Ghost (GhoState (x,y,v/2,t,h,l) Dead)) : setMega xs


{- | A função repSetGet é responsável pela substituição da orientação do Pacman,
por uma à escolha, que está inserido numa lista de Players. -}
repSetGet :: [Player] -> Orientation -> [Player]
repSetGet [] o = []
repSetGet (x:xs) o = case x of Pacman _ -> (setPlayerOrientation x o) : xs
                               Ghost _ -> x: repSetGet xs o


{- | A função checkID é responsável por ver se um player tem
o mesmo identificador que um número Inteiro. Esta função é
útil para verificar sobre qual dos players é aplicado o
Move. -}
checkID :: Player -> Int -> Bool
checkID x i = if getPlayerID x == i then True else False


{- | A função checkNextpiece é responsável por dar a próxima peça,
tendo em consideração o identificador do (Move _ _ ) e o identificador
de uma lista de players, através da função moveNewCoords e da função
que está no ínicio da tarefa, a função getpiece. -}
checkNextpiece :: Play -> State -> Piece
checkNextpiece move state@(State maze _ _) = getpiece (moveNewCoords move state) maze


{- | A função moveNewCoords é responsável por dar as coordenadas
seguintes ao player cujo identificador é igual ao do (Move _ _),
utilizando a função getPlayerCoords como auxiliar. -}
moveNewCoords :: Play -> State -> Coords
moveNewCoords (Move i o) (State maze (x:xs) level)
   | checkID x i = case o of R -> if snd (getPlayerCoords x) == length (head maze)-1
                                  then ((fst (getPlayerCoords x)),0)
                                  else (fst (getPlayerCoords x), snd (getPlayerCoords x)+1)
                             L -> if snd (getPlayerCoords x) == 0
                                  then ((fst (getPlayerCoords x)),(length (head maze)-1))
                                  else (fst (getPlayerCoords x), snd (getPlayerCoords x)-1)
                             U -> (fst (getPlayerCoords x)-1, snd (getPlayerCoords x))
                             D -> (fst (getPlayerCoords x)+1, snd (getPlayerCoords x))
                             Null -> getPlayerCoords x
   | otherwise = moveNewCoords (Move i o) (State maze xs level)


{- | A função checkcoords é responsável por verificar dentro de uma lista
de players se algum deles está numa posição cuja Piece é uma Wall. -}
checkcoords :: State -> Bool
checkcoords (State m [] l) = True
checkcoords (State m (x:xs) l) = if getpiece (getPlayerCoords x) m /= Wall then checkcoords (State m xs l) else False


-- | The function switchMouth alternates the Mouth of the pacman.
switchMouth :: Player -> Player
switchMouth (Ghost z) = Ghost z
switchMouth (Pacman (PacState a b m c)) = (Pacman (PacState a b x c))
  where
    x = case m of Open -> Closed
                  _ -> Open


{- | The function switchMouthState returns a state with the mouth of the pacman changed.
 Used in the function pieceMove in the case of a wall. -}
switchMouthState :: State -> State
switchMouthState (State m [] l) = State m [] l
switchMouthState state@(State m pl l) = State m (checkTimeMegazero ((switchMouth $ getPacman pl) : rmPacman pl)) l


-- | This function gets the player by the inputed ID (:: Int) in a player list.
getPlayerByID :: Int -> [Player] -> Player
getPlayerByID _ [] = error("Cant't find the player by id.")
getPlayerByID i (x:xs)
  | getPlayerID x == i = x
  | otherwise = getPlayerByID i xs

-- | Used when the timeMega == 0, changes the modes of all players and change velocity for ghosts that are dead.
changeModes :: [Player] -> [Player]
changeModes [] = []
changeModes (ghost@(Ghost (GhoState (x,y,v,t,h,l) Alive)):xs) = ghost : changeModes xs
changeModes ((Ghost (GhoState (x,y,v,t,h,l) m)):xs) = (Ghost (GhoState (x,y,v*2,t,h,l) Alive)) : changeModes xs
changeModes ((Pacman (PacState (x,y,v,t,h,l) q c m)):xs) = (Pacman (PacState (x,y,v,t,h,l) q c Normal)) : changeModes xs

-- | Checks if the time in mode Mega is over.
checkTimeMegazero :: [Player] -> [Player]
checkTimeMegazero [] = []
checkTimeMegazero pl | getPacmanMode (getPacman pl) == Mega && getPacmanTimeMega (getPacman pl) <= 0 = changeModes pl
                     | otherwise = pl

{- | A função getPacmanTimeMega é responsável por retirar
de um player Pacman o valor do seu Time Mega.-}
getPacmanTimeMega :: Player -> Double
getPacmanTimeMega (Pacman (PacState (x,y,v,t,h,l) q c m)) = q


-- | Function used when the pacman eats a big food and becomes Mega.
setTimeMega :: Player -> Player
setTimeMega (Pacman (PacState (x,y,v,t,h,l) q c m)) = (Pacman (PacState (x,y,v,t,h,l) 10 c m))

{- | A função rmGhostByID é responsável por remover um
ghost com um determinado id de uma lista de players. -}
rmGhostByID :: Int -> [Player] -> [Player]
rmGhostByID _ [] = []
rmGhostByID i (x:xs)
  | getPlayerID x == i = xs
  | otherwise = x : rmGhostByID i xs


-- | Based on a player list returns the list of the respectives players ID's.
getPlayersID :: [Player] -> [Int]
getPlayersID [] = []
getPlayersID ((Pacman (PacState (i,y,z,t,h,l) q c d)):xs) = i : getPlayersID xs
getPlayersID ((Ghost (GhoState (i,y,z,t,h,l) q)):xs) = i : getPlayersID xs



-- | Testing

testCasesplay :: [(Int, Orientation, Int, Int, Int, [Player], Int)]
testCasesplay = [(1, R, 15, 10, 0 ,[Pacman (PacState (1,(2,2),0.0,R,10,1) 0.0 Open Normal)], 1),
                 (0, L, 15, 15, 1 ,[Pacman (PacState (1,(3,3),0.0,R,10,1) 0.0 Open Normal),(Ghost (GhoState (2,(3,3),0.0,R,0,1) Alive))], 1)]

testplay :: [(Int, Orientation, Int, Int, Int, [Player], Int)] -> [State]
testplay [] = []
testplay ((i, o, x, y, z, pl, lvl):h) = (play (Move i o) (State (generateMaze x y z) pl lvl)) : testplay h

printResults2 :: Show a => [a] -> IO()
printResults2 m = mapM_ (\a -> putStrLn ("test play >>>\n" ++ show a)) m

-- $| printResults2 $ testplay testCasesplay
