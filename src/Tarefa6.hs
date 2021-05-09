{- | Chegamos agora à última tarefa deste projeto. O objetivo desta tarefa é implementar um robô que jogue
Pacman automaticamente. Desde o início que esta tarefa não se revelou ser uma tarefa ser uma tarefa fácil
visto que teria que ter em conta enúmeros casos. Não foi por isso a Tarefa em que mais trabalhamos e cremos
que ficou um pouco aquém das expectativas. No entanto, tentámos utilizar vários métodos para tentar sobreviver
o Pacman o maior tempo possivel e ainda assim obter pontuação. Tem uma restrição para quando o pacman estiver
rodeado por 3 paredes e depois 4 condições para que o mesmo continue a comer normalmente se nao aparecer nenhum
ghost perto dele. As condições seguintes comparam as coordenadas do pacman e do ghost mais proximo e através do
modo do ghost dá uma jogada para o pacman. Esta tarefa acabou por não funcionar como queriamos mas ficou a ideia.

Conclusão:
Na nossa perspetiva o trabalho correu bem e conseguimos cumprir com todos os critérios das várias tarefas.
Das tarefas 1,2 e 3 concluímos que estas funcionavam corretamente e faziam o que era proposto não só pelos
nossos testes locais, mas também pelas avaliações colocadas com os professores no codeboard.Em relação às
tarefas 4 e 5, a realização destas está de acordo com o nosso agrado pois até à entrega do trabalho não
encontramos nada a que possamos chamar de bug, nos extensos testes que fizemos.Por último, assumimos que
tivemos uma dificuldade na realização da tarefa 6, pois a estratégia aplicada não é, na nossa opinião, a
mais eficiente, mas é a nossa melhor tentativa. Em suma, as tarefas fazem o que lhe é pedido, mas cremos
que, como tudo na vida, possam ser melhoradas. Principalmente tendo em conta o conhecimentos de Haskell
que temos hoje, comparando com o inicio do trabalho.No geral o trabalho acabou por correr bem e achamos
que é uma boa ideia a interligação entre esta UC e PF.
-}


module Tarefa6 where

import Types
import Tarefa2
import Tarefa5
import Tarefa1 -- ^ used for testing



bot :: Int -> State -> Maybe Play
bot pid s@(State m pl l) = case getPlayerByID pid pl of
  Pacman _ -> moveMaybePac (getPacman pl) (rmPacman pl) m l
  _ -> Nothing

{- The moveMaybePac is the main function of the bot and the eating of
the food by the pacman it's prioritized. -}

moveMaybePac :: Player -> [Player] -> Maze -> Int -> Maybe Play
moveMaybePac pac@(Pacman (PacState (i,(x,y),_,_,_,_) _ _ pacmode)) ghosts m l
  | (countWalls $ pecasvizinhas m (x,y)) == 3 = justMovePac pac m
  | nextPiecePac R pac m == Wall = focus pac m
  | nextPiecePac L pac m == Wall = focus pac m
  | nextPiecePac U pac m == Wall = focus pac m
  | nextPiecePac D pac m == Wall = focus pac m
  | x<h && nextPiecePac U pac m /= Wall && gmode == Alive = Just (Move i U)
  | x>h && nextPiecePac D pac m /= Wall && gmode == Alive = Just (Move i D)
  | y<t && nextPiecePac L pac m /= Wall && gmode == Alive = Just (Move i L)
  | y>t && nextPiecePac R pac m /= Wall && gmode == Alive = Just (Move i R)
  | x<h && nextPiecePac D pac m /= Wall && gmode == Dead = Just (Move i D)
  | x>h && nextPiecePac U pac m /= Wall && gmode == Dead = Just (Move i U)
  | y<t && nextPiecePac R pac m /= Wall && gmode == Dead = Just (Move i R)
  | y>t && nextPiecePac L pac m /= Wall && gmode == Dead = Just (Move i L)
  | otherwise = Just (Move i R)
  where
   nearestGho@(Ghost (GhoState (_,(h,t),_,_,_,_) gmode )) = getfstGhost (State m (pac:ghosts) l)



{- Used in the function moveMaybePac and its crucial to the function bot because
its prioritized the eating of Food by the pacman. -}

focus :: Player-> Maze -> Maybe Play
focus pac@(Pacman (PacState (i,(x,y),_,_,_,_) _ _ pacmode)) maze
     | head (pecasvizinhas maze (x,y)) == Food Big = Just  (Move i L)
     | head (tail (pecasvizinhas maze (x,y))) == Food Big = Just (Move i R)
     | last (init (pecasvizinhas maze (x,y))) == Food Big = Just (Move i U)
     | last (pecasvizinhas maze (x,y)) == Food Big  = Just (Move i D)
     | head (pecasvizinhas maze (x,y)) == Food Little = Just  (Move i L)
     | head (tail (pecasvizinhas maze (x,y))) == Food Little = Just (Move i R)
     | last (init (pecasvizinhas maze (x,y))) == Food Little = Just (Move i U)
     | last (pecasvizinhas maze (x,y)) == Food Little = Just (Move i D)
     | otherwise = Just (Move i $ chooseWay' (pecasvizinhas maze (x,y)))

-- | Bases on the adjacent pieces returns the next available move.
chooseWay' :: [Piece] -> Orientation
chooseWay' [a,b,c,d] | a/= Wall = L
                     | b/= Wall = R
                     | c/= Wall = U
                     | d/= Wall = D

-- | Used in the case that the pacman is surrounded by 3 walls.
justMovePac :: Player -> Maze -> Maybe Play
justMovePac pac@(Pacman (PacState (i,_,_,_,_,_) _ _ _)) m
  | nextPiecePac R pac m /= Wall = Just (Move i R)
  | nextPiecePac L pac m /= Wall = Just (Move i L)
  | nextPiecePac U pac m /= Wall = Just (Move i U)
  | nextPiecePac D pac m /= Wall = Just (Move i D)

{- Based on a orienation, the pacman coords and a maze it's provided the
 piece matching the next move of the Pacman. -}
nextPiecePac :: Orientation -> Player -> Maze -> Piece
nextPiecePac o (Pacman (PacState (_,(x,y),_,_,_,_) _ _ _)) m | o == R = getpiece (x,y+1) m
                                                             | o == L = getpiece (x,y-1) m
                                                             | o == U = getpiece (x-1,y) m
                                                             | o == D = getpiece (x+1,y) m

getPlayersByCoords :: Coords -> [Player] -> [Player]
getPlayersByCoords _ [] = []
getPlayersByCoords (y,x) (h:t)
  | getPlayerCoords h == (y,x) = h : getPlayersByCoords (y,x) t
  | otherwise = getPlayersByCoords (y,x) t

checkGhostsAlive :: [Player] -> Bool
checkGhostsAlive [] = False
checkGhostsAlive (h:t)
 | elem Alive (getGhostsModes (h:t)) = True || checkGhostsAlive t
 | otherwise = False

-- NOTA Dont recieve empty lists
getGhostAlive :: [Player] -> [Player]
getGhostAlive [] = []
getGhostAlive (x:xs)
 | getGhostMode x == Alive = [x]
 | otherwise = getGhostAlive xs

getGhostDead :: [Player] -> [Player]
getGhostDead [] = []
getGhostDead (x:xs)
 | getGhostMode x == Dead = [x]
 | otherwise = getGhostAlive xs

{- | If there's a ghost Alive it's returned the first ghost Alive in the playerlist
otherwise its returned the first ghost Dead find in the list of players of the state. -}

getfstGhost :: State -> Player
getfstGhost s@(State m pl l) = if getclosestghosts s == []
                               then (rmPacman pl)!!0
                               else if checkGhostsAlive (getclosestghosts s)
                                    then (getGhostAlive (getclosestghosts s))!!0
                                    else (getGhostDead (getclosestghosts s))!!0



getclosestghosts :: State -> [Player]
getclosestghosts s@(State m pl l) = aux (getPlayerCoords (getPacman pl)) (rmPacman pl) m 1
  where
    aux :: Coords -> [Player] -> Maze -> Int -> [Player]
    aux (y,x) pl m i | 0<x && x<(length m)-1 && 0<y && y<(length $ head m) && map (\x -> getPlayersByCoords x pl) (closeCoords i (y,x)) /= [] = concat $ map (\x -> getPlayersByCoords x pl) (closeCoords i (y,x))
                  --   | x==0 = concat $ map (\x -> getPlayersByCoords x pl) (leftTunnel i (y,x) m)
                  --   | x==(length m)-1 = concat $ map (\x -> getPlayersByCoords x pl) (rightTunnel i (y,x) m)
                     | map (\x -> getPlayersByCoords x pl) (closeCoords (i) (y,x)) == [] = concat $ map (\x -> getPlayersByCoords x pl) (closeCoords (i+1) (y,x))
                     | otherwise = []

closeCoords :: Int -> Coords -> [Coords]
closeCoords i (y,x) = [(y-i,x-i),(y+i,y+i),(y-i,x+i),(y+i,x-i),(y,x-i),(y,x+i),(y-i,x),(y+i,x)]
{- exemplo do padrao closeCoords (i=3)
.  .  .
 . . .
  ...
...P...
  ...
 . . .
.  .  .
-}

{-
fun :: Int -> Coords -> [Player] -> [Player]
fun i (y,x) pl | map (\x -> getPlayersByCoords x pl) (closeCoords i (y,x)) /= [] = concat $ map (\x -> getPlayersByCoords x pl) (closeCoords i (y,x))
               | otherwise = fun (i+1) (y,x) pl

leftTunnel :: Int -> Coords -> Maze -> [Coords]
leftTunnel i (y,x) m = [(y-i,(length m)-1),(y,(length m)-1),(y+i,(length m)-1),(y,x+i),(y+i,x+i),(y-i,x+i),(y-i,x),(y+i,x)]

rightTunnel :: Int -> Coords -> Maze -> [Coords]
rightTunnel i (y,x) m = [(y-i,0),(y,0),(y+i,0),(y-i,x-i),(y,x-i),(y+i,x-i),(y-i,x),(y+i,x)]
-}

-- Testing
state2 = State (generateMaze 15 15 1) [Ghost (GhoState (2,(3,5),0.0,R,0,1) Alive),
                                       Pacman (PacState (1,(6,0),0.0,R,10,1) 0.0 Open Normal),
                                       Ghost (GhoState (2,(6,2),0.0,R,0,1) Alive),
                                       Ghost (GhoState (2,(6,7),0.0,R,0,1) Dead),
                                       Ghost (GhoState (3,(7,4),0.0,R,0,1) Alive)] 1
