{- | Passando agora à tarefa 4 cujo objetivo é calcular o efeito da passagem de um instante de tempo num estado do jogo.
Para tratar de todos os objetivos começamos por colocar todas as restrições pedidas que incluíam o facto do ppacman abrir
e fechar a boca alternadamente entre cada jogada, o Pacman em modo Mega deve perder tempo mega em cada jogada, o Pacman
deve voltar ao modo Alive se o tempo mega for <=0 e estiver em Mega, os Fantasmas devem voltar ao modo Alive se não houver
nenhum Pacman em modo Mega. A última condição foi colocada nesta Tarefa 4 que consistia em colocar os jogadores a progredir
n jogadas em cada iteração da função, em que n é um número inteiro inferido a partir da velocidade do jogador e paridade
da iteração (step) da jogada. Assim, esta Tarefa é também responsável por chamar a função da Tarefa 5 como também é responsável
por fazer todas as jogadas sobre os ghosts, visto que a jogada do Pacman é feita na função nextFrame aplicando a função play
sobre o state dado por esta função.
-}
module Tarefa4 where

import Types
import Tarefa5
import Tarefa2

defaultDelayTime = 250
{-}
passTime :: Int  -> State -> State
passTime pid s = aux (ghostPlay s) s
 where
  aux [] s = s
  aux (h:t) s = aux t (play h s)
-}
-- FIXME
passTime :: Int  -> State -> State
passTime step s@(State maze pl l) | even step = aux (ghostPlay s) s  --acrescenta ((getPacman pl):ghostdeath pl) $ aux (ghostPlay state) state
                                  | odd step = aux (ghostPlay state) s
               where
                aux [] s = s
                aux (h:t) s = aux t (play h s)
                state = State maze (others pl) l
                ghost = playNull $ ghostdeath pl

--acrescentar outros ghosts ao state

acrescenta :: [Player] -> State -> State
acrescenta [] s = s
acrescenta pl (State maze players l) = State maze (pl ++ players) l

playNull::[Player] -> [Play]
playNull [] = []
playNull (x:xs) = map (\y -> Move (getPlayerID y) Null) (x:xs)


ghostdeath :: [Player] -> [Player]
ghostdeath [] = []
ghostdeath (x:xs) = case x of Ghost a -> if getPlayerMode x == Dead then x : ghostdeath xs else ghostdeath xs
                              Pacman _ -> ghostdeath xs
{-}
ghostdeath :: [Player] -> [Player]
ghostdeath [] = []
ghostdeath (x:xs) | getplayervelocity x == 0.5 =  x : ghostdeath xs
                  | otherwise = ghostdeath xs
-}

others :: [Player] -> [Player]
others [] = []
others (x:xs) = case x of Ghost a -> if getPlayerMode x == Alive then x : others xs else others xs
                          Pacman _ -> x: others xs
{-}
others :: [Player] -> [Player]
others [] = []
others (x:xs) | getplayervelocity x == 1.0 =  x : ghostdeath xs
              | otherwise = ghostdeath xs
-}
getplayervelocity :: Player -> Double
getplayervelocity (Pacman (PacState (x,y,z,t,h,l) q c d)) = z
getplayervelocity (Ghost (GhoState (x,y,z,t,h,l) q)) = z

getPlayerMode :: Player -> GhostMode
getPlayerMode (Ghost (GhoState (x,y,z,t,h,l) q)) = q

{-}
passTime :: Int  -> State -> State
passTime a s@(State maze pl l) = State maze (jogarplayers a ((ghostdeath pl) ++ (others pl)) maze l) l

ghostdeath :: [Player] -> [Player]
ghostdeath [] = []
ghostdeath (x:xs) | getplayervelocity x == 0.5 =  x : ghostdeath xs
                  | otherwise = ghostdeath xs

others :: [Player] -> [Player]
others [] = []
others (x:xs) | getplayervelocity x == 1.0 =  x:others xs
              | otherwise = others xs

jogarplayers :: Int -> [Player] -> Maze -> Int -> [Player]
jogarplayers a (x:xs) maze b | odd a = func $ State maze (x:xs) b
                             | even a = if getplayervelocity x == 0.5 then x:jogarplayers a xs maze b else (func $ State maze (x:xs) b)

func :: State -> [Player]
func s = getPlayerlist $ aux (ghostPlay s) s
               where
                aux [] s = s
                aux (h:t) s = aux t (play h s)


--pcte :: State
--pcte = State mazetest [Pacman ((PacState (0,(5,8),(1.5),U,1,1)) 10 Open Mega),(Ghost (GhoState (1,(4,8),(0.5),D,1,1) Alive))] 1

getplayervelocity :: Player -> Double
getplayervelocity (Pacman (PacState (x,y,z,t,h,l) q c d)) = z
getplayervelocity (Ghost (GhoState (x,y,z,t,h,l) q)) = z
-}
