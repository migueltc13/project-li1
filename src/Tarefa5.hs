{- | Passando agora à tarefa 5 cujo objetivo é implementar um comportamento para os fantasmas. Nesta Tarefa
decidimos adotar uma estratégia de separar a função em vários casos de forma a que os ghosts tomassem as melhores
decisões de forma a resolver vários "bugs" que foram aparecendo. Assim, temos uma restrição para quando o pacman
está dentro da casa mas no lado direito para os mesmos irem para o centro da casa para serem dirigidos até à saída.
O processo é semelhante para os dois casos seguintes. Para o 4ª caso dá-mos uma direção ao ghost para os mesmos irem
atrás do pacman. Em seguida temos um caso em que o pacman está rodeado por 3 paredes. Os próximos casos permitem ao ghost
passar nos dois túneis caso o pacman se encontre do lado oposto. Em seguida temos os casos mais simples de deslocamento do
ghost comparando as suas coordenadas com as do pacman. Quando o pacman se encontra no modo dead o mesmo segue um conjunto
de restrições que o fazem andar no sentido dos ponteiros do relógio. -}
module Tarefa5 where
import Data.List -- (\\)
import Tarefa2
import Types
import Tarefa1 -- ^ Testing

{- | A função ghostPlay é responsável por tratar
por dar a lista de jogadas de todos os players num
step. -}

ghostPlay :: State -> [Play]
ghostPlay state@(State maze pl@(x:xs) l) = rmNulls $ map (\y-> moveGho pac y maze) (rmPacman pl)
                                          where pac = getPacman pl
ghostPlay (State maze _ l) = []

{- | A função rmNulls é responsável por filtrar
a lista de jogadas removendo todas aquelas onde
ghost não sai do sítio. -}

rmNulls :: [Play] -> [Play]
rmNulls [] = []
rmNulls (m@(Move i o):t) = case o of Null -> rmNulls t
                                     _ -> m : rmNulls t

{- | Function that provides the next move for a ghost based on the pacman coords,
 the next piece, the ghost mode and the infrastructure of the maze. -}

moveGho :: Player -> Player -> Maze -> Play
moveGho pac@(Pacman (PacState (_,(h,t),_,_,_,_) j _ _)) gho@(Ghost (GhoState (i,(x,y),_,a,_,_) gmode)) maze
  | elem (getPlayerCoords gho) (ghoHouseCoordsdir maze) = Move i L
  | elem (getPlayerCoords gho) (ghoHouseCoordsesq maze) = Move i R
  | elem (getPlayerCoords gho) (ghoHouseCoords maze) = Move i U
  | elem (getPlayerCoords gho) (ghofugirhouse maze) = if y<t then (Move i R) else if y>t then (Move i L) else (Move i Null)
  | (countWalls $ pecasvizinhas maze (x,y)) == 3 = justmove gho maze -- ^ When only one move is available.
  | y<=2 && t >= comp = gotunelesq gho maze
  | t<=2 && y >= comp = gotuneldir gho maze
  | x<h && nextPiece D gho maze /= Wall && gmode == Alive = Move i D
  | x>h && nextPiece U gho maze /= Wall && gmode == Alive = Move i U
  | y<t && nextPiece R gho maze /= Wall && gmode == Alive = Move i R
  | y>t && nextPiece L gho maze /= Wall && gmode == Alive = Move i L
  | x<h && nextPiece U gho maze /= Wall && gmode == Dead = Move i U
  | x>h && nextPiece D gho maze /= Wall && gmode == Dead = Move i D
  | y<t && nextPiece L gho maze /= Wall && gmode == Dead = Move i L
  | y>t && nextPiece R gho maze /= Wall && gmode == Dead = Move i R
  | gmode == Dead && j > 9.75 = Move i (posicaoOposta a)
  | gmode == Dead = escolherNovaDirecao maze gho
  | x==h && y==t = Move i Null -- ^ Case the pacman and ghost are in the same coords.
  | otherwise = Move i R
    where lista = pecasvizinhas maze (x,y)
          comp = (length (head maze)-1)-2 -- ^ distancia para fugir das paredes

 --y <= 3 && y >= comp = principalfugirdasparedesparacima i (x,y) (h,t) lista maze

{- | A função nextPiece é responsável
por dar a peça seguinte ao ghost se o mesmo
manter a direção e o sentido que o mesmo tinha
na sua última jogada. -}

nextPiece :: Orientation -> Player -> Maze -> Piece
nextPiece o (Ghost (GhoState (_,(x,y),_,_,_,_) _ )) maze | o == R = getpiece (x,y+1) maze
                                                         | o == L = getpiece (x,y-1) maze
                                                         | o == U = getpiece (x-1,y) maze
                                                         | o == D = getpiece (x+1,y) maze


{- | A função countwalls é responsável
por ver quantas paredes tem um ghost tem
à sua volta. -}

countWalls :: [Piece] -> Int
countWalls [] = 0
countWalls (x:xs) = case x of Wall -> 1 + countWalls xs
                              _ -> countWalls xs


{- | A função justmove é responsável por dar um
movimento na mesma direção e no mesmo sentido da
jogada anterior se a peça seguinte não for uma parede. -}

justmove :: Player -> Maze -> Play -- Its the same move for ghost Dead and Alive
justmove gho@(Ghost (GhoState (i,(x,y),_,_,_,_) _)) maze
 | nextPiece R gho maze /= Wall = Move i R
 | nextPiece L gho maze /= Wall = Move i L
 | nextPiece U gho maze /= Wall = Move i U
 | nextPiece D gho maze /= Wall = Move i D

{- | A função gotunneldir é responsável
por chamar a função chooseCoordsdir,
sendo esta última uma sua auxiliar de
modo a fazer o ghost passar o túnel direito. -}

gotuneldir :: Player -> Maze -> Play
gotuneldir pl maze = chooseCoordsdir (getPlayerID pl) (getPlayerCoords pl) coordstuneldir
                       where coordstuneldir = ((div (length maze) 2) +1,(length $ head maze)-1)

{- | A função chooseCoordsdir é responsável
por dar instruções ao ghost para o mesmo ir
para o túnel direito de forma a perseguir facilmente
o pacman caso ele esteja do lado contrario do mapa.-}

chooseCoordsdir :: Int -> Coords -> Coords -> Play--playercoords;tunnelcoords
chooseCoordsdir a (x,y) (h,t) | y<(t-1) = Move a R
                              | x>h = Move a U
                              | x<h = Move a D
                              | otherwise =  Move a R

{- | A função gotunneldir é responsável
por chamar a função chooseCoordsdir,
sendo esta última uma sua auxiliar de
modo a fazer o ghost passar o túnel esquerdo. -}

gotunelesq ::Player -> Maze -> Play
gotunelesq pl maze = chooseCoordsesq (getPlayerID pl) (getPlayerCoords pl) coordstunelesq
                          where coordstunelesq = ((div (length maze) 2) +1 ,0)

{- | A função chooseCoordsesq é responsável
por dar instruções ao ghost para o mesmo ir
para o túnel esquerdo de forma a perseguir facilmente
o pacman caso ele esteja do lado contrario do mapa-}

chooseCoordsesq :: Int -> Coords -> Coords -> Play--playercoords;tunnelcoords
chooseCoordsesq a (x,y) (h,t) | y>=1 = Move a L
                              | x>h = Move a U
                              | x<h = Move a D
                              | otherwise =  Move a L

{- | A função coordsvizinhas é responsável
por dar a lista de coordendas vizinhas
de uma, ou seja aquelas que estão à sua
direita,esquerda,acima ou abaixo desta.-}

coordsvizinhas :: Coords -> [Coords]
coordsvizinhas (x,y) = [(x,y-1),(x,y+1),(x-1,y),(x+1,y)] -- [L,R,U,D]

{- | A função pecasvizinhas é responsável
por dar as peças que se encontram nas coordendas
vizinhas ao ghost.-}

pecasvizinhas :: Maze -> Coords -> [Piece]
pecasvizinhas maze coords = map (getPiece maze) coordslist
                                where coordslist = (coordsvizinhas coords)
                                      getPiece :: Maze -> Coords -> Piece   -- | A função getPiece é responsável por dar a peça num maze-}
                                      getPiece ((z:t):ts) (x,y) | x==0 = piecex y (z:t)
                                                                | otherwise = getPiece (take (length ((z:t):ts)-1) ts) (x-1,y)

{- | A função checkchangeorientation é responsável
por verificar se um ghost pode fazer uma determinada
mudança de direção verificando se ele irá embater contra
alguma parede.-}

checkchangeorientation:: Orientation -> [Piece] -> Bool
checkchangeorientation o (a:b:c:d:[]) = case o of L -> a /= Wall
                                                  R -> b /= Wall
                                                  U -> c /= Wall
                                                  D -> d /= Wall

{-| A função principalfugirdasparedesparacima é utilizada
quando o ghost estiver na situação de ter de fugir das paredes
na vertical  de modo a impedir com que o mesmo não fique
"preso" naquele lugar.
-}

principalfugirdasparedesparacima :: Int -> Coords -> Coords -> [Piece] -> Maze -> Play --ghost ; pac
principalfugirdasparedesparacima i (x,y) (h,t) pieces@[a,b,c,d] maze | y<t && checkchangeorientation R pieces = Move i R
                                                                     | y>t && checkchangeorientation L pieces = Move i L
                                                                     | x >= (div (length maze) 2) = Move i D
                                                                     | otherwise = Move i U

{- | A função scatterMode é responsável por
dar sequência a todos os casos onde os ghosts
estão a jogar contra um pacman no modo Mega -}

scatterMode :: State -> Int -> Play -- Fugir
scatterMode s@(State maze pl l) pid = Move pid (posicaoOposta $ getPlayerOrientation $ getPlayerByID pid pl) -- fazer com que ele so entre nesta restrição uma unica vez

{- | A função fugir é responsável por dar sequência
 a todos os casos onde os ghosts estão no modo death
e a fugir de um pacman. -}

--fugir :: State -> Int -> Play
--fugir s@(State maze pl l) x | nextPiece orientation player maze /= Wall = Move x orientation
--                            | otherwise = escolherNovaDirecao s x
--                                   where orientation = (getPlayerOrientation $ getPlayerByID x pl)
--                                         player = getPlayerByID x pl

{- | A função escolherNovaDirecao é uma função auxiliar
utilizada na função fugir que retrata os casos de deslocamento
do ghosts e fazem com que o mesmo esteja sempre em
movimento no sentido dos ponteiros do relogio
-}

escolherNovaDirecao ::Maze -> Player -> Play
escolherNovaDirecao maze gho@(Ghost (GhoState (i,(x,y),_,a,_,_) gmode)) = Move i (virarADireita a)
                     where virarADireita :: Orientation -> Orientation
                           virarADireita R | nextPiece R gho maze == Wall = D
                                           | otherwise = R
                           virarADireita U | nextPiece U gho maze == Wall = R
                                           | otherwise = U
                           virarADireita L | nextPiece L gho maze == Wall = U
                                           | otherwise = L
                           virarADireita D | nextPiece D gho maze == Wall = L
                                           | otherwise = D


-- | Esta função retorna o move para quando os ghost se encontram na porta da casa dos fantasmas. (Move id U)
moveGhostHouse :: Maze -> Player -> Play
moveGhostHouse maze x
 | elem (getPlayerCoords x) (ghoHouseCoords maze) = (Move (getPlayerID x) U) -- : moveGhostHouse maze xs
 | otherwise = (Move (getPlayerID x) Null)


{- | A função getGhosInHouse verifica se algum ghost ocupa as coordenadas definidas na função ghoHouseCoords
se tal não acontecer é retornada a lista vazia. -}
getGhosInHouse :: Maze -> [Player] -> [Player]
getGhosInHouse _ [] = []
getGhosInHouse maze (x:xs)
 | elem (getPlayerCoords x) (ghoHouseCoords maze) = x : getGhosInHouse maze xs
 | otherwise = getGhosInHouse maze xs

{- | A função ghoHouseCoords é utlizada para verificar se algum ghost está na 'porta' da casa de fantasmas.
O seu output dá as coords da porta da casa dos fantasmas sendo estas variáveis dependentes da paridade do maze inputed. -}
ghoHouseCoords :: Maze -> [Coords]
ghoHouseCoords m | (even $ length m) = [((length m `div` 2),(div (length (m!!0)) 2)-1),
                                                                ((length m `div` 2), (div (length (m!!0)) 2)),
                                                                ((length m `div` 2)-1,(div (length (m!!0)) 2)-1),
                                                                ((length m `div` 2)-1, (div (length (m!!0)) 2))]
                 | (odd $ length m) = [((length m `div` 2), (div (length (m!!0)) 2)-1),
                                                                 ((length m `div` 2), div (length (m!!0)) 2),
                                                                 ((length m `div` 2), (div (length (m!!0)) 2)+1),
                                                                 ((length m `div` 2)-1, (div (length (m!!0)) 2)-1),
                                                                 ((length m `div` 2)-1, div (length (m!!0)) 2),
                                                                 ((length m `div` 2)-1, (div (length (m!!0)) 2)+1)]


ghofugirhouse :: Maze -> [Coords]
ghofugirhouse m | (even $ length m) = [((length m `div` 2)-2,(div (length (m!!0)) 2)-1),
                                                                ((length m `div` 2)-1, (div (length (m!!0)) 2))]
                | (odd $ length m) = [((length m `div` 2)-2, (div (length (m!!0)) 2)-1),
                                                                 ((length m `div` 2)-2, div (length (m!!0)) 2),
                                                                 ((length m `div` 2)-2, (div (length (m!!0)) 2)+1)]
{- | A função ghoHouseCoordsesq é utlizada para verificar se algum ghost está do lado esquerdo de dentro da casa de
fantasmas e assim dar lhe indicações para ele sair.-}

ghoHouseCoordsesq :: Maze -> [Coords]
ghoHouseCoordsesq m
                    | (even $ length m) = [((length m `div` 2),(div (length (m!!0)) 2)-3),((length m `div` 2), (div (length (m!!0)) 2)-2)]
                    | (odd $ length m) = [((length m `div` 2), (div (length (m!!0)) 2)-3),((length m `div` 2), (div (length (m!!0)) 2)-2)]

{- | A função ghoHouseCoordsdir é utlizada para verificar se algum ghost está do lado direito de dentro da casa de
fantasmas e assim dar lhe indicações para ele sair.-}
ghoHouseCoordsdir::Maze -> [Coords]
ghoHouseCoordsdir m
                    | (even $ length m) = [((length m `div` 2),(div (length (m!!0)) 2)+1) , ((length m `div` 2), (div (length (m!!0)) 2)+2)]
                    | (odd $ length m) = [((length m `div` 2), (div (length (m!!0)) 2)+2) , ((length m `div` 2), (div (length (m!!0)) 2)+3)]




{- | A função posicaoOposta é chamada na função
scatterMode e é responsável por dar a orientação
oposta do player à que o mesmo tinha na jogada
anterior. -}

posicaoOposta:: Orientation -> Orientation
posicaoOposta R = L
posicaoOposta U = D
posicaoOposta L = R
posicaoOposta D = U


-- Testing
state1 = State (generateMaze 15 15 1) [Ghost (GhoState (2,(3,5),0.0,R,0,1) Alive),
                                       Pacman (PacState (1,(3,3),0.0,R,10,1) 0.0 Open Normal),
                                       Ghost (GhoState (2,(5,5),0.0,R,0,1) Alive),
                                       Ghost (GhoState (2,(6,7),0.0,R,0,1) Dead),
                                       Ghost (GhoState (3,(7,4),0.0,R,0,1) Alive)] 1

teste= [Ghost (GhoState (2,(3,5),0.0,R,0,1) Alive),Pacman (PacState (1,(3,3),0.0,R,10,1) 0.0 Open Normal),Ghost (GhoState (2,(5,5),0.0,R,0,1) Alive),Ghost (GhoState (2,(6,7),0.0,R,0,1) Dead),Ghost (GhoState (3,(7,4),0.0,R,0,1) Alive)]
