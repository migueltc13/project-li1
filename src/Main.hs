module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import FileUtils
import Types
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6

data Manager = Manager
    {
        state  :: State
    ,   pid    :: Int
    ,   step   :: Int
    ,   before :: Integer
    ,   delta  :: Integer
    ,   delay  :: Integer
    }
    deriving Show

{- | A função loadManager é responsável por dar o Manager
inicial para utilizar na função main. -}

loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/2.txt") 0 0 0 0 defaultDelayTime )

{- | A função updateControlledPlayer é responsável
por alterar o orientação do player controlado
por uma pessoa quando se pressiona uma tecla. -}

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k m = case k of KeyUpArrow -> changeplayerorientation U m
                                       KeyDownArrow -> changeplayerorientation D m
                                       KeyLeftArrow -> changeplayerorientation  L m
                                       KeyRightArrow -> changeplayerorientation  R m

{- | A função changeplayerorientation é responsável
por alterar o manager para o manager com a nova
orientação do player. -}

changeplayerorientation :: Orientation -> Manager -> Manager
changeplayerorientation o (Manager state pid step before delta delay)= Manager (findplayer pid o state) pid step before delta delay

{- | A função findplayer é responsável por encontrar o player
 em questão auxiliando a função changeplayerorientation
 e alterar a orientação. -}

findplayer :: Int -> Orientation -> State -> State
findplayer x o (State m (y:ys) l) | x == getPlayerID y = State m ((setPlayerOrientation y o):ys) l
                                  | otherwise = State m (aux x (y:ys) o) l
                                        where aux i [] o = []
                                              aux i (x:xs) o= x: aux i xs o

{- | A função updateScreen é responsável por atualizar
o ecrâ. -}

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      -- clear
                      setColor a
                      moveCursor 0 0
                      drawString $ show (state man)
                    render

{- | A função currentTime é responsável por dar o tempo
em segundos num determinado momento. -}

currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

{- | A função updateTime é responsável por atualizar
o tempo nos valores do tipo manager num dado momento. -}

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager state pid step before delta delay) = Manager state pid step before (now-before) delay

{- | A função nextFrame é responsável por fazer as jogadas
de todos os players. -}

nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state pid step before delta delay) = Manager (play (Move pid orientation) (passTime step state)) pid (step+1) now 0 delay
          where
            orientation = getPlayerOrientation $ findplayer2 pid state


nextFramebot :: Integer -> Manager -> Manager
nextFramebot now (Manager state pid step before delta delay) = Manager (play (unMaybe pid state) (passTime step state)) pid (step +1) now 0 delay
          where orientation = getPlayerOrientation $ findplayer2 pid state
                resultado = unMaybe pid state

--Função que implementa a jogada do bot do pacman
unMaybe :: Int -> State -> Play
unMaybe i (State m x n) = unMaybe (bot i (State m x n)) i
 where unMaybe y i = case y of
                   Nothing -> (Move i Null)
                   Just a -> a
{- | A função findplayer2 é responsável por encontrar o player
 em questão através de um ID. -}

findplayer2 :: Int-> State -> Player
findplayer2 x (State m (y:ys) l) | x == getPlayerID y = y
                                 | otherwise = findplayer2 x (State m ys l)

{- | A função loop é responsável por verificar se alguma
tecla foi pressionada e por verificar se os 250 segundos
ja se passaram sendo por isso uma base de toda a 2ª parte
do projeto.-}

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do
  color_schema <- newColorID ColorWhite ColorBlack 10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else if (ev' == EventCharacter 'k') then loop' w $ nextFramebot now man
                      else loop w (updateTime now man)

loop' :: Window -> Manager -> Curses ()
loop' w man@(Manager s pid step bf delt del ) = do
  color_schema <- newColorID ColorWhite ColorBlack 10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop' w $ nextFramebot now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop' w (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else if (ev' == EventCharacter 'k') then loop w $ nextFrame now man
                      else loop' w (updateTime now man)

{- | A função main é responsável por tratar de todos
os movimentos dos players. -}

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager1

loadManager1 :: Manager
loadManager1 = ( Manager pcte 0 0 0 0 defaultDelayTime )

pcte :: State
pcte = State mazetest [Pacman ((PacState (0,(4,0),(1),L,1,1)) 10 Open Mega),(Ghost (GhoState (1,(7,12),(0.5),L,1,1) Dead))] 1

mazetest = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
            [Wall,Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Big, Wall],
            [Wall,Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Big, Wall],
            [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little, Food Little, Food Little, Food Little,Food Little, Food Little, Food Little, Food Little, Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
            [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
            [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
            [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little, Wall,Wall,Wall,Wall,Wall,Wall,Wall, Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
            [Wall,Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Big, Wall],
            [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]
