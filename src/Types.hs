module Types where

import Data.List

-- | The data State is composed by a maze (:: Maze), a playersState (:: [Player]) and a level (:: Int).
data State = State
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

-- | The type Maze is composed by a list of Corridors.
type Maze = [Corridor]

-- | The type Corridor is composed by a list of Pieces.
type Corridor = [Piece]

{- | The data Piece can be a Food with a data FoodType (Big or Little),
a PacPlayer with a data Player (Pacman or Ghost),
a Empty Piece or
a Wall piece. -}
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data FoodType = Big | Little deriving (Eq)

-- | The data Player either is Pacman (PacState) or Ghost (GhoState).
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

{- | The data PacState is composed by a pacState (:: PlayerState),
a timeMega, a openClosed (:: Mouth), and a pacmanMode (:: PacMode). -}
data PacState = PacState
    {
        pacState :: PlayerState
    ,   timeMega :: Double      -- ^ represents the time left (in ms) to change pacmode back to Normal (timeMega > 0 => pacmode = Mega )
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    } deriving Eq

{- | The data GhoState is composed by a ghostState (:: PlayerState) and a
ghostMode (:: GhostMode). -}
data GhoState = GhoState
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

-- FIXME (IN HADDOCK)
{- | The type PlayerState is present in the datas PacState and GhoState,
The PlayerState is given by: \n
ID (:: Int) -> identifier for the player, where the ID is unique for each player; \n
Coords (:: (Int,Int)) -> player coordenates in a maze; \n
Velocity (:: Double) -> velocity of the player; \n
Orientation (:: Orientation) -> direction faced by the player; \n
Points (:: Int) -> score of the Player; \n
Lives (:: Int) -> number of player lifes; -}
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives)

{- | The type Coords represents the coordinates of a player in a maze where the
first coord represents the number of Corridors and the second the number of
pieces on each Corridor. -}
type Coords = (Int,Int)

-- | The data Orientation determines the direction a Player is facing. (Left, Right, Up, Down and Null)
data Orientation = L | R | U | D | Null deriving (Eq,Show)

-- | The data Mouth is used for the data pacState and it can either be Closed or Open.
data Mouth = Open | Closed deriving (Eq,Show)

-- FIXME (IN HADDOCK)
{- | The data Pacmode is formed by the datas:
Dying -> Represents when the pacman dies;
Normal -> The pacman can be killed by ghosts when the GhostMode is Alive);
Mega -> This is the only time where pacman can defeat ghosts (when PacMode switches to Mega, all the ghosts GhostMode switch to Dead);
 Pacman is switched to Mega when he eats a Food Big.
-}
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead | Alive deriving (Eq,Show)

data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq

{- | The data Play is formed by a player ID (:: Int) and a Orientation.
 The ID espeficies the player we want to move and the Orientation
the direction where we want to move that player.
-}
data Play = Move Int Orientation deriving (Eq,Show)

-- | The type Instructinons is used to optimize the maze defined in Tarefa1.hs
type Instructions = [Instruction]

-- | The data Instruction
data Instruction = Instruct [(Int, Piece)]        -- ^ The first elem of the tuple is the number of times that a consecutive Piece repeats.
                 | Repeat Int deriving (Show, Eq) -- ^ Repeats the n corridor where n is the number of corridors starting at 0.



instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show (PacState s o m Dying  ) =  "X"
   show (PacState (a,b,c,R,i,l) _ Open m) = "{"
   show (PacState (a,b,c,R,i,l) _ Closed m) = "<"
   show (PacState (a,b,c,L,i,l) _ Open m) = "}"
   show (PacState (a,b,c,L,i,l) _ Closed m) = ">"
   show (PacState (a,b,c,U,i,l) _ Open m) = "V"
   show (PacState (a,b,c,U,i,l) _ Closed m) = "v"
   show (PacState (a,b,c,D,i,l) _ Open m) = "^"
   show (PacState (a,b,c,D,i,l) _ Closed m) = "|"
   show (PacState (a,b,c,Null,i,l) _ Closed m) = "<"
   show (PacState (a,b,c,Null,i,l) _ Open m) = "{"

instance Show Player where
   show (Pacman x) = show x
   show (Ghost x) = show x

instance Show GhoState where
   show (GhoState x Dead) = "?"
   show (GhoState x Alive) = "M"

instance Show FoodType where
   show (Big) = "o"
   show (Little) = "."

instance Show Piece where
   show (Wall) = coloredString "#" None
   show (Empty) = coloredString " " None
   show (Food z) = coloredString (show z ) Green
   show (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Normal))) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)) Yellow
   show (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Mega  ))) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega  )) Blue
   show (PacPlayer (Pacman (PacState (i,c,x,y,z,l) o m Dying ))) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying )) Red
   show (PacPlayer (Ghost z)) = coloredString (show z) Purple


coloredString :: String -> Color -> String
coloredString x y= x

{-  | y == Blue   = "\x1b[36m" ++ x ++ "\x1b[0m"
    | y == Red    = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green  = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple = "\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow = "\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise   = "\x1b[0m" ++ x
-}

-- | Takes as inputs a list of Players and a Maze and then returns a new Maze with the Players in the respective Coords.
placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs (replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m)

-- | Converts a Maze to a String
printMaze :: Maze -> String
printMaze [] = ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x ) ++ "\n" ++ printMaze xs

-- |  Outputs the String with the ID, the Points and the Lives info for a player given in the input
printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++ " Points:" ++ show e ++ " Lives:" ++ show l ++ "\n"

-- | Takes as input a Player and returns his PlayerState
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d)) = a
getPlayerState (Ghost (GhoState a b)) = a

-- | Take as input a Player and returns the respective Player ID
getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d)) = x
getPlayerID (Ghost (GhoState (x,y,z,t,h,l) q)) = x

-- | Take as input a Player and returns his points
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d)) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q)) = h

-- | Take as input a Player and returns that player Orientation
getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d)) = t
getPlayerOrientation (Ghost (GhoState (x,y,z,t,h,l) q)) = t

-- | Take as input a Piece and returns that Piece Orientation. If the Piece doesn't have any Orientation returns Null
getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) = getPlayerOrientation p
getPieceOrientation _ = Null

-- | Take as input a Player and returns that player Coords
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d)) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b)) = y

-- | Takes as inputs a Player and Coords and then replaces the Player Coords by the new Coords returning a Player with the given new coords
setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d)) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d)
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q)) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q)

-- | Take as input a Player Pacman and returns his PacMode
getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d

-- | Takes as inputs a Coords, a Piece and a Maze and then returns the Maze with the given piece at the given Coords replaced.
replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs)
  | a == 0 = replaceNElem b p x : xs
  | otherwise = x : replaceElemInMaze (a-1,b) p xs

{- | Takes as input a Int (n), a element with any type (t) and a list of elements t type ([t]),
and returns the list of elements t type ([t]) where is replaced the n position element (starting at 0)
by the inputed element with t type in the given list of elements t type.
 If the list haves n or more elements then the returned output is the same list . -}
replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = []
replaceNElem i el (x:xs)
  | i == 0 = el : xs
  | otherwise =  x : replaceNElem (i-1) el xs
