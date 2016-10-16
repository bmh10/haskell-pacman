module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import System.Random
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 5
width = 420 -- 28 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileHoriz = 27
pacmanInitialPos = (1,1)
redGhostInitialPos = (1,5)
blueGhostInitialPos = (1,6)
yellowGhostInitialPos = (1,7)
pinkGhostInitialPos = (1,8)
pacmanInitialLives = 3
pacmanInitialDir = East
ghostInitialDir = East
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data PlayerState = Normal | Scared | Returning deriving (Eq, Show)

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East
oppositeDir None  = None

nextDir :: Direction -> Direction
nextDir West = North
nextDir d = succ d 

data PacmanGame = Game
  { 
    level :: [String],      -- Level layout
    pacmanPos :: (Int, Int), -- Tile coord of pacman
    pacmanDir :: Direction,  -- Pacman's direction of travel
    pacmanNextDir :: Direction, -- Buffered next direction
    ghostPos :: [(Int, Int)],
    ghostDir :: [Direction],
    ghostState :: [PlayerState],
    score :: Int,
    lives :: Int,
    seconds :: Float
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> PacmanGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> PacmanGame -> PacmanGame
setTile x y c g = g {level = updatedLevel}
  where 
    updatedLevel = take y (level g) ++ [take x ((level g) !! y) ++ [c] ++ drop (x+1) ((level g) !! y)] ++ drop (y+1) (level g)

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> Position 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

-- Rendering
render :: PacmanGame -> Picture 
render g = pictures [renderLevel g, 
                     renderPlayer "pacman" (pacmanPos g) (pacmanDir g) Normal (seconds g),
                     renderPlayer "redGhost" ((ghostPos g) !! 0) ((ghostDir g) !! 0) ((ghostState g) !! 0) (seconds g),
                     renderPlayer "blueGhost" ((ghostPos g) !! 1) ((ghostDir g) !! 1) ((ghostState g) !! 1) (seconds g),
                     renderPlayer "yellowGhost" ((ghostPos g) !! 2) ((ghostDir g) !! 2) ((ghostState g) !! 2) (seconds g),
                     renderPlayer "pinkGhost" ((ghostPos g) !! 3) ((ghostDir g) !! 3) ((ghostState g) !! 3) (seconds g),
                     renderDashboard g]

renderPlayer :: String -> (Int, Int) -> Direction -> PlayerState -> Float -> Picture 
renderPlayer player (x, y) dir state seconds = translate x' y' $ GG.png file
  where 
    (x', y') = tileToCoord (x, y)
    file = getFile player dir state seconds

-- TODO: should preload images
getFile :: String -> Direction -> PlayerState -> Float -> String
getFile player dir state seconds
 | state == Scared = "img/scaredGhost" ++ step ++ ".png"
 | state == Returning = "img/eyes.png"
 | otherwise = "img/" ++ player ++ show dir ++ step ++ ".png"
  where 
    step = if (mod (round seconds) 2) == 1 then "1" else "2"

renderDashboard :: PacmanGame -> Picture
renderDashboard g = pictures [scorePic, livesPic]
  where
    scorePic = color white $ translate (-50) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ show $ score g
    livesPic = color white $ translate 50 (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ show $ lives g

renderLevel :: PacmanGame -> Picture
renderLevel game = renderLines (level game) 0

renderLines :: [String] -> Int -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l 0 y, renderLines ls (y+1)]

renderLine :: String -> Int -> Int -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+1) y]

renderTile :: Char -> Int -> Int -> Picture
renderTile c x y
 | c == 'x'  = translate x' y' $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
 | c == '+'  = translate x' y' $ color white $ rectangleSolid (tileSize-1) 2
 | c == '.'  = translate x' y' $ color yellow $ circleSolid 2
 | c == 'o'  = translate x' y' $ color yellow $ circleSolid 4
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)

-- Event handling
handleKeys :: Event -> PacmanGame -> PacmanGame
handleKeys (EventKey (Char 'd') Down _ _) g = setPacmanDir East g
handleKeys (EventKey (Char 'a') Down _ _) g = setPacmanDir West g
handleKeys (EventKey (Char 'w') Down _ _) g = setPacmanDir North g
handleKeys (EventKey (Char 's') Down _ _) g = setPacmanDir South g
handleKeys _ game = game

setPacmanDir dir g
 | (pacmanDir g) == oppositeDir dir = g { pacmanDir = dir, pacmanNextDir = None } 
 | otherwise                        = g { pacmanNextDir = dir }

-- Have to update lives twice to prevent missed collision
update :: Float -> PacmanGame -> PacmanGame
update seconds game = updateScore $ updateLives $ updateGhosts 3 $ updateLives $ updatePacman $ updateSeconds game

updateSeconds :: PacmanGame -> PacmanGame
updateSeconds game = game {seconds = (seconds game) + 1}

updateLives :: PacmanGame -> PacmanGame
updateLives g
 | ghostIdx == Nothing = g
 | (ghostState g) !! (fromJust ghostIdx) /= Normal = setGhostReturning g (fromJust ghostIdx) 
 | otherwise = g {lives = (lives g) - 1}
  where
    ghostIdx = elemIndex (pacmanPos g) (ghostPos g)

updateScore :: PacmanGame -> PacmanGame
updateScore g
  | tile == '.' = setBlankTile $ g { score = s+10 }
  | tile == 'o' = setGhostsScared $ setBlankTile $ g { score = s+50 }
  | otherwise = g
  where
    (x, y) = pacmanPos g
    s = score g
    tile = getTile x y g
    setBlankTile = setTile x y '_'

setGhostsScared g = g {ghostState = replicate 4 Scared}
setGhostReturning g idx = g {ghostState = take idx (ghostState g) ++ [Returning] ++ drop (idx+1) (ghostState g)}

updatePacman g = updatePacmanPos g

-- If ghost does not move after update (e.g. hit a wall), change direction then update again
updateGhosts :: Int -> PacmanGame -> PacmanGame
updateGhosts 0 g = updateGhost 0 g
updateGhosts n g = updateGhost n $ updateGhosts (n-1) g  

updateGhost :: Int -> PacmanGame -> PacmanGame
updateGhost idx g
  | x == x' && y == y' = updateGhost idx $ g {ghostDir = ((take idx (ghostDir g)) ++ [nextDir dir] ++ (drop (idx+1) (ghostDir g)))}
  | otherwise          = g {ghostPos = (take idx (ghostPos g) ++ [(x', y')] ++ drop (idx+1) (ghostPos g))}
  where
    (x, y)   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    (x', y') = updateGhostPos g dir (x, y)

updatePacmanPos :: PacmanGame -> PacmanGame
updatePacmanPos g
 | canMove (x, y) nextDir g = g { pacmanPos = (move (x, y) nextDir), pacmanDir = nextDir, pacmanNextDir = None }
 | canMove (x, y) dir g     = g { pacmanPos = (move (x, y) dir) }
 | otherwise                = g
  where
    dir = pacmanDir g
    nextDir = pacmanNextDir g
    (x, y) = pacmanPos g

updateGhostPos :: PacmanGame -> Direction -> (Int, Int) -> (Int, Int)
updateGhostPos g dir (x, y) = if canMove (x, y) dir g then move (x, y) dir else (x, y)

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) None = (x, y)
move (x, y) East = (wrapx $ x+1, y)
move (x, y) West = (wrapx $ x-1, y)
move (x, y) North = (x, y-1)
move (x, y) South = (x, y+1)

canMove :: (Int, Int) -> Direction -> PacmanGame -> Bool
canMove (x, y) None g = False
canMove (x, y) dir g = canMoveTo g $ move (x, y) dir

canMoveTo :: PacmanGame -> (Int, Int) -> Bool
canMoveTo g (x, y) = getTile x y g /= 'x'

wrapx x
 | x < 0 = maxTileHoriz
 | x > maxTileHoriz = 0
 | otherwise = x

-- Not sure why print is required...
initTiles = do 
  handle <- openFile "2.lvl" ReadMode
  contents <- hGetContents handle
  let rows = words contents
  let initialState = Game { level = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = [redGhostInitialPos, blueGhostInitialPos, yellowGhostInitialPos, pinkGhostInitialPos], ghostDir = [ghostInitialDir, ghostInitialDir, ghostInitialDir, ghostInitialDir], ghostState = replicate 4 Normal, score = 0, seconds = 0, lives = pacmanInitialLives, pacmanNextDir = None }
  print rows
  hClose handle
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
