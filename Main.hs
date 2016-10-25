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
pacmanInitialPos = (13,23)
redGhostInitialPos = (13,11)
blueGhostInitialPos = (15,15)
centerPos = (14,14)
aboveCenterPos = (14,11)
yellowGhostInitialPos = (10,15)
pinkGhostInitialPos = (13,14)
pacmanInitialLives = 3
pacmanInitialDir = East
ghostInitialDir = East
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data Direction = North | East | South | West | None deriving (Enum, Eq, Show, Bounded)
data PlayerState = Normal | CenterZone | Scared | Returning deriving (Eq, Show)

oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir East  = West
oppositeDir West  = East
oppositeDir None  = None

randomDir :: StdGen -> (Direction, StdGen)
randomDir g = (toEnum $ r, g') where (r, g') = randomR (0,3) g

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
    seconds :: Float,
    gen :: StdGen,
    scaredTimer :: Int,
    paused :: Bool
  } deriving Show 

-- Tile functions
getTile :: Int -> Int -> PacmanGame -> Char
getTile x y g = (level g) !! y !! x

setTile :: Int -> Int -> Char -> PacmanGame -> PacmanGame
setTile x y c g = g {level = updatedLevel}
  where 
    updatedLevel = setAtIdx y (setAtIdx x c ((level g) !! y)) (level g)

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> Position 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

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
handleKeys (EventKey (Char 'p') Down _ _) g = g {paused = not (paused g)}
handleKeys _ game = game

setPacmanDir dir g
 | (pacmanDir g) == oppositeDir dir = g { pacmanDir = dir, pacmanNextDir = None } 
 | otherwise                        = g { pacmanNextDir = dir }

-- Have to update lives twice to prevent missed collision
update :: Float -> PacmanGame -> PacmanGame
update seconds game
 | (paused game) = game
 | otherwise     = updateScore $ updateLives $ updateGhosts 3 $ updateLives $ updatePacman $ updateSeconds game

updateSeconds :: PacmanGame -> PacmanGame
updateSeconds game = game {seconds = (seconds game) + 1, scaredTimer = (scaredTimer game) + 1}

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

setGhostsScared g = g {ghostState = replicate 4 Scared, scaredTimer = 0}
setGhostReturning g idx = g {ghostState = setAtIdx idx Returning (ghostState g)}

updatePacman g = updatePacmanPos g

-- If ghost does not move after update (e.g. hit a wall), change direction then update again
updateGhosts :: Int -> PacmanGame -> PacmanGame
updateGhosts 0 g = updateGhost 0 g
updateGhosts n g = updateGhost n $ updateGhosts (n-1) g  

updateGhost :: Int -> PacmanGame -> PacmanGame
updateGhost idx g = updateGhostState idx $ updateGhostPos idx $ updateGhostDir idx g

updateGhostState idx g
 | state == Scared && (scaredTimer g) > 50         = g {ghostState = setAtIdx idx Normal (ghostState g)}
 | state == Returning && (x, y) == centerPos       = g {ghostState = setAtIdx idx CenterZone (ghostState g)}
 | state == CenterZone && getTile x (y+1) g == '+' = g {ghostState = setAtIdx idx Normal (ghostState g)}
 | otherwise = g
  where
    (x, y) = (ghostPos g) !! idx
    state = (ghostState g) !! idx

updateGhostDir idx g
 | atJunction (x, y) dir state g  = updateRandGen sg $ g {ghostDir = setAtIdx idx dir' (ghostDir g)}
 | otherwise = g
  where
    (x, y)   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    state    = (ghostState g) !! idx
    (x', y') = move (x, y) dir
    (dir', sg) = (calculateGhostNextDir idx g)
    updateRandGen :: (Maybe StdGen) -> PacmanGame -> PacmanGame
    updateRandGen Nothing g = g
    updateRandGen (Just sg) g = g {gen = sg}

updateGhostPos idx g
  | canMove state (x, y) dir g = g {ghostPos = setAtIdx idx (x', y') (ghostPos g)}
  | otherwise = g
  where
    (x, y)   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    state    = (ghostState g) !! idx
    (x', y') = move (x, y) dir
  
atJunction (x, y) dir st g = 
  ((dir == North || dir == South) && (canMove st (x, y) East g || canMove st (x, y) West g)) ||
  ((dir == East || dir == West) && (canMove st (x, y) North g || canMove st (x, y) South g))

calculateGhostNextDir :: Int -> PacmanGame -> (Direction, Maybe StdGen)
calculateGhostNextDir idx g
 | state == CenterZone = calculateNextDir g state dir pos aboveCenterPos
 | state == Returning = calculateNextDir g state dir pos centerPos
 | state == Normal = calculateNextDir g state dir pos (getTileToTarget idx g)
 | otherwise = (randDir, Just g')
  where 
    pos   = (ghostPos g) !! idx
    dir      = (ghostDir g) !! idx
    state    = (ghostState g) !! idx
    (randDir, g') = randomDir (gen g)

getTileToTarget idx g
 | idx == 0 = (x, y)
 | idx == 1 = (x-1, y-1)
 | idx == 2 = (x+1, y+1)
 | idx == 3 = (x+10, y-10)
 | otherwise = (x, y)
   where
     (x, y) = pacmanPos g

calculateNextDir :: PacmanGame -> PlayerState -> Direction -> (Int,Int) -> (Int,Int) -> (Direction, Maybe StdGen)
calculateNextDir g st curDir (x,y) (tx,ty)
 | x < tx && canMove st (x,y) East g && curDir /= West = (East, Nothing)
 | x > tx && canMove st (x,y) West g && curDir /= East = (West, Nothing)
 | y < ty && canMove st (x,y) South g && curDir /= North = (South, Nothing)
 | y > ty && canMove st (x,y) North g && curDir /= South = (North, Nothing)
 | mx >= my && canMove st (x,y) South g && curDir /= North = (South, Nothing)
 | mx >= my && canMove st (x,y) North g && curDir /= South = (North, Nothing)
 | my >= mx && canMove st (x,y) East g && curDir /= West = (East, Nothing)
 | my >= mx && canMove st (x,y) West g && curDir /= East = (West, Nothing)
 | otherwise = (randDir, Just g')
  where
    mx = abs $ x - tx
    my = abs $ y - ty
    (randDir, g') = randomDir (gen g)

updatePacmanPos :: PacmanGame -> PacmanGame
updatePacmanPos g
 | canMove Normal (x, y) nextDir g = g { pacmanPos = (move (x, y) nextDir), pacmanDir = nextDir, pacmanNextDir = None }
 | canMove Normal (x, y) dir g     = g { pacmanPos = (move (x, y) dir) }
 | otherwise                = g
  where
    dir = pacmanDir g
    nextDir = pacmanNextDir g
    (x, y) = pacmanPos g

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) None = (x, y)
move (x, y) East = (wrapx $ x+1, y)
move (x, y) West = (wrapx $ x-1, y)
move (x, y) North = (x, y-1)
move (x, y) South = (x, y+1)

canMove :: PlayerState -> (Int, Int) -> Direction -> PacmanGame -> Bool
canMove _ _ None _ = False
canMove state (x, y) dir g = canMoveTo g state dir $ move (x, y) dir

canMoveTo :: PacmanGame -> PlayerState -> Direction -> (Int, Int) -> Bool
canMoveTo g state dir (x, y) = getTile x y g /= 'x' && not (getTile x y g == '+' && dir == South && state /= Returning)

wrapx x
 | x < 0 = maxTileHoriz
 | x > maxTileHoriz = 0
 | otherwise = x

-- Not sure why print is required...
initTiles = do 
  handle <- openFile "2.lvl" ReadMode
  contents <- hGetContents handle
  stdGen <- newStdGen
  let rows = words contents
  let initialState = Game { level = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = [redGhostInitialPos, blueGhostInitialPos, yellowGhostInitialPos, pinkGhostInitialPos], ghostDir = [ghostInitialDir, ghostInitialDir, ghostInitialDir, ghostInitialDir], ghostState = replicate 4 CenterZone, score = 0, seconds = 0, lives = pacmanInitialLives, pacmanNextDir = None, gen = stdGen, scaredTimer = 0, paused = False}
  print rows
  hClose handle
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
