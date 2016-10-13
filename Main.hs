module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import System.Random
import Control.Monad

fps = 5
width = 420 -- 28 * 15
height = 465 -- 31 * 15
offset = 100
tileSize = 15
maxTileHoriz = 27
pacmanInitialPos = (1,1)
ghostInitialPos = (1,5)
pacmanInitialDir = East
ghostInitialDir = East
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data Direction = North | East | South | West deriving (Enum, Eq, Show, Bounded)

nextDir :: Direction -> Direction
nextDir West = North
nextDir d = succ d 

data PacmanGame = Game
  { 
    level :: [String],      -- Level layout
    pacmanPos :: (Int, Int), -- Tile coord of pacman
    pacmanDir :: Direction,  -- Pacman's direction of travel
    ghostPos :: (Int, Int),
    ghostDir :: Direction,
    score :: Int
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
render g = pictures [renderLevel g, renderPlayer (pacmanPos g) orange, renderPlayer (ghostPos g) blue, renderScore g]

renderPlayer :: (Int, Int) -> Color -> Picture 
renderPlayer (x, y) col = translate x' y' $ color col $ circleSolid (tileSize/2-1)
  where 
    (x', y') = tileToCoord (x, y)

renderScore :: PacmanGame -> Picture
renderScore g = color white $ scale 0.1 0.1 $ text $ show $ score g 

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
handleKeys (EventKey (Char 'd') Down _ _) game = game { pacmanDir = East }
handleKeys (EventKey (Char 'a') Down _ _) game = game { pacmanDir = West }
handleKeys (EventKey (Char 'w') Down _ _) game = game { pacmanDir = North }
handleKeys (EventKey (Char 's') Down _ _) game = game { pacmanDir = South }
handleKeys _ game = game

update :: Float -> PacmanGame -> PacmanGame
update seconds game = updateScore $ updateGhost $ updatePacman game

updateScore :: PacmanGame -> PacmanGame
updateScore g
  | tile == '.' = setBlankTile $ g { score = s+1 }
  | tile == 'o' = setBlankTile $ g { score = s+10 }
  | otherwise = g
  where
    (x, y) = pacmanPos g
    s = score g
    tile = getTile x y g
    setBlankTile = setTile x y '_'

updatePacman g = g {pacmanPos = updatePlayerPos g (pacmanDir g) (pacmanPos g)}

-- If ghost does not move after update (e.g. hit a wall), change direction then update again
updateGhost g
  | x == x' && y == y' = updateGhost $ g {ghostDir = nextDir (ghostDir g)}
  | otherwise          = g {ghostPos = (x', y')}
  where
    (x, y)   = (ghostPos g)
    (x', y') = updatePlayerPos g (ghostDir g) (ghostPos g)

updatePlayerPos :: PacmanGame -> Direction -> (Int, Int) -> (Int, Int)
updatePlayerPos g dir (x, y)
 | dir == East  = move g (x,y) (1,0)
 | dir == West  = move g (x,y) (-1,0)
 | dir == North = move g (x,y) (0,-1)
 | dir == South = move g (x,y) (0,1)

move :: PacmanGame -> (Int, Int) -> (Int, Int) -> (Int, Int)
move game (x, y) (xm, ym) = (x', y')
  where
    x' = if getTile (wrapx $ x+xm) y game == 'x' then x else wrapx $ x + xm
    y' = if getTile x (y+ym) game == 'x' then y else y + ym

wrapx pos
 | pos < 0 = maxTileHoriz
 | pos > maxTileHoriz = 0
 | otherwise = pos

-- Not sure why print is required...
initTiles = do 
  handle <- openFile "2.lvl" ReadMode
  contents <- hGetContents handle
  let rows = words contents
  let initialState = Game { level = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, ghostPos = ghostInitialPos, ghostDir = ghostInitialDir, score = 0 }
  print rows
  hClose handle
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
