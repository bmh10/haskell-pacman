module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import Control.Monad

fps = 5
width = 420 -- 28 * 15
height = 465 -- 31 * 15
offset = 100
tileSize = 15
maxTileHoriz = 27
pacmanInitialPos = (1,1)
pacmanInitialDir = East
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data Direction = North | East | South | West deriving (Enum, Eq, Show)

data PacmanGame = Game
  { 
    level :: [String],      -- Level layout
    pacmanPos :: (Int, Int), -- Tile coord of pacman
    pacmanDir :: Direction,  -- Pacman's direction of travel
    score :: Int
  } deriving Show 

-- Tile functions
getTile :: PacmanGame -> Int -> Int -> Char
getTile g x y = (level g) !! y !! x

setTile :: Int -> Int -> Char -> PacmanGame -> PacmanGame
setTile x y c g = g {level = updatedLevel}
  where 
    updatedLevel = take y (level g) ++ [take x ((level g) !! y) ++ [c] ++ drop (x+1) ((level g) !! y)] ++ drop (y+1) (level g)

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> Position 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

-- Rendering
render :: PacmanGame -> Picture 
render g = pictures [renderLevel g, renderPacman g, renderScore g]

renderPacman :: PacmanGame -> Picture 
renderPacman game = translate x y $ color orange $ circleSolid (tileSize/2-1)
  where 
    (x, y) = tileToCoord $ pacmanPos game

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
update seconds game = updateScore $ updatePacman game

updateScore :: PacmanGame -> PacmanGame
updateScore g = 
  if getTile g x y == '.' then setTile x y '_' $ g { score = s+1 } else g 
  where
    (x, y) = pacmanPos g
    s = score g

updatePacman :: PacmanGame -> PacmanGame
updatePacman g
 | dir == East  = move g 1 0
 | dir == West  = move g (-1) 0
 | dir == North = move g 0 (-1)
 | dir == South = move g 0 1
   where dir = pacmanDir g

move game xm ym = game {pacmanPos = (x', y')}
  where
    (x, y) = pacmanPos game
    x' = if getTile game (wrapx $ x+xm) y == 'x' then x else wrapx $ x + xm
    y' = if getTile game x (y+ym) == 'x' then y else y + ym

wrapx pos
 | pos < 0 = maxTileHoriz
 | pos > maxTileHoriz = 0
 | otherwise = pos

-- Not sure why print is required...
initTiles = do 
  handle <- openFile "2.lvl" ReadMode
  contents <- hGetContents handle
  let rows = words contents
  let initialState = Game { level = rows, pacmanPos = pacmanInitialPos, pacmanDir = pacmanInitialDir, score = 0 }
  print rows
  hClose handle
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
