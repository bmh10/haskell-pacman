module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import Control.Monad

fps = 60
width = 420 -- 28 * 15
height = 465 -- 31 * 15
offset = 100
tileSize = 15
pacmanInitialPos = (1,1)
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data PacmanGame = Game
  { 
    level :: [String],      -- Level layout
    pacmanPos :: (Int, Int) -- Tile coord of pacman
  } deriving Show 

-- Tile functions
getTile :: PacmanGame -> Int -> Int -> Char
getTile g x y = (level g) !! y !! x

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> Position 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

-- Rendering
render :: PacmanGame -> Picture 
render g = pictures [renderLevel g, renderPacman g]

renderPacman :: PacmanGame -> Picture 
renderPacman game = translate x y $ color orange $ circleSolid (tileSize/2-1)
  where 
    (x, y) = tileToCoord $ pacmanPos game

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
handleKeys (EventKey (Char 'd') Down _ _) game = move game 1 0
handleKeys (EventKey (Char 'a') Down _ _) game = move game (-1) 0
handleKeys (EventKey (Char 'w') Down _ _) game = move game 0 (-1)
handleKeys (EventKey (Char 's') Down _ _) game = move game 0 1
handleKeys _ game = game

move game xm ym = game {pacmanPos = (x', y')}
  where
    (x, y) = pacmanPos game
    x' = if getTile game (x+xm) y == 'x' then x else x + xm
    y' = if getTile game x (y+ym) == 'x' then y else y + ym

update :: Float -> PacmanGame -> PacmanGame
update seconds game = game

-- Not sure why print is required...
initTiles = do 
  handle <- openFile "2.lvl" ReadMode
  contents <- hGetContents handle
  let rows = words contents
  let initialState = Game { level = rows, pacmanPos = pacmanInitialPos }
  print rows
  hClose handle
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
