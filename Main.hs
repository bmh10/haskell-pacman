module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.IO  
import Control.Monad

fps = 60
topLeft = (-fromIntegral width/2 + tileSize/2, fromIntegral height/2 - tileSize/2)
width = 420 -- 28 * 15
height = 465 -- 31 * 15
offset = 100
tilesHorizontal = 30
tileSize = 15
pacmanInitialPos = (tileSize/2, -8*tileSize)
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data PacmanGame = Game
  { 
    level :: [String],
    pacmanPos :: Position
  } deriving Show 

render :: PacmanGame -> Picture 
render g = pictures [renderPacman g, renderLevel g]

renderPacman :: PacmanGame -> Picture 
renderPacman game = translate x y $ color orange $ circleSolid (tileSize/2-1)
  where 
    (x, y) = pacmanPos game

renderLevel :: PacmanGame -> Picture
renderLevel game = renderLines (level game) (snd topLeft)

renderLines :: [String] -> Float -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l (fst topLeft) y, renderLines ls (y-tileSize)]

renderLine :: String -> Float -> Float -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+tileSize) y]

renderTile :: Char -> Float -> Float -> Picture
renderTile 'x' x y = translate x y $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
renderTile '+' x y = translate x y $ color white $ rectangleSolid (tileSize-1) 2
renderTile '.' x y = translate x y $ color yellow $ circleSolid 2
renderTile 'o' x y = translate x y $ color yellow $ circleSolid 4
--renderTile 'p' x y = translate x y $ color orange $ circleSolid (tileSize/2-1)
renderTile _ _ _ = blank

-- Event handling
handleKeys :: Event -> PacmanGame -> PacmanGame
handleKeys (EventKey (Char 'd') Down _ _) game = move game 1 0
handleKeys (EventKey (Char 'a') Down _ _) game = move game (-1) 0
handleKeys (EventKey (Char 'w') Down _ _) game = move game 0 1
handleKeys (EventKey (Char 's') Down _ _) game = move game 0 (-1)
handleKeys _ game = game

move game xm ym = game {pacmanPos = (x', y')}
  where
    (x, y) = pacmanPos game
    x' = x + xm*tileSize
    y' = y + ym*tileSize

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
