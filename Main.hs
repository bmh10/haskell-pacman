module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import System.IO  
import Control.Monad

fps = 60
width = 600
height = 400
offset = 100
tileSize = 15
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data PacmanGame = Game
  { 
    level :: [String]
  } deriving Show 

render :: PacmanGame -> IO Picture 
render game
  | otherwise       = renderGame game

renderGame :: PacmanGame -> IO Picture
renderGame game = return $ renderLines (level game) (200)

renderLines :: [String] -> Float -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l (-300) y, renderLines ls (y-tileSize)]

renderLine :: String -> Float -> Float -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+tileSize) y]

renderTile :: Char -> Float -> Float -> Picture
renderTile 'x' x y = translate x y $ color blue $ rectangleSolid (tileSize-1) (tileSize-1)
renderTile '.' x y = translate x y $ color yellow $ circleSolid 2
renderTile 'o' x y = translate x y $ color yellow $ circleSolid 4
renderTile _ _ _ = blank

-- Event handling
handleKeys :: Event -> PacmanGame -> IO PacmanGame
handleKeys (EventKey (Char 'r') Down _ _) game = return game
handleKeys _ game = return game

update :: Float -> PacmanGame -> IO PacmanGame
update seconds game = return game

-- Not sure why print is required...
initTiles = do 
  handle <- openFile "2.lvl" ReadMode
  contents <- hGetContents handle
  let rows = words contents
  let initialState = Game { level = rows }
  print rows
  hClose handle
  return initialState

main = do
  initialState <- initTiles
  playIO window background fps initialState render handleKeys update
