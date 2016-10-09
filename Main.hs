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
tileSize = 30
window = InWindow "Pacman" (width, height) (offset, offset)
background = black

type Radius = Float 
type Position = (Float, Float)

data PacmanGame = Game
  { 
    level :: [String]
  } deriving Show 

initialState :: PacmanGame
initialState = Game
  { 
    level = ["11111", "01110", "11111"]
  }

render :: PacmanGame -> IO Picture 
render game
  | otherwise       = renderGame game

renderGame :: PacmanGame -> IO Picture
renderGame game = return $ renderLines (level initialState) 0

renderLines :: [String] -> Float -> Picture
renderLines [] _ = blank
renderLines (l:ls) y = pictures [renderLine l 0 y, renderLines ls (y-tileSize)]

renderLine :: String -> Float -> Float -> Picture
renderLine [] _ _      = blank
renderLine (t:ts) x y  = pictures [renderTile t x y, renderLine ts (x+tileSize) y]

renderTile :: Char -> Float -> Float -> Picture
renderTile '0' _ _ = blank
renderTile '1' x y = translate x y $ color red $ rectangleSolid (tileSize-1) (tileSize-1)

-- Event handling
handleKeys :: Event -> PacmanGame -> IO PacmanGame
handleKeys (EventKey (Char 'r') Down _ _) game = return game
handleKeys _ game = return game

update :: Float -> PacmanGame -> IO PacmanGame
update seconds game = return game

initTiles = do 
  handle <- openFile "1.lvl" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
  return $ initialState {level = ["00100", "01110", "11111"]}
  print singlewords
  hClose handle

main = do
  initTiles
  playIO window background fps initialState render handleKeys update
