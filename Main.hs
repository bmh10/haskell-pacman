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
    level = ["0000", "1111"]
  }

render :: PacmanGame -> IO Picture 
render game
  | otherwise       = renderGame game

renderGame :: PacmanGame -> IO Picture
renderGame game = return $ f ("101011") 0

f :: String -> Float -> Picture
f [] _       = blank
f (x:xs) xco = translate [pictures xco 0 $ color (greyN 0.5) $ rectangleSolid 30 30, f xs (xco+40)]

-- Event handling
handleKeys :: Event -> PacmanGame -> IO PacmanGame
handleKeys (EventKey (Char 'r') Down _ _) game = return game
handleKeys _ game = return game

update :: Float -> PacmanGame -> IO PacmanGame
update seconds game = do  
  handle <- openFile "1.lvl" ReadMode
  contents <- hGetContents handle
  let singlewords = words contents
  --game {level = singlewords}
  print singlewords
  hClose handle
  return (game)

main = playIO window background fps initialState render handleKeys update
