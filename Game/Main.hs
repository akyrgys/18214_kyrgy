module Main(updateIO, window, background, main, update) where

import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent
import Keys
import PongGame
import Physics
import Picture


-- | Update the game in IO
updateIO :: (Float -> CurrGameState -> IO CurrGameState)
updateIO s game = return $ update s game

window :: Display
window = InWindow "Tennis-v7.21c" (width, windowHeight) (offset, offset)

background :: Color
background = black


-- play :: Display   -- ^ Display window
--      -> Color     -- ^ Background color
--      -> Int       -- ^ Frames per second (FPS)
--      -> PongGame  -- ^ Initial state of the game
--      -> (PongGame -> Picture) -- ^ Rendering the game
--      -> (Event -> PongGame -> PongGame) -- ^ Handlering key events
--      -> (Float -> PongGame -> PongGame) -- ^ Update Game
main :: IO ()
main = playIO window background fps initialState renderIO handleKeysIO updateIO

-- Update the game by moving the ball and bouncing off walls.
update :: Float -> CurrGameState -> CurrGameState
update seconds game
  | xBall - circleRadius <= -fromIntegral width / 2  = game{player2Score = 1+ player2Score game, ballLoc = (0, 0)}
  | xBall + circleRadius >= fromIntegral width / 2   = game{player1Score = 1+ player1Score game, ballLoc = (0, 0)}
  | isPaused game                                = game 
  | (pl1Moves game) /= (0, 0) || (pl2Moves game) /= (0, 0) = bounce . (moveRackets seconds) . (moveBall seconds) $ game
  | otherwise                                    = bounce . (moveBall seconds) $ game
    where xBall = fst (ballLoc game)





