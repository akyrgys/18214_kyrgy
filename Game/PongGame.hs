module PongGame where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

width, height, offset, windowHeight, fps:: Int
windowHeight = 500
width = 400
height = 400
offset = 100
racketWidth = 20
racketHeight = 80
racketLeftPos = -fromIntegral width / 2 + 30
racketRightPos = fromIntegral width / 2 - 30
circleRadius, ping, racketWidth, racketHeight, delay, racketVel :: Float
circleRadius = 10
wallPos1 = 200
wallPos2 = (-200)
wallWidth = fromIntegral width - 30
wallHeight = 10
ping = 2
fps  = 40
delay = 0.1
racketVel = 70

type Position = (Float, Float)
-- Data for current game state. 
data CurrGameState = Game
  { ballLoc      :: Position  -- Pong ball (x, y) location.
  , ballVel      :: (Float, Float)  -- Pong ball (x, y) velocity. 
  , player1      :: Position           -- Left player racket height.
                                  -- Zero is the middle of the screen. 
  , player2      :: Position  -- Right  player racket height.
  , player1Last  :: Position  -- player1 position delay time ago
  , player2Last  :: Position  -- player2 position delay time ago
  , isPaused     :: Bool            -- is paused or unpaused flag
  , pl1Moves     :: (Float, Float) -- player1 velocitity
  , pl2Moves     :: (Float, Float) -- player2 velocity
  , player1Score :: Int              
  , player2Score :: Int
  , currTime     :: Float          -- time after starting a game
  , lastRacketCollision :: Float   -- time of the last bounce of the ball from the racket
  } deriving Show 

  -- The starting state for the game.
initialState :: CurrGameState
initialState = Game
  { ballLoc  = (-10, 30)
  , ballVel  = (-80, -120)
  , player1  = (racketLeftPos, 40)
  , player2  = (racketRightPos, -80)
  , player1Last = (racketLeftPos, 40)
  , player2Last = (racketRightPos, -80)
  , pl1Moves = (0, 0)
  , pl2Moves = (0, 0)
  , player1Score = 0
  , player2Score = 0
  , isPaused = False
  , currTime = 0
  , lastRacketCollision = (-0.2)
  }


