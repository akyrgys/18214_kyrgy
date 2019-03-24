module Keys(handleKeys, handleKeysIO) where 
import Graphics.Gloss.Interface.Pure.Game
import PongGame
import System.Exit


handleKeysIO :: (Event -> CurrGameState -> IO CurrGameState) -- ^ handleKeys function in IO

  -- For an 'q' keypress, exit the game
handleKeysIO (EventKey (Char 'q') Up _ _) game = exitSuccess

  -- | Just return the same function as handleKeys but in IO
handleKeysIO event game = return $ handleKeys event game


handleKeys :: Event -> CurrGameState -> CurrGameState

handleKeys (EventKey (Char 'r') _ _ _) game = 
    game { ballLoc = (0, 0) }

handleKeys (EventKey (Char 'p') (Up) _ _) game = 
    game { isPaused = (not $ isPaused game) }

handleKeys (EventKey (Char 'w') (Up) _ _) game  = 
    game { pl1Moves = (0, 0) }

handleKeys (EventKey (Char 'w') (Down) _ _) game =
    game {pl1Moves  = (0, racketVel) }

handleKeys (EventKey (Char 's') (Up) _ _) game = 
    game { pl1Moves = (0, 0) }

handleKeys (EventKey (Char 's') (Down) _ _) game =
    game { pl1Moves  = (0, -racketVel) }

handleKeys (EventKey (Char 'a') (Up) _ _) game  = 
    game { pl1Moves = (0, 0) }

handleKeys (EventKey (Char 'a') (Down) _ _) game =
    game {pl1Moves  = (-racketVel, 0) }

handleKeys (EventKey (Char 'd') (Up) _ _) game = 
    game { pl1Moves = (0, 0) }

handleKeys (EventKey (Char 'd') (Down) _ _) game =
    game { pl1Moves  = (racketVel, 0) }

handleKeys (EventKey (SpecialKey KeyDown) (Up) _ _) game = 
    game { pl2Moves = (0, 0)  }

handleKeys (EventKey (SpecialKey KeyDown) (Down) _ _) game = 
    game { pl2Moves = (0, -racketVel)   }

handleKeys (EventKey (SpecialKey KeyUp) (Up) _ _) game = 
    game { pl2Moves = (0, 0) }

handleKeys (EventKey (SpecialKey KeyUp) (Down) _ _) game = 
    game { pl2Moves = (0, racketVel)}

handleKeys (EventKey (SpecialKey KeyLeft) (Up) _ _) game  = 
    game { pl2Moves = (0, 0) }

handleKeys (EventKey (SpecialKey KeyLeft) (Down) _ _) game =
    game { pl2Moves = (-racketVel, 0) }

handleKeys (EventKey (SpecialKey KeyRight) (Up) _ _) game = 
    game { pl2Moves = (0, 0) }

handleKeys (EventKey (SpecialKey KeyRight) (Down) _ _) game =
    game { pl2Moves = (racketVel, 0) }

handleKeys (EventKey (Char 'n') (Up) _ _) game = game
handleKeys (EventKey (Char 'n') (Down) _ _) game = initialState

handleKeys _ game = game