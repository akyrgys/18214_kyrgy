module Picture(renderIO,render) where
import PongGame
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Physics

renderIO :: (CurrGameState -> IO Picture)
renderIO game = return $ render game

--Convert a game state into a picture.
render :: CurrGameState  -> Picture 
render game =
  pictures [ball, walls,
            mkRacket red $ player2 game,
            mkRacket blue $ player1 game,
            translate (-fromIntegral width / 10) (fromIntegral height / 2 +15) $
               color white $
               scale (0.15) (0.35) $
               text (show (player1Score game) ++ " : " ++ show (player2Score game)),
            translate (-fromIntegral width / 10) (-fromIntegral height / 2 - 30) $
               color white $
               scale (0.1) (0.2) $
               text ("TIME " ++ show (truncate (currTime game)))]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid circleRadius
    ballColor = dark yellow

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid (fromIntegral $ wallWidth) (fromIntegral $ wallHeight)

    wallColor = green
    walls = pictures [wall $ fromIntegral $ wallPos1, wall $ fromIntegral $ wallPos2]

    --  Make a racket of a given border and vertical offset.


    mkRacket :: Color -> (Float, Float) -> Picture
    mkRacket col (x, y) = pictures
      [ translate x y $ color col $ 
          rectangleSolid (racketWidth + racketWidth / 4)
            (racketHeight + racketWidth / 4)
      , translate x y $ color racketColor $ rectangleSolid racketWidth racketHeight
      ]

    racketColor = light (light orange)


