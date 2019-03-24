module Physics (moveBall, moveRackets, bounce, wallCollision, racketCollision) where
import PongGame

-- Update the ball position using its current velocity.
moveBall :: Float    -- The number of seconds since last update
         -> CurrGameState 
         -> CurrGameState

moveBall seconds game = game { ballLoc = (x', y'), currTime = (currTime game) + seconds}
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
   --currTime' = seconds

moveRackets :: Float -> CurrGameState -> CurrGameState
moveRackets seconds game = game { player1 = (p1x', p1y'), player2 = (p2x', p2y') }
    where
        vyp1 = snd $ pl1Moves game -- "velocities" of player rackets
        vyp2 = snd $ pl2Moves game 
        --(racketWidth, racketHeight) = racketRect game
        p1y  = snd $ player1 game -- height of player 1 racket
        p2y  = snd $ player2 game -- ------||-------- 2 --||--

        screenHalfHeight = (fromIntegral height / 2)
        racketHalfHeight = racketHeight / 2
        p1upper = p1y + racketHalfHeight
        p1lower = p1y - racketHalfHeight

        p2upper = p2y + racketHalfHeight
        p2lower = p2y - racketHalfHeight

        p1MayMoveUp = p1upper < screenHalfHeight && vyp1 > 0
        p1MayMoveDown = p1lower > -screenHalfHeight && vyp1 < 0

        p2MayMoveUp = p2upper < screenHalfHeight && vyp2 > 0
        p2MayMoveDown = p2lower > -screenHalfHeight && vyp2 < 0

        p1y' = if p1MayMoveUp || p1MayMoveDown then p1y + vyp1 * seconds else p1y
        p2y' = if p2MayMoveUp || p2MayMoveDown then p2y + vyp2 * seconds else p2y


        vxp1 = fst $ pl1Moves game -- "velocities" of player rackets
        vxp2 = fst $ pl2Moves game 
        --(racketWidth, racketHeight) = racketRect game
        p1x = fst $ player1 game -- height of player 1 racket
        p2x = fst $ player2 game -- ------||-------- 2 --||--

        screenHalfWidth = fromIntegral width / 2
        racketHalfWidth = racketWidth / 2
        p1right = p1x + racketHalfWidth
        p1left = p1x - racketHalfWidth

        p2right = p2x + racketHalfWidth
        p2left  = p2x - racketHalfWidth

        p1MayMoveRight = p1right < screenHalfWidth && vxp1 > 0
        p1MayMoveLeft  = p1left > -screenHalfWidth && vxp1 < 0

        p2MayMoveRight = p2right < screenHalfWidth && vxp2 > 0
        p2MayMoveLeft = p2left > -screenHalfWidth && vxp2 < 0

        p1x' = if p1MayMoveRight || p1MayMoveLeft then p1x + vxp1 * seconds else p1x
        p2x' = if p2MayMoveRight || p2MayMoveLeft then p2x + vxp2 * seconds else p2x



-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
bounce :: CurrGameState -> CurrGameState
bounce game  = game { ballVel = (vx', vy') }
        where
        -- The old velocities.
          (vx, vy) = ballVel game
       
          vx' = if (racketCollision (ballLoc game) (player2 game) (player1 game) circleRadius)
               then
                  -- Update the velocity.
                  -vx
               
                else
                 -- Do nothing. Return the old velocity.
                 vx
  
          vy' = if wallCollision (ballLoc game) circleRadius{- || 
             racketCollisionTopBot (ballLoc game) (player2 game) (player1 game) circleRadiusthen-} then -vy else vy



type Radius = Float



-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >=  fromIntegral height / 2

racketCollision :: Position -> Position -> Position -> Radius -> Bool 
racketCollision (xBall, yBall) (xRight, yRight) (xLeft, yLeft) radius = (leftCollision || rightCollision)
  where
    leftCollision  = (xBall - radius <= xLeft + racketWidth / 2) &&
                     (xBall + radius >= xLeft - racketWidth / 2) &&
                     (yBall + radius >= yLeft - racketHeight / 2) &&
                     (yBall - radius <= yLeft + racketHeight / 2)

    rightCollision = (xBall + radius >= xRight - racketWidth / 2) &&
                     (xBall - radius <= xRight + racketWidth / 2) &&
                     (yBall + radius >= yRight - racketHeight / 2) &&
                     (yBall - radius <= yRight + racketHeight / 2)





{-racketCollisionTopBot :: Position -> Position -> Position -> Radius -> Bool 
racketCollision (xBall, yBall) (xRight, yRight) (xLeft, yLeft) radius = (botCollision || topCollision) 
  where
  	botCollision = (xBall - radius <= xLeft + fromIntegral racketWidth / 2) &&
                   (xBall + radius >= xLeft - fromIntegral racketWidth / 2) &&
                   (yBall + )-}