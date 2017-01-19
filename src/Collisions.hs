module Collisions (wallBounce,paddleBounce, loose) where
  import Types(Position, Radius, Window(..), PongGame(..), Player(P1, P2), Score(..))
  import Window(window)


  endCollision :: Position -> Radius -> (Bool, Bool)
  endCollision (x,_) radius = (leftCollision,  rightCollision)
    where
      leftCollision = x - radius <= (-fromIntegral (width window) / 2)
      rightCollision =  x + radius >= fromIntegral (width window) / 2

  loose game =
    game {
      score = Score {
        p1Points = p1p
      , p2Points = p2p }
    , ballLoc = ballLoc'
    , ballVel = ballVel' }

    where
      points = score game
      (vx, vy) = ballVel game
      (p1p, p2p, ballLoc', ballVel') =
        case endCollision (ballLoc game) 10 of
          (True, False) -> (p1Points points + 1, p2Points points, 0, (-vx, -vy))
          (False, True) -> (p1Points points, p2Points points + 1, 0, (-vx, -vy))
          _ -> (p1Points points, p2Points points, ballLoc game, (vx, vy))


  paddleCollision :: Position -> Player -> Player -> Radius -> (Bool, Float)
  paddleCollision (x, y) (P1 p1) (P2 p2) radius
    | leftCollision = (True, y - p1)
    | rightCollision = (True, y - p2)
    | otherwise = (False, 0)
    where
      leftCollision=
        x - radius <= (-fromIntegral (width window - 94) / 2)
        && x - radius >= (-fromIntegral (width window - 88) / 2)
        && y < (p1 + 43) && y > (p1 - 43)
      rightCollision =
        x + radius >= fromIntegral (width window - 94) / 2
        && x + radius <= fromIntegral (width window - 88) / 2
        && y < (p2 + 43) && y > (p2 - 43)

  paddleBounce :: PongGame -> PongGame
  paddleBounce game = game { ballVel = (vx', vy') }
    where
      radius = 10
      -- The old velocities.
      (vx, vy)  = ballVel game
      (vx',vy') =
        case paddleCollision (ballLoc game) (player1 game) (player2 game) radius of
          (True, i) -> (-vx, i*3)
          (False, i) -> (vx, vy)

  -- | Given position and radius of the ball, return whether a collision occurred.
  wallCollision :: Position -> Radius -> Bool
  wallCollision (_, y) radius = topCollision || bottomCollision
    where
      topCollision    = y - radius <= -fromIntegral (height window) / 2
      bottomCollision = y + radius >=  fromIntegral (height window) / 2

  wallBounce :: PongGame -> PongGame
  wallBounce game = game { ballVel = (vx, vy') }
    where
      -- Radius. Use the same thing as in `render`.
      radius = 10

      -- The old velocities.
      (vx, vy) = ballVel game

      vy' = if wallCollision (ballLoc game) radius
            then
               -- Update the velocity.
               -vy
             else
              -- Do nothing. Return the old velocity.
              vy
