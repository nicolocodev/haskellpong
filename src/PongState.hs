module PongState (initialState, update) where

  import Types(PongGame(..), PaddleDir(..), Player(..), Window(..), Score(..))

  import Graphics.Gloss.Interface.Pure.Game

  import Window(window)

  import Collisions(wallBounce, paddleBounce, loose)

  import Events(m'')

  -- | Update the ball position using its current velocity.
  moveBall :: Float    -- ^ The number of seconds since last update
           -> PongGame -- ^ The initial game state
           -> PongGame -- ^ A new game state with an updated ball position
  moveBall seconds game = game { ballLoc = (x', y') }
   where
     -- Old locations and velocities.
     (x, y) = ballLoc game
     (vx, vy) = ballVel game
     -- New locations.
     x' = x + vx * seconds
     y' = y + vy * seconds

  -- | The starting state for the game of Pong.
  initialState :: PongGame
  initialState = Game
   { ballLoc = (0, 0)
   , ballVel = (180, 60)
   , player1 = P1 0
   , player2 = P2 0
   , paused = False
   , score = Score {p1Points = 0, p2Points = 0 }
   , p1control = Nothing
   , p2control = Nothing  }

  update :: Float -> PongGame -> PongGame
  update seconds game | paused game = game
                      | otherwise = loose $ paddleBounce $ wallBounce $ m'' $ moveBall seconds game
