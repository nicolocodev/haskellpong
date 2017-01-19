module Types
  ( Radius
  , Position
  , Player (..)
  , PongGame (..)
  , PaddleDir(..)
  , Window(..)
  , Score(..)
  , P1Controls (..)
  , P2Controls (..)
  , getPlayer
  )
  where

    import Graphics.Gloss

    type Radius = Float

    type Position = (Float, Float)

    data Player =
      P1 Float -- ^ Left player paddle height.
      | P2 Float -- ^ Right player paddle height.
      deriving (Show)

    getPlayer :: Player -> Float
    getPlayer (P1 y) = y
    getPlayer (P2 y) = y

    data Score = Score
      { p1Points :: Int
      , p2Points :: Int }

    -- | Data describing the state of the pong game.
    data PongGame = Game
      { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
      , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
      , player1 :: Player           -- ^ Left player paddle height.
                                   -- Zero is the middle of the screen.
      , player2 :: Player           -- ^ Right player paddle height.
      , paused  :: Bool
      , p1control :: Maybe P1Controls
      , p2control :: Maybe P2Controls
      , score :: Score }

    data Window = Window
      { width :: Int
      , height :: Int
      , offset :: Int
      , fps :: Int
      , background :: Color }

    data PaddleDir = PdllUp | PdllDown

    data P1Controls = S | W
    data P2Controls = ArrowUp | ArrowDown
