module Main(main) where

import           Events                             (handleKeys)
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.Pure.Game
import           PongState                          (initialState, update)
import           Types                              (Player (..), PongGame (..),
                                                     Score (..), Window (..),
                                                     getPlayer)
import           Window                             (winHeight, winWidth,
                                                     window)

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose p1x $ getPlayer $ player1 game,
            mkPaddle orange p2x $ getPlayer $ player2 game,
            middleLine, scores $ score game ]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    midWinX = fromIntegral winWidth / 2

    p1x = - (midWinX - 30)
    p2x =  midWinX - 30

    midWinY = fromIntegral winHeight / 2
    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid (fromIntegral winWidth - 30) 10

    wallColor = greyN 0.5
    walls = pictures [wall midWinY, wall (-midWinY)]

    middleLine = uncurry translate (0, 0) $ color wallColor $ rectangleWire 10 (fromIntegral winWidth)

    write txt x  =
      translate x (midWinY - 60)
      $ scale (1/4) (1/4)
      $ color wallColor
      $ text txt

    scores :: Score -> Picture
    scores points =
      pictures
        [ write (show $ p1Points points) (midWinX/2)
        , write (show $ p2Points points) (- midWinX/2) ]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

pongdisplay :: Display
pongdisplay = InWindow "Pong" (width window, height window) (offset window, offset window)

main :: IO ()
main = play pongdisplay (background window) (fps window) initialState render handleKeys update
