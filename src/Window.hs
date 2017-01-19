module Window (window, winWidth, winHeight) where
  import Graphics.Gloss
  import Types(Window(..))

  window :: Window
  window = Window
    { width = winWidth
    , height = winHeight
    , offset = 100
    , fps = 70
    , background = black }

  winWidth :: Int
  winWidth = 680

  winHeight :: Int
  winHeight = 480
