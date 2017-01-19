module Events (handleKeys, m'') where
  import Types(PongGame(..), PaddleDir(..), Player(..), Window(..), P1Controls(..), P2Controls(..))

  import Graphics.Gloss.Interface.Pure.Game

  import Window(window)

  import Collisions(wallBounce, paddleBounce)

  -- | Respond to key events.
  handleKeys :: Event -> PongGame -> PongGame
  -- For a 'p' keypress, pause the game
  handleKeys (EventKey (Char 'p') Down _ _) game =
    game { paused = not (paused game) }
  handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
    game { p2control = Just ArrowUp }
  handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game =
    game { p2control = Nothing }
  handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
    game { p2control = Just ArrowDown }
  handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game =
    game { p2control = Nothing }

  handleKeys (EventKey (Char 's') Down _ _) game =
    game { p1control = Just S }
  handleKeys (EventKey (Char 's') Up _ _) game =
    game { p1control = Nothing }
  handleKeys (EventKey (Char 'w') Down _ _) game =
    game { p1control = Just W }
  handleKeys (EventKey (Char 'w') Up _ _) game =
    game { p1control = Nothing }
  -- Do nothing for all other events.
  handleKeys _ game = game


  m :: PongGame -> PongGame
  m game =
    case p1control game of
      Just W -> movePaddle game (PdllUp, player1 game)
      Just S -> movePaddle game (PdllDown, player1 game)
      Nothing -> game

  m' game =
    case p2control game of
      Just ArrowUp -> movePaddle game (PdllUp, player2 game)
      Just ArrowDown -> movePaddle game (PdllDown, player2 game)
      Nothing -> game

  m'' = m' . m

  movePaddle :: PongGame -> (PaddleDir,Player) -> PongGame
  -- Player 1
  movePaddle game (PdllUp, P1 p1) =
    if p1 + 43 < (fromIntegral (height window) / 2) - 10 then
      game { player1 = P1 (3 + p1) }
    else
      game
  movePaddle game (PdllDown, P1 p1) =
    if p1 - 43 > -(fromIntegral (height window) / 2) + 10 then
      game { player1 = P1 (p1 - 3) }
    else
      game
  -- Player 2
  movePaddle game (PdllUp, P2 p2) =
    if p2 + 43 < (fromIntegral (height window) / 2) - 10 then
      game { player2 = P2 (p2 + 3) }
    else
      game
  movePaddle game (PdllDown, P2 p2) =
    if p2 - 43 > -(fromIntegral (height window) / 2) + 10 then
      game { player2 = P2 (p2 - 3) }
    else
      game
