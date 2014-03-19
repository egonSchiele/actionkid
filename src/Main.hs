import ActionKid
import Graphics.Gloss.Interface.IO.Game
import Data.Monoid ((<>), mconcat)

gameState = [Tile "adit" defaultAttrs]

main :: IO ()
main = do
  playIO
    (InWindow "ones" (500, 500) (1, 1))
    white
    30
    gameState
    draw
    on
    stepGame

draw :: GameState -> IO Picture
draw state = return $ mconcat $ map ActionKid.display state

stepGame _ state = return state
on _ state = return state
