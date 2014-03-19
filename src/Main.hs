import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils

data Tile = Tile {
              name :: String,
              tileAttrs :: Attributes
}

instance MovieClip Tile where
    attrs = tileAttrs
    render t = (color black $ box 100 100) <> (scale 0.3 0.3 $ color white $ text (name t))

gameState = [Tile "adit" defaultAttrs, Tile "calvin" (defaultAttrs { azindex = 2, ax = 25, ay = 25 })]

main :: IO ()
main = play "test game" (500, 500) gameState on stepGame

stepGame _ state = return state

moveLeft (Tile name attrs) = Tile name (attrs { ax = (ax attrs - 10) })
moveRight (Tile name attrs) = Tile name (attrs { ax = (ax attrs + 10) })

on (EventKey (SpecialKey KeyLeft) Down _ _) (x:xs) = return (moveLeft x:xs)
on (EventKey (SpecialKey KeyRight) Down _ _) (x:xs) = return (moveRight x:xs)
on _ state = return state
