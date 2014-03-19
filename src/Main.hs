import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils

data Tile = Tile {
              name :: String,
              tileAttrs :: Attributes
}

instance MovieClip Tile where
    attrs = tileAttrs
    render t = (color black $ box 100 100) <> (color white $ text (name t))

gameState = [Tile "adit" defaultAttrs]

main :: IO ()
main = play "test game" gameState on stepGame

stepGame _ state = return state
on _ state = return state
