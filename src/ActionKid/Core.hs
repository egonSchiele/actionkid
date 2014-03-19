module ActionKid.Core where
import ActionKid.Types
import ActionKid.Utils
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf
import Graphics.Gloss
import Data.Monoid ((<>), mconcat)

data Tile = Tile {
              name :: String,
              tileAttrs :: Attributes
}

instance MovieClip Tile where
    attrs = tileAttrs
    render t = (color black $ box 100 100) <> (color white $ text (name t))

type GameState = [Tile]
