{-# LANGUAGE TemplateHaskell #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss

data Tile = Tile {
              _tileColor :: Color,
              _tileAttrs :: Attributes
}

makeLenses ''Tile

instance MovieClip Tile where
  getAttrs = _tileAttrs
  setAttrs mc a = mc { _tileAttrs = a }
  render tile = (color (tile ^. tileColor) $ box 100 100)

adit   = Tile blue defaultAttrs
calvin = Tile red defaultAttrs

gameState = [adit, y .~ 50 $ calvin]

main = run "test game" (500, 500) gameState on stepGame

-- moveTile tile = (x +~ (fromIntegral $ tile ^. moveX)) tile
stepGame _ state = return state

on (EventKey (SpecialKey KeyLeft) Down _ _) (t:ts) = return ((moveX .~ -10.0 $ t):ts)
on (EventKey (SpecialKey KeyRight) Down _ _) (t:ts) = return ((moveX .~ 10.0 $ t):ts)
on (EventKey (SpecialKey KeyUp) Down _ _) (t:ts) = return ((moveY .~ 10.0 $ t):ts)
on (EventKey (SpecialKey KeyDown) Down _ _) (t:ts) = return ((moveY .~ -10.0 $ t):ts)
on (EventKey _ Up _ _) (t:ts) = return ((moveX .~ 0.0 $ moveY .~ 0.0 $ t):ts)
on _ state = return state
