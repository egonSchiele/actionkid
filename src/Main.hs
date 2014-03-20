{-# LANGUAGE TemplateHaskell #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss

data Direction = L | R deriving (Ord, Show, Eq)

data Tile = Tile {
              _tileColor :: Color,
              _direction :: Direction,
              _tileAttrs :: Attributes
}

makeLenses ''Tile

instance MovieClip Tile where
  getAttrs = _tileAttrs
  setAttrs mc a = mc { _tileAttrs = a }
  render tile = (color (tile ^. tileColor) $ box 100 100)

adit   = Tile blue L defaultAttrs
calvin = Tile red L defaultAttrs

gameState = [adit, y .~ 50 $ calvin]

main = run "test game" (500, 500) gameState on stepGame

stepGame _ state = return $ map moveTile state

moveTile tile
  | tile ^. direction == L && tile ^. x <= 0 = direction .~ R $ tile
  | tile ^. direction == L = x -~ 10 $ tile
  | tile ^. x >= 500       = direction .~ L $ tile
  | otherwise              = x +~ 10 $ tile

on (EventKey (SpecialKey KeyLeft) Down _ _) (t:ts) = return ((x -~ 10 $ t):ts)
on (EventKey (SpecialKey KeyRight) Down _ _) (t:ts) = return ((x +~ 10 $ t):ts)
on _ state = return state
