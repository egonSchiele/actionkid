{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss
import qualified Debug.Trace as D
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Data.Maybe

data Tile = Empty { _ea :: Attributes }
empty = Empty defaultAttrs
makeLenses ''Tile
emptyPNG = fromJust . unsafePerformIO $ loadJuicyPNG "images/empty.png"

instance MovieClip Tile where
    getAttrs (Empty x) = x
    setAttrs (Empty _) new = Empty new
    render (Empty _) = emptyPNG

data GameState = GameState {
                    _tiles :: [Tile],
                    _ga :: Attributes
}

makeLenses ''GameState

instance MovieClip GameState where
    getAttrs = _ga
    setAttrs gs new = gs { _ga = new }
    render ga = pictures $ map ActionKid.display (_tiles ga)

gameState = GameState [empty] defaultAttrs

main = run "chips challenge" (500, 500) gameState on stepGame

on _ gs = return gs
stepGame _ gs = return gs
