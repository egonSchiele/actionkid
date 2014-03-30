{-# LANGUAGE TemplateHaskell #-}
module ActionKid.Types where
import Graphics.Gloss.Interface.IO.Game
import ActionKid.Utils
import ActionKid.Globals
import Data.StateVar
import Control.Monad hiding (join)
import Control.Lens
import System.IO.Unsafe

data Attributes = Attributes {
                    _ax :: Float,
                    _ay :: Float,
                    _ascaleX :: Float,
                    _ascaleY :: Float,
                    _avisible :: Bool,
                    _azindex :: Int
}

makeLenses ''Attributes

defaultAttrs = Attributes 0.0 0.0 1.0 1.0 True 1

-- these should be lenses so we can get and set

class MovieClip a where
    attrs :: Lens a a Attributes Attributes
    render :: a -> Picture

    x :: Lens a a Float Float
    x = lens (view $ attrs . ax) (flip $ set (attrs . ax))
    -- x = lens (ax . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { ax = new }))

    y :: Lens a a Float Float
    y = lens (view $ attrs . ay) (flip $ set (attrs . ay))

    scaleX :: Lens a a Float Float
    scaleX = lens (view $ attrs . ascaleX) (flip $ set (attrs . ascaleX))

    scaleY :: Lens a a Float Float
    scaleY = lens (view $ attrs . ascaleY) (flip $ set (attrs . ascaleY))

    visible :: Lens a a Bool Bool
    visible = lens (view $ attrs . avisible) (flip $ set (attrs . avisible))

    zindex :: Lens a a Int Int
    zindex = lens (view $ attrs . azindex) (flip $ set (attrs . azindex))

    display :: a -> Picture
    display mc
      | mc ^. visible = translate (mc ^. x) (mc ^. y) $
                        scale (mc ^. scaleX) (mc ^. scaleY) $
                        render mc
      | otherwise = blank
