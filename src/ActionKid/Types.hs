module ActionKid.Types where
import Graphics.Gloss.Interface.IO.Game
import ActionKid.Utils
import ActionKid.Globals
import Data.StateVar
import Control.Monad hiding (join)
import Control.Lens
import System.IO.Unsafe

data Attributes = Attributes {
                    ax :: Float,
                    ay :: Float,
                    ascaleX :: Float,
                    ascaleY :: Float,
                    avisible :: Bool,
                    azindex :: Int
}

defaultAttrs = Attributes 0.0 0.0 1.0 1.0 True 1

-- these should be lenses so we can get and set

class MovieClip a where
    getAttrs :: a -> Attributes
    setAttrs :: a -> Attributes -> a
    render :: a -> Picture

    x :: Lens a a Float Float
    x = lens (ax . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { ax = new }))

    y :: Lens a a Float Float
    y = lens (ay . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { ay = new }))

    scaleX :: Lens a a Float Float
    scaleX = lens (ascaleX . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { ascaleX = new }))

    scaleY :: Lens a a Float Float
    scaleY = lens (ascaleY . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { ascaleY = new }))

    visible :: Lens a a Bool Bool
    visible = lens (avisible . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { avisible = new }))

    zindex :: Lens a a Int Int
    zindex = lens (azindex . getAttrs) (\mc new -> setAttrs mc ((getAttrs mc) { azindex = new }))

    display :: a -> Picture
    display mc
      | mc ^. visible = translate (mc ^. x) (mc ^. y) $
                        scale (mc ^. scaleX) (mc ^. scaleY) $
                        render mc
      | otherwise = blank
