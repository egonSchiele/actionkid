{-# LANGUAGE TemplateHaskell #-}
module ActionKid.Types where
import Graphics.Gloss.Interface.IO.Game
import ActionKid.Utils
import ActionKid.Globals
import Data.StateVar
import Control.Monad hiding (join)
import Control.Lens
import System.IO.Unsafe

-- | Attributes that get added to each MovieClip.
-- You won't use them raw, like this. Instead, each
-- movieclip can access these attributes through lenses.
data Attributes = Attributes {
                    -- | x position
                    _ax :: Float,
                    -- | y position
                    _ay :: Float,
                    -- | scale
                    _ascaleX :: Float,
                    _ascaleY :: Float,
                    -- | visibility
                    _avisible :: Bool,
                    -- | when this gets drawn. Note: unless you use
                    -- the `displayAll` function, you have to handle
                    -- zindex yourself!
                    _azindex :: Int
}

makeLenses ''Attributes

-- | default Attributes.
def = Attributes 0.0 0.0 1.0 1.0 True 1

-- | Make your data type an instance of this class.
-- For example, suppose you have a data type like this:
--
-- > data Tile = Tile { _tileAttrs :: Attributes, _tileColor :: Color }
--
-- You can make it into a MovieClip like this:
--
-- > makeLenses ''Tile
-- > instance MovieClip GameState where
-- >    attrs = tileAttrs
-- >    render (Tile a c) = color c $ circle 5
--
-- `attrs` is a lens for the attributes of this MovieClip.
-- `render` just defines how you want to render this data type.
--
-- Now, you can use the attributes like this:
--
-- > tile = Tile defaultAttrs blue
-- > newTile = x +~ 10 $ tile -- tile, moved to the right by 10
--
-- The coordinate system starts from the bottom left.
-- So the bottom-left is (0,0).
class MovieClip a where
    -- | your data type needs to have a field for attributes.
    -- This is a lens for that field. You can build your own lens:
    --
    -- > data Tile = Tile { tileAttrs :: Attributes }
    -- > attrs = lens tileAttrs (\mc newAttrs -> mc { tileAttrs = newAttrs })
    --
    -- Basically, the `lens` function takes:
    --
    -- - A function to *get* the attributes of your tile
    --
    -- - A function to *set* the attributes of your tile.
    --
    -- It's even easier if you just let the lens library do everything:
    --
    -- > data Tile = Tile { _tileAttrs :: Attributes }
    -- > makeLenses ''Tile
    -- > attrs = tileAttrs
    attrs :: Lens a a Attributes Attributes

    -- | This is how your data type actually gets rendered.
    -- ActionKid will take care of positioning, scaling etc for you.
    -- All you need to define is how the type should be rendered.
    -- Example:
    --
    -- > render tile = color blue $ circle 5
    render :: a -> Picture

    -- | These are the different lenses you can use on your data type
    -- after you make it an instance of MovieClip.
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

    -- | This is the internal function that positions the MovieClip
    -- correctly, checks if it is visible, etc etc. Override this at your
    -- own peril.
    display :: a -> Picture
    display mc
      | mc ^. visible = translate (mc ^. x) (mc ^. y) $
                        scale (mc ^. scaleX) (mc ^. scaleY) $
                        render mc
      | otherwise = blank
