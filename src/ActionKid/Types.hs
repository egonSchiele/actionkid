{-# LANGUAGE TemplateHaskell #-}
-- | Each game has several objects. For example, Mario has Mario, goombas,
-- mushrooms etc. In ActionKid terminology, an object is called
-- a `MovieClip`. To use one of your data types in a game, it needs to be
-- an instance of two typeclasses `MovieClip` and `Renderable`.
--
-- The `MovieClip` class does the book-keeping for an object. What are it's
-- x and y position? scale? visibility?
--
-- The `Renderable` class defines how your object will be rendered on
-- screen.
--
-- A `MovieClip` has several `Attributes`: x and y position, x and y scale, etc. So when you create a data type, the last
-- field needs to be of type `Attributes`:
--
-- > data Player = Player { _name :: String, _attrs :: Attributes }
--
-- Then, ActionKid takes care of rendering the player on screen correctly.
-- So you can write code like this:
--
-- > player.x += 10
--
-- And that updates the x field on the player's attributes. Then ActionKid will
-- make sure the player's position gets updated on-screen.

module ActionKid.Types where
import Graphics.Gloss.Interface.IO.Game
import ActionKid.Utils
import ActionKid.Globals
import Data.StateVar
import Control.Monad hiding (join)
import Control.Lens
import System.IO.Unsafe
import qualified Debug.Trace as D

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
} deriving (Show, Eq)

makeLenses ''Attributes

-- | default Attributes.
-- Every `MovieClip` will have `Attributes` has it's last field.
-- When you create an instance of that movieclip, use the `def` function
-- to specify default attributes. Example: suppose you have a data type
-- like so:
--
-- > data Player = Player { _name :: String, _attrs :: Attributes }
--
-- You can instantiate a player like this:
--
-- > adit = Player "adit" def
--
-- Note that you don't need to specify x or y coordinates for the player...
-- that's what the attributes are for.
def = Attributes 0.0 0.0 1.0 1.0 True 1

-- | Make your data type an instance of this class.
-- For example, suppose you have a data type like this:
--
-- > data Tile = Tile { _tileAttrs :: Attributes, _tileColor :: Color }
--
-- Before you can use Tile in a game, it needs to be an instance of
-- `MovieClip`. You can use `deriveMC` to do this automatically using
-- TemplateHaskell:
--
-- > deriveMC ''Tile
--
-- Or write it yourself: https:\/\/gist.github.com\/egonSchiele\/e692421048cbd79acb26
class MovieClip a where
    -- | your data type needs to have a field for attributes.
    -- This is a lens for that field. For example, in our above example
    -- of a player, you can get the player's attributes like this:
    --
    -- > player ^. attrs
    --
    -- You can also use the rest of the following lenses on the player:
    --
    -- Get a player's position with:
    --
    -- > player ^. x
    --
    -- Set a player's position with:
    --
    -- > x .~ 10 $ player
    --
    -- The lens library gives you all kinds of pretty syntax for this
    -- stuff.
    attrs :: Lens a a Attributes Attributes

    x :: Lens a a Float Float
    x = lens (view $ attrs . ax) (flip $ set (attrs . ax))

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

-- | Before you can use your data type in a game, it also needs to be an
-- instance of `Renderable`. This class defines how your data type will
-- look on the screen.
class MovieClip a => Renderable a where
    -- | Implement this method to tell ActionKid how to render your data
    -- type. Returns an instance of Picture (from the Gloss package).
    -- Example:
    --
    -- > render tile = color blue $ circle 5
    render :: a -> Picture
    -- | This is the internal function that positions the MovieClip
    -- correctly, checks if it is visible, etc etc.
    -- DO NOT IMPLEMENT!
    display :: a -> Picture
    display mc
      | mc ^. visible = translate (mc ^. x) (mc ^. y) $
                        scale (mc ^. scaleX) (mc ^. scaleY) $
                        render mc
      | otherwise = D.trace "not rendering invisible movieclip" blank
