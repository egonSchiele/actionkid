module ActionKid.Core where
import ActionKid.Types
import ActionKid.Utils
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Text.Printf
import Graphics.Gloss hiding (display)
import Data.Monoid ((<>), mconcat)
import Graphics.Gloss.Interface.IO.Game
import Data.Ord
import ActionKid.Globals
import Data.StateVar
import Control.Lens
--------------------------------------------------------------------------------

-- | Code borrowed from https://hackage.haskell.org/package/gloss-game-0.3.0.0/docs/src/Graphics-Gloss-Game.html
-- type Size = (Float, Float)    -- ^width & height
-- type Rect = (Point, Size)     -- ^origin & extent, where the origin is at the centre
-- boundingBox :: Picture -> Rect
-- boundingBox G.Blank                    = ((0, 0), (0, 0))
-- boundingBox (G.Polygon _)              = error "Graphics.Gloss.Game.boundingbox: Polygon not implemented yet"
-- boundingBox (G.Line _)                 = error "Graphics.Gloss.Game.boundingbox: Line not implemented yet"
-- boundingBox (G.Circle r)               = ((0, 0), (2 * r, 2 * r))
-- boundingBox (G.ThickCircle t r)        = ((0, 0), (2 * r + t, 2 * r + t))
-- boundingBox (G.Arc _ _ _)              = error "Graphics.Gloss.Game.boundingbox: Arc not implemented yet"
-- boundingBox (G.ThickArc _ _ _ _)       = error "Graphics.Gloss.Game.boundingbox: ThickArc not implemented yet"
-- boundingBox (G.Text _)                 = error "Graphics.Gloss.Game.boundingbox: Text not implemented yet"
-- boundingBox (G.Bitmap w h _ _)         = ((0, 0), (fromIntegral w, fromIntegral h))
-- boundingBox (G.Color _ p)              = boundingBox p
-- boundingBox (G.Translate dx dy p)      = let ((x, y), size) = boundingBox p in ((x + dx, y + dy), size)
-- boundingBox (G.Rotate _ang _p)         = error "Graphics.Gloss.Game.boundingbox: Rotate not implemented yet"
-- boundingBox (G.Scale xf yf p)          = let (origin, (w, h)) = boundingBox p in (origin, (w * xf, h * yf))
-- boundingBox (G.Pictures _ps)           = error "Graphics.Gloss.Game.boundingbox: Pictures not implemented yet"

onEnterFrame :: MovieClip a => (Float -> a -> IO a) -> Float -> a -> IO a
onEnterFrame stepFunc num state = stepFunc num state

run :: MovieClip a => String -> (Int, Int) -> a -> (Event -> a -> IO a) -> (Float -> a -> IO a) -> IO ()
run title (w,h) state keyHandler stepFunc = do
  boardWidth $= w
  boardHeight $= h
  playIO
    (InWindow title (w,h) (1, 1))
    white
    30
    state
    -- this could be done through a pre-defined function too...
    -- just need to make the gamestate be a global var that is always
    -- a list of elements to display
    draw
    keyHandler
    (onEnterFrame stepFunc)

draw gs = do
  w <- get boardWidth
  h <- get boardHeight
  return $ translate (-(fromIntegral $ w // 2)) (-(fromIntegral $ h // 2)) $
           display gs
