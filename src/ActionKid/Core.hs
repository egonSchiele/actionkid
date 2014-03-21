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
import qualified Debug.Trace as D

--------------------------------------------------------------------------------

-- bounding box for a series of points
pathBox points =
    let minx = minimum $ map fst points
        miny = minimum $ map snd points
        maxx = maximum $ map fst points
        maxy = maximum $ map snd points
    in ((minx, miny), (maxx, maxy))

catPoints :: (Point, Point) -> [Point]
catPoints (p1, p2) = [p1, p2]

-- | Code borrowed from https://hackage.haskell.org/package/gloss-game-0.3.0.0/docs/src/Graphics-Gloss-Game.html
type Rect = (Point, Point)     -- ^origin & extent, where the origin is at the centre
boundingBox :: Picture -> Rect
boundingBox Blank                    = ((0, 0), (0, 0))
boundingBox (Polygon path)           = pathBox path
boundingBox (Line path)              = pathBox path
boundingBox (Circle r)               = ((0, 0), (2 * r, 2 * r))
boundingBox (ThickCircle t r)        = ((0, 0), (2 * r + t, 2 * r + t))
boundingBox (Arc _ _ _)              = error "ActionKid.Core.boundingbox: Arc not implemented yet"
boundingBox (ThickArc _ _ _ _)       = error "ActionKid.Core.boundingbox: ThickArc not implemented yet"
boundingBox (Text _)                 = error "ActionKid.Core.boundingbox: Text not implemented yet"
boundingBox (Bitmap w h _ _)         = ((0, 0), (fromIntegral w, fromIntegral h))
boundingBox (Color _ p)              = boundingBox p
boundingBox (Translate dx dy p)      = ((x1 + dx, y1 + dy), (x2 + dx, y2 + dy))
    where ((x1, y1), (x2, y2)) = boundingBox p
boundingBox (Rotate _ang _p)         = error "Graphics.Gloss.Game.boundingbox: Rotate not implemented yet"

-- TODO fix scale, this implementation is incorrect (only works if scale
-- = 1). Commented out version is incorrect too
boundingBox (Scale xf yf p)          = boundingBox p
    -- let ((x1, y1), (x2, y2)) = boundingBox p
    --     w = x2 - x1
    --     h = y2 - y1
    --     scaledW = w * xf
    --     scaledH = h * yf
    -- in ((x1, x2), (x1 + scaledW, y1 + scaledH))
boundingBox (Pictures ps) = pathBox points
    where points = concatMap (catPoints . boundingBox) ps

hits :: MovieClip a => a -> a -> Bool
hits a b = f a `intersects` f b
    where f = boundingBox . display

intersects :: Rect -> Rect -> Bool
intersects ((min_ax, min_ay), (max_ax, max_ay)) ((min_bx, min_by), (max_bx, max_by))
    | max_ax < min_bx = False
    | min_ax > max_bx = False
    | min_ay > max_by = False
    | max_ay < min_by = False
    | otherwise = True

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
