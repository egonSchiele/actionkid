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

onEnterFrame :: MovieClip a => (Float -> [a] -> IO [a]) -> Float -> [a] -> IO [a]
onEnterFrame stepFunc num state = do
    -- update x value from moveX
    let newState = for state $ \mc -> x +~ (mc ^. moveX) $
                                      y +~ (mc ^. moveY) $ mc
    stepFunc num newState

run :: MovieClip a => String -> (Int, Int) -> [a] -> (Event -> [a] -> IO [a]) -> (Float -> [a] -> IO [a]) -> IO ()
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

draw :: MovieClip a => [a] -> IO Picture
draw state = liftM mconcat $ mapM display . sortBy (comparing $ azindex . getAttrs) $ state
