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

--------------------------------------------------------------------------------

play :: MovieClip a => String -> (Int, Int) -> [a] -> (Event -> [a] -> IO [a]) -> (Float -> [a] -> IO [a]) -> IO ()
play title size state keyHandler onEnterFrame = do
  playIO
    (InWindow title size (1, 1))
    white
    30
    state
    -- this could be done through a pre-defined function too...
    -- just need to make the gamestate be a global var that is always
    -- a list of elements to display
    (draw size)
    keyHandler
    onEnterFrame

draw :: MovieClip a => (Int, Int) -> [a] -> IO Picture
draw size state = return . mconcat $ map (display size) . sortBy (comparing zindex) $ state
