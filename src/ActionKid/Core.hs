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

play :: MovieClip a => String -> [a] -> (Event -> [a] -> IO [a]) -> (Float -> [a] -> IO [a]) -> IO ()
play title state keyHandler onEnterFrame = do
  playIO
    (InWindow "ones" (500, 500) (1, 1))
    white
    30
    state
    -- this could be done through a pre-defined function too...
    -- just need to make the gamestate be a global var that is always
    -- a list of elements to display
    draw
    keyHandler
    onEnterFrame

draw :: MovieClip a => [a] -> IO Picture
draw state = return . mconcat $ map display . sortBy (comparing zindex) $ state
