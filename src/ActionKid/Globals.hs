module ActionKid.Globals where
import Data.IORef
import System.IO.Unsafe
import qualified Data.Map as M
import qualified Graphics.Gloss as G

-- | Global variable to set board width
boardWidth :: IORef Int
boardWidth  = unsafePerformIO $ newIORef 0

-- | Global variable to set board height
boardHeight :: IORef Int
boardHeight = unsafePerformIO $ newIORef 0

-- | Global variable to cache game images
imageCache :: IORef (M.Map String G.Picture)
imageCache = unsafePerformIO $ newIORef M.empty
