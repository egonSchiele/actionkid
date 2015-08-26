module ActionKid.Globals where
import           Control.Concurrent
import           Data.IORef
import qualified Data.Map           as M
import qualified Graphics.Gloss     as G
import           System.IO.Unsafe

-- | Set the IORef to the actual int
setRef :: a -> IORef a
setRef = unsafePerformIO . newIORef

-- | Global variable to set board width
boardWidth :: IORef Int
boardWidth  = setRef 0

-- | Global variable to set board height
boardHeight :: IORef Int
boardHeight = setRef 0

-- | Global variable to cache game images
imageCache :: IORef (M.Map String G.Picture)
imageCache = setRef M.empty
