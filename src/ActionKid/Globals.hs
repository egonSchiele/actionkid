module ActionKid.Globals where
import Data.IORef
import System.IO.Unsafe

-- | Global variable to set board width
boardWidth :: IORef Int
boardWidth  = unsafePerformIO $ newIORef 0

-- | Global variable to set board height
boardHeight :: IORef Int
boardHeight = unsafePerformIO $ newIORef 0
