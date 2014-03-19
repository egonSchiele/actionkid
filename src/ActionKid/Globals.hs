module ActionKid.Globals where
import Data.IORef
import System.IO.Unsafe

boardWidth :: IORef Int
boardWidth  = unsafePerformIO $ newIORef 0

boardHeight :: IORef Int
boardHeight = unsafePerformIO $ newIORef 0
