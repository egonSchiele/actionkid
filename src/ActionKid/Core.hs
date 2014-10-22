{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

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
import Control.Lens
import qualified Debug.Trace as D
import ActionKid.Internal
import Control.Monad.State
import Language.Haskell.TH
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import Graphics.Gloss.Juicy
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent
import Foreign.ForeignPtr
import Graphics.Rendering.OpenGL.GL.StateVar

-- | (Currently disabled) Given a path to an audio file, plays the file.
playSound :: String -> Bool -> IO ()
playSound src loopSound = return ()
-- Needs some love...either SDL is buggy or I don't understand it...
-- playSound src loopSound = do
--   let audioRate     = 22050
--       audioFormat   = Mix.AudioS16LSB
--       audioChannels = 2
--       audioBuffers  = 4096
--       anyChannel    = (-1)

--   forkOS $ do
--     -- Don't ask why this is needed. If it isn't there, somehow
--     -- this audio thread blocks all other execution, and you can't
--     -- do anything else. But introducing it somehow prevents that.
--     -- WTF.
--     threadDelay 5000
--     SDL.init [SDL.InitAudio]
--     result <- openAudio audioRate audioFormat audioChannels audioBuffers
--     audioData <- Mix.loadWAV src
--     Mix.playChannel anyChannel audioData 0
--     fix $ \loop -> do
--       touchForeignPtr audioData
--       threadDelay 500000
--       stillPlaying <- numChannelsPlaying
--       when (stillPlaying /= 0) loop
--     Mix.closeAudio
--     SDL.quit
--     when loopSound $
--       playSound src loopSound
--     return ()
--   return ()

-- cacheImage src pic = unsafePerformIO $ do
--   modifyIORef' imageCache (\cache -> D.trace ("caching: " ++ src) $ M.insert src pic cache)
--   cache <- readIORef imageCache
--   putStrLn $ "new cache is: " ++ (show cache)
--   return pic

-- | Given a path, loads the image and returns it as a picture. It performs
-- caching, so if the same path has been given before, it will just return
-- the image from the cache.
image :: String -> Picture
image src = unsafePerformIO $ do
    pic_ <- loadJuicy src
    case pic_ of
      Nothing -> error $ "didn't find an image at " ++ src
      Just pic@(Bitmap w h _ _) -> do
        let x = fromIntegral w / 2
            y = fromIntegral h / 2
            newPic = translate x y pic
        cache <- readIORef imageCache
        case M.lookup src cache of
          Nothing -> do
            modifyIORef' imageCache (M.insert src newPic)
            return newPic
          Just cachedPic -> do
            return cachedPic

{-# NOINLINE image #-}

-- This will eventually be a function that takes a tile map png or jpg and
-- cuts it up into the individual tiles and returns them as a 2-d array.
-- http://hackage.haskell.org/package/vector-0.5/docs/Data-Vector-Unboxed.html
-- http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial#Indexing_vectors
-- http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Repa_Tutorial#Indexing_arrays
-- http://hackage.haskell.org/package/repa-3.2.3.3/docs/Data-Array-Repa.html#t:Array
-- loadTileMap :: String -> Int -> Int -> [[Picture]]
-- loadTileMap src w h =
--    where image = (fromRight . unsafePerformIO . readImage $ src) :: Img RGBA
--          vec   = toUnboxed image

-- > :t vec
-- vec :: U.Vector GHC.Word.Word8
--
-- vec U.! 14 => gives you a number (word8)
-- U.length vec == 4096 (4 channels, RGBA, so really 1024...and it's
-- a 32x32 image. 32x32 = 1024).


-- | Convenience function to make `MovieClip` instances for your data types.
-- Suppose you have a data type like so:
--
-- > data Tile = Empty { _ea :: Attributes } | Wall  { _wa :: Attributes } | Chip  { _ca :: Attributes }
--
-- Just call:
--
-- > deriveMC ''Tile
--
-- Or you can write your instance by hand if you want. Something like this:
-- https:\/\/gist.github.com\/egonSchiele\/e692421048cbd79acb26
deriveMC :: Name -> Q [Dec]
deriveMC name = do
    TyConI (DataD _ _ _ records _) <- reify name

    -- The following answers helped a lot:
    -- http://stackoverflow.com/questions/8469044/template-haskell-with-record-field-name-as-variable
    -- http://stackoverflow.com/questions/23400203/multiple-function-definitions-with-template-haskell
    [d|instance MovieClip $(conT name) where
         attrs = lens viewer mutator
           where viewer = $(mkViewer records)
                 mutator = $(mkMutator records)|]

-- | Used internally. Generates something like \mc -> case mc of ...
mkViewer :: [Con] -> Q Exp
mkViewer records = return $ LamE [VarP mc] (CaseE (VarE mc) $ map (mkMatch mc) records)
  where mc = mkName "mc"

-- | Used internally. Generates something like \mc new -> case mc of ...
mkMutator :: [Con] -> Q Exp
mkMutator records = return $ LamE [VarP mc, VarP new] (CaseE (VarE mc) $ map (mkMutatorMatch mc new) records)
  where mc = mkName "mc"
        new = mkName "new"

-- | Used internally by the `mkViewer` function to make all the cases
mkMatch :: Name -> Con -> Match
mkMatch mc (RecC n fields) = Match (ConP n (take (length fields) $ repeat WildP)) (NormalB body) []
    where lastField = last $ map (\(name,_,_) -> name) fields
          body = AppE (VarE lastField) (VarE mc)

-- THis works with data types without names for fields, but mkMutatorMatch
-- doesn't work yet
mkMatch mc (NormalC n fields) = Match (ConP n ((take ((length fields) - 1) $ repeat WildP) ++ [VarP mcAttrs])) (NormalB $ VarE mcAttrs) []
    where mcAttrs = mkName "mcAttrs"

-- | Used internally by the `mkMutator` function to make all the cases
mkMutatorMatch :: Name -> Name -> Con -> Match
mkMutatorMatch mc new (RecC n fields) = Match (ConP n (take (length fields) $ repeat WildP)) (NormalB body) []
    where lastField = last $ map (\(name,_,_) -> name) fields
          body = RecUpdE (VarE mc) [(lastField, VarE new)]

mkMutatorMatch mc new (NormalC n fields) = Match (ConP n (take (length fields) $ repeat WildP)) (NormalB body) []
    where lastField = last $ map fst fields
          body = AppE (ConE n) (VarE new) -- NOTE: this only works for data types with only one attribute: Attributes. Like `data Color = Red Attributes`. Make it work for more than one.

-- | Given a 2d array, returns a array of movieclips that make up a
-- grid of tiles. Takes:
--
-- 1. A 2d array of ints
--
-- 2. A function that takes an int and returns the related `MovieClip`
--
-- 3. (width, height) for the tiles in pixels
renderTileMap :: MovieClip a => [[Int]] -> (Int -> a) -> (Int, Int) -> [a]
renderTileMap tileMap f (w,h) =
    concat $ forWithIndex tileMap $ \(row, j) ->
         forWithIndex row $ \(tile, i) ->
            withMC (f tile) $ do
              x .= (fromIntegral $ i*w)
              y .= (fromIntegral $ boardH - h - j*h)

    where boardH = (*h) . length $ tileMap

-- | Check if one `MovieClip` is hitting another. Example:
--
-- > when player `hits` enemy $ die
hits :: Renderable a => a -> a -> Bool
hits a b = f a `intersects` f b
    where f = boundingBox . display

-- | Call this to run your game. Takes:
--
-- 1. Window title
--
-- 2. (width, height) of window
--
-- 3. Game state (a `MovieClip`)
--
-- 4. an event handler function (for handling user input)
--
-- 5. a function that keeps getting called in a loop (the main game loop)
run :: (MovieClip a, Renderable a) => String -> (Int, Int) -> a -> (Event -> StateT a IO ()) -> (Float -> StateT a IO ()) -> IO ()
run title (w,h) state keyHandler stepFunc = do
  boardWidth $= w
  boardHeight $= h
  playIO
    (InWindow title (w,h) (1, 1))
    white
    30
    state
    draw
    (\k gs -> execStateT (keyHandler k) gs)
    (\i gs -> execStateT (stepFunc i) gs)


-- | Convenience function. Given a list of movie clips,
-- displays all of them.
-- TODO support zindex.
displayAll :: Renderable a => [a] -> Picture
displayAll mcs = Pictures $ map display mcs
