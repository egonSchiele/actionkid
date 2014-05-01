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
import Data.StateVar
import Control.Lens
import qualified Debug.Trace as D
import ActionKid.Internal
import Control.Monad.State
import Language.Haskell.TH

-- instance MovieClip GameState where
--     attrs = ga
--     render gs = displayAll (_tiles gs)

-- instance MovieClip Tile where
--     attrs = lens viewer mutator
--       where viewer mc = _ga mc
--             mutator mc new = mc {_ga = new}
--     render gs = color blue $ circle 10

deriveMC :: Name -> Q [Dec]
deriveMC name = do
    TyConI (DataD _ _ _ [RecC _ fields] _) <- reify name
 
    let names = map (\(name,_,_) -> name) fields
        lastField = last names
 
        -- showField :: Name -> Q Exp
        -- showField name = [|\x -> s ++ " = " ++ show ($(global name) x)|] where
        --     s = nameBase name
 
        -- showFields :: Q Exp
        -- showFields = listE $ map showField names
 
    -- dont hardcode the field in the mutator. See:
    -- http://stackoverflow.com/questions/8469044/template-haskell-with-record-field-name-as-variable
    [d|instance MovieClip $(conT name) where
         attrs = lens viewer mutator
           where viewer mc = $(global lastField) mc
                 mutator mc new = $(recUpdE [| mc |] [fieldExp lastField [| new |]])
                 -- mutator mc new = mc { $(global lastField) = new }
         render gs = color blue $ circle 10|]
    -- [d|instance Show $(conT name) where show x = intercalate ", " (map ($ x) $showFields)|]

-- Prelude Language.Haskell.TH> runQ [| adit { name = "maggie" } |]
-- RecUpdE (VarE adit_1627395593) [(:Interactive.name,LitE (StringL "maggie"))]

-- makeChange :: Name -> Q Exp
-- makeChange x = [|
--     \z -> Record $ \s -> ( $(recUpdE [| s |] [fieldExp x [| z |]]), () ) |]

-- changeBeta :: Double -> Record ()
-- changeBeta x = $(makeChange 'beta) x


-- | Given a 2d array, returns a array of movieclips that make up a
-- grid of tiles. Takes:
--
-- 1. A 2d array of ints
-- 2. A function that takes an int and returns the related movieclip.
-- 3. (width, height) for the tiles
renderTileMap :: MovieClip a => [[Int]] -> (Int -> a) -> (Int, Int) -> [a]
renderTileMap tileMap f (w,h) =
    concat $ forWithIndex tileMap $ \(row, j) ->
         forWithIndex row $ \(tile, i) ->
            with (f tile) $ do
              x .= (fromIntegral $ i*w)
              y .= (fromIntegral $ j*h)

-- | hittest. Check if one MovieClip is hitting another.
hits :: MovieClip a => a -> a -> Bool
hits a b = f a `intersects` f b
    where f = boundingBox . display

-- | Call this to run your game. Takes:
--
-- 1. Window title
--
-- 2. (width, height)
--
-- 3. Game state (a MovieClip)
--
-- 4. a key handler function (exactly the same as Gloss)
--
-- 5. a step function (onEnterFrame)
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

-- | Convenience function. Given a list of movie clips,
-- displays all of them.
-- TODO support zindex.
displayAll :: MovieClip a => [a] -> Picture
displayAll mcs = Pictures $ map display mcs
