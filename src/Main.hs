{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss hiding (display)
import qualified Debug.Trace as D
import Data.Maybe
import Codec.Picture hiding (readImage)
import Codec.Picture.Repa
import qualified Data.Vector.Unboxed as U
import Data.IORef
import ActionKid.Globals
import qualified Data.Map as M
import Control.Seq
import System.IO.Unsafe
import Graphics.Gloss.Juicy

-- | Given a path, loads the image and returns it as a picture. It performs 
-- caching, so if the same path has been given before, it will just return
-- the image from the cache. This makes this function easily usable in 
-- `render`...you don't have to worry about the image getting loaded into 
-- memory multiple times. By my testing, this actually worked, and memory
-- didn't increase. Before the caching, it WAS reading images into memory 
-- multiple times...leading to massive memory use.
image :: String -> Picture
image src = case M.lookup src (unsafePerformIO . readIORef $ imageCache) of
              -- force evaluation of the first part, so the image gets 
              -- cached in the imageCache, before returning the read image.
              Nothing -> (unsafePerformIO $ modifyIORef imageCache (M.insert src newPic)) `seq` newPic
              Just cachedPic -> cachedPic
    where pic@(Bitmap w h _ _) = fromJust . unsafePerformIO . loadJuicy $ src
          newPic = translate x y pic
          x = fromIntegral w / 2
          y = fromIntegral h / 2

{-# NOINLINE image #-}

tileSize = 32

data Tile = Empty Attributes
          | Wall  Attributes
          | Chip  Attributes
          | KeyYellow Attributes
          | KeyRed Attributes
          | KeyGreen Attributes
          | KeyBlue Attributes
          | LockYellow Attributes
          | LockRed Attributes
          | LockGreen Attributes
          | LockBlue Attributes
          | Gate Attributes
          | GateFinal Attributes
          | Help Attributes
          deriving Show

deriveMC ''Tile

instance Renderable Tile where
    render (Empty _)      = image "images/empty.png"
    render (Wall _)       = image "images/wall.png"
    render (Chip _)       = image "images/chip.png"
    render (KeyYellow _)  = image "images/key_yellow.png"
    render (KeyRed _)     = image "images/key_red.png"
    render (KeyGreen _)   = image "images/key_green.png"
    render (KeyBlue _)    = image "images/key_blue.png"
    render (LockYellow _) = image "images/lock_yellow.png"
    render (LockRed _)    = image "images/lock_red.png"
    render (LockGreen _)  = image "images/lock_green.png"
    render (LockBlue _)   = image "images/lock_blue.png"
    render (Gate _)       = image "images/gate.png"
    render (GateFinal _)  = image "images/gate_final1.png"
    render (Help _)       = image "images/help.png"

data Direction = DirUp | DirDown | DirLeft | DirRight

data Player = Player {
                _direction :: Direction,
                _ar :: Attributes
}

makeLenses ''Player
deriveMC ''Player

instance Renderable Player where
    render p = case p ^. direction of
                 DirUp    -> image "images/player_up.png"
                 DirDown  -> image "images/player_down.png"
                 DirLeft  -> image "images/player_left.png"
                 DirRight -> image "images/player_right.png"

data GameState = GameState {
                    _tiles :: [Tile],
                    _player :: Player,
                    _redKeyCount :: Int,
                    _blueKeyCount :: Int,
                    _yellowKeyCount :: Int,
                    _hasGreenKey :: Bool,
                    _ga :: Attributes
}

makeLenses ''GameState
deriveMC ''GameState

instance Renderable GameState where
    render gs = displayAll (_tiles gs) <> display (_player gs)

tileMap = 
    [[1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
     [1, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 1, 1],
     [1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1],
     [1, 1, 1, 2, 1, 3, 1, 2, 13, 2, 1, 3, 1, 2, 1, 1, 1],
     [1, 2, 2, 2, 2, 2, 10, 2, 12, 2, 10, 2, 2, 2, 2, 2, 1],
     [1, 2, 1, 4, 1, 11, 1, 1, 1, 1, 1, 9, 1, 4, 1, 2, 1],
     [1, 2, 1, 3, 1, 2, 7, 1, 14, 1, 5, 2, 1, 3, 1, 2, 1],
     [1, 2, 2, 2, 2, 2, 3, 1, 1, 1, 3, 2, 2, 2, 2, 2, 1],
     [1, 2, 1, 3, 1, 2, 7, 1, 1, 1, 5, 2, 1, 3, 1, 2, 1],
     [1, 2, 1, 1, 1, 9, 1, 1, 3, 1, 1, 11, 1, 1, 1, 2, 1],
     [1, 2, 2, 2, 2, 2, 2, 8, 2, 8, 2, 2, 2, 2, 2, 2, 1],
     [1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 2, 1, 3, 2, 3, 1, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 2, 1, 1, 2, 6, 1, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1],
     [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]]

boardW = length . head $ tileMap
boardH = length tileMap

playerCoords :: GameState -> (Int, Int)
playerCoords gs = ((floor (p ^. x)) // tileSize, (((boardH * tileSize) - (floor (p ^. y))) // tileSize)-1)
    where p = gs ^. player
          ts = gs ^. tiles

currentIdx :: GameState -> Int
currentIdx gs = y_ * boardW + x_
    where (x_,y_) = playerCoords gs

currentTile gs = (_tiles gs) !! (currentIdx gs)
leftTile gs    = (_tiles gs) !! (currentIdx gs - 1)
rightTile gs   = (_tiles gs) !! (currentIdx gs + 1)
upTile gs      = (_tiles gs) !! (currentIdx gs - boardW)
downTile gs    = (_tiles gs) !! (currentIdx gs + boardW)

renderedTiles = renderTileMap tileMap f (tileSize, tileSize)
    where f 1 = Empty def
          f 2 = Wall def
          f 3 = Chip def
          f 4 = KeyYellow def
          f 5 = KeyRed def
          f 6 = KeyGreen def
          f 7 = KeyBlue def
          f 8 = LockYellow def
          f 9 = LockRed def
          f 10 = LockGreen def
          f 11 = LockBlue def
          f 12 = Gate def
          f 13 = GateFinal def
          f 14 = Help def

gameState = GameState renderedTiles (x .~ (8*tileSize) $ y .~ (8*tileSize) $ player_) 0 0 0 False def
        where player_ = (Player DirDown def)

main = run "chips challenge" (boardW * tileSize, boardH * tileSize) gameState on stepGame

checkWall func gs newGs = case func gs of
                      Wall _ -> gs
                      _ -> newGs

checkLock func gs newGs = case func gs of
                            LockRed _    -> if _redKeyCount gs > 0 then newGs else gs
                            LockBlue _   -> if _blueKeyCount gs > 0 then newGs else gs
                            LockGreen _  -> if _hasGreenKey gs then newGs else gs
                            LockYellow _ -> if _yellowKeyCount gs > 0 then newGs else gs
                            _ -> newGs

on (EventKey (SpecialKey KeyLeft) Down _ _) gs = return $ checkLock leftTile gs . checkWall leftTile gs $ move
    where move = player.direction .~ DirLeft
                 $ player.x -~ tileSize
                 $ gs

on (EventKey (SpecialKey KeyRight) Down _ _) gs = return $ checkLock rightTile gs . checkWall rightTile gs $ move
    where move = player.direction .~ DirRight
                 $ player.x +~ tileSize
                 $ gs

on (EventKey (SpecialKey KeyUp) Down _ _) gs = return $ checkLock upTile gs . checkWall upTile gs $ move
    where move = player.direction .~ DirUp
                 $ player.y +~ tileSize
                 $ gs

on (EventKey (SpecialKey KeyDown) Down _ _) gs = return $ checkLock downTile gs . checkWall downTile gs $ move
    where move = player.direction .~ DirDown
                 $ player.y -~ tileSize
                 $ gs

on (EventKey (SpecialKey KeySpace) Down _ _) gs = return gameState
on _ gs = return $ player.direction .~ DirDown $ gs

stepGame _ gs = do
    let playerIx = currentIdx gs
    let attrs_ = ((gs ^. tiles) !! playerIx) ^. attrs
    let resetTile i = tiles.(ix i) .~ (Empty attrs_) $ gs
    case currentTile gs of
        Chip _ -> return $ resetTile playerIx
        KeyYellow _ -> return $ yellowKeyCount +~ 1 $ resetTile playerIx
        KeyBlue _ -> return $ blueKeyCount +~ 1 $ resetTile playerIx
        KeyGreen _ -> return $ hasGreenKey .~ True $ resetTile playerIx
        KeyRed _ -> return $ redKeyCount +~ 1 $ resetTile playerIx
        LockYellow _ -> return $ yellowKeyCount -~ 1 $ resetTile playerIx
        LockBlue _ -> return $ blueKeyCount -~ 1 $ resetTile playerIx
        LockGreen _ -> return $ resetTile playerIx
        LockRed _ -> return $ redKeyCount -~ 1 $ resetTile playerIx
        _ -> return gs
