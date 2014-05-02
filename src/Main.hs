{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss hiding (display)
import qualified Debug.Trace as D
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import Data.Maybe
import Codec.Picture hiding (readImage)
import Codec.Picture.Repa
import qualified Data.Vector.Unboxed as U

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

empty = Empty def
wall  = Wall def
chip  = Chip def
keyYellow = KeyYellow def
keyRed = KeyRed def
keyGreen = KeyGreen def
keyBlue = KeyBlue def
lockYellow = LockYellow def
lockRed = LockRed def
lockGreen = LockGreen def
lockBlue = LockBlue def
gate = Gate def
gateFinal = GateFinal def
help = Help def

deriveMC ''Tile

fromRight (Right x) = x

data Direction = DirUp | DirDown | DirLeft | DirRight

data Player = Player {
                _direction :: Direction,
                _ar :: Attributes
}

makeLenses ''Player
deriveMC ''Player

image :: String -> Picture
image src = translate x y pic
    where pic@(Bitmap w h _ _) = fromJust . unsafePerformIO . loadJuicy $ src
          x = fromIntegral w / 2
          y = fromIntegral h / 2

{-# NOINLINE image #-}

data GameState = GameState {
                    _tiles :: [Tile],
                    _player :: Player,
                    _ga :: Attributes
}

makeLenses ''GameState
deriveMC ''GameState

emptyPng = image "images/empty.png"
wallPng = image "images/wall.png"
chipPng = image "images/chip.png"
key_yellowPng = image "images/key_yellow.png"
key_redPng = image "images/key_red.png"
key_greenPng = image "images/key_green.png"
key_bluePng = image "images/key_blue.png"
lock_yellowPng = image "images/lock_yellow.png"
lock_redPng = image "images/lock_red.png"
lock_greenPng = image "images/lock_green.png"
lock_bluePng = image "images/lock_blue.png"
gatePng = image "images/gate.png"
gate_final1Png = image "images/gate_final1.png"
helpPng = image "images/help.png"

instance Renderable Tile where
    render (Empty _)      = emptyPng
    render (Wall _)       = wallPng
    render (Chip _)       = chipPng
    render (KeyYellow _)  = key_yellowPng
    render (KeyRed _)     = key_redPng
    render (KeyGreen _)   = key_greenPng
    render (KeyBlue _)    = key_bluePng
    render (LockYellow _) = lock_yellowPng
    render (LockRed _)    = lock_redPng
    render (LockGreen _)  = lock_greenPng
    render (LockBlue _)   = lock_bluePng
    render (Gate _)       = gatePng
    render (GateFinal _)  = gate_final1Png
    render (Help _)       = helpPng

instance Renderable GameState where
    render gs = displayAll (_tiles gs) <> display (_player gs)

instance Renderable Player where
    render p = case p ^. direction of
                 DirUp    -> image "images/player_up.png"
                 DirDown  -> image "images/player_down.png"
                 DirLeft  -> image "images/player_left.png"
                 DirRight -> image "images/player_right.png"

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
    where f 1 = empty
          f 2 = wall
          f 3 = chip
          f 4 = keyYellow
          f 5 = keyRed
          f 6 = keyGreen
          f 7 = keyBlue
          f 8 = lockYellow
          f 9 = lockRed
          f 10 = lockGreen
          f 11 = lockBlue
          f 12 = gate
          f 13 = gateFinal
          f 14 = help

gameState = GameState renderedTiles (x .~ (8*tileSize) $ y .~ (8*tileSize) $ player_) def
        where player_ = (Player DirDown def)

main = run "chips challenge" (boardW * tileSize, boardH * tileSize) gameState on stepGame


on (EventKey (SpecialKey KeyLeft) Down _ _) gs = return $ player.direction .~ DirLeft
                                                        $ player.x -~ x_
                                                        $ gs
    where x_ = case leftTile gs of
                 Wall _ -> 0
                 _ -> tileSize

on (EventKey (SpecialKey KeyRight) Down _ _) gs = return $ player.direction .~ DirRight
                                                        $ player.x +~ x_
                                                        $ gs
    where x_ = case rightTile gs of
                 Wall _ -> 0
                 _ -> tileSize

on (EventKey (SpecialKey KeyUp) Down _ _) gs = return $ player.direction .~ DirUp
                                                        $ player.y +~ y_
                                                        $ gs
    where y_ = case upTile gs of
                 Wall _ -> 0
                 _ -> tileSize

on (EventKey (SpecialKey KeyDown) Down _ _) gs = return $ player.direction .~ DirDown
                                                        $ player.y -~ y_
                                                        $ gs
    where y_ = case downTile gs of
                 Wall _ -> 0
                 _ -> tileSize

on (EventKey (SpecialKey KeySpace) Down _ _) gs = return gameState

on _ gs = return $ player.direction .~ DirDown $ gs


-- on _ gs = return gs


stepGame _ gs = do
    let playerIx = currentIdx gs
    case currentTile gs of
        Chip _ -> do
          let attrs_ = ((gs ^. tiles) !! playerIx) ^. attrs
          return $ tiles.(ix playerIx) .~ (Empty attrs_) $ gs
        _ -> return gs



-- stepGame _ gs = return gs
