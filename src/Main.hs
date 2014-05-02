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
          deriving Show

empty = Empty def
wall  = Wall def
chip  = Chip def

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

instance Renderable Tile where
    render (Empty _) = image "images/empty.png"
    render (Wall _)  = image "images/wall.png"
    render (Chip _)  = image "images/chip.png"

instance Renderable GameState where
    render gs = displayAll (_tiles gs) <> display (_player gs)

instance Renderable Player where
    render p = case p ^. direction of
                 DirUp    -> image "images/player_up.png"
                 DirDown  -> image "images/player_down.png"
                 DirLeft  -> image "images/player_left.png"
                 DirRight -> image "images/player_right.png"

tileMap = 
    [[1, 1, 1, 1],
     [1, 2, 0, 1],
     [1, 0, 0, 1],
     [1, 0, 0, 1],
     [1, 1, 1, 1]]

boardW = length . head $ tileMap
boardH = length tileMap

playerCoords :: GameState -> (Int, Int)
playerCoords gs = ((floor (p ^. x)) // tileSize, (((boardH * tileSize) - (floor (p ^. y))) // tileSize)-1)
    where p = gs ^. player
          ts = gs ^. tiles

currentIdx :: GameState -> Int
currentIdx gs = y_ * boardW + x_
    where (x_,y_) = playerCoords gs

currentTile gs = D.trace (show curTile) $ curTile
    where (x_,y_) = playerCoords gs
          curTile = (_tiles gs) !! (currentIdx gs)

leftTile gs = (_tiles gs) !! (currentIdx gs - 1)
  where (x_,y_) = playerCoords gs

rightTile gs = (_tiles gs) !! (currentIdx gs + 1)
  where (x_,y_) = playerCoords gs

upTile gs = (_tiles gs) !! (currentIdx gs - boardW)
  where (x_,y_) = playerCoords gs

downTile gs = (_tiles gs) !! (currentIdx gs + boardW)
  where (x_,y_) = playerCoords gs

renderedTiles = renderTileMap tileMap f (tileSize, tileSize)
  where f 0 = empty
        f 1 = wall
        f 2 = chip

gameState = GameState renderedTiles (x .~ 64 $ player_) def
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
stepGame _ gs = do
    let playerIx = currentIdx gs
    case currentTile gs of
        Chip _ -> do
          let attrs_ = ((gs ^. tiles) !! playerIx) ^. attrs
          return $ tiles.(ix playerIx) .~ (Empty attrs_) $ gs
        _ -> return gs
