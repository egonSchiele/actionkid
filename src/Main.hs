{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss
import qualified Debug.Trace as D
import Data.Maybe
import Control.Applicative

data Jumping = NotJumping | Jumping Int | Falling Int deriving Eq

data SimpleTile = SimpleTile { _simpleTileAttrs :: Attributes, _simpleTileColor :: Color }
makeLenses ''SimpleTile

instance MovieClip SimpleTile where
    attrs = simpleTileAttrs
    render tile = (color (tile ^. simpleTileColor) $ box 100 100)

data Tile = Tile {
              _tileAttrs :: Attributes,
              _tileColor :: Color,
              _jumping :: Jumping,
              _moveX :: Float
}

makeLenses ''Tile

instance MovieClip Tile where
  attrs = tileAttrs
  render tile = (color (tile ^. tileColor) $ box 100 100)

data GameState = GameState {
                  _player :: Tile,
                  _enemy :: Tile,
                  _gameMoveX :: Float,
                  _gameMap :: [SimpleTile],
                  _gameAttrs :: Attributes
}

makeLenses ''GameState

instance MovieClip GameState where
    attrs = gameAttrs
    render gs = (displayAll (gs ^. gameMap)) <> (displayAll $ [gs ^. player] ++ [gs ^. enemy])
                  

tileMap :: [[Int]]
tileMap =
    [[1, 1, 1, 1],
     [1, 0, 0, 0],
     [1, 0, 0, 0],
     [1, 1, 0, 0],
     [0, 0, 0, 1]]

toCoords gs = (x_ // 100, y_ // 100)
    where x_ = floor $ (-1 * (gs ^. x)) + (gs ^. player.x)
          y_ = floor $ 400 - gs ^. player.y

get :: Int -> [a] -> Maybe a
get index arr = if index > length arr - 1 || index < 0
                   then Nothing
                   else Just $ arr !! index

above :: GameState -> Int
above gs = fromMaybe 0 $ (get (y_ - 1) tileMap) >>= get x_
    where (x_, y_) = toCoords gs

below :: GameState -> Int
below gs = fromMaybe 0 $ (get (y_ + 1) tileMap) >>= get x_
    where (x_, y_) = toCoords gs

left :: GameState -> Int
left gs = fromMaybe 0 $ (get y_ tileMap) >>= get (x_ - 1)
    where (x_, y_) = toCoords gs

right :: GameState -> Int
right gs = fromMaybe 0 $ (get y_ tileMap) >>= get (x_ + 1)
    where (x_, y_) = toCoords gs

renderedTileMap = concat $ mapWithIndex f tileMap
    where f (row, j) = mapWithIndex (func j) row
          func j (t,i) = y .~ (fromIntegral $ 400 - (j * 100)) $ x .~ (fromIntegral $ i * 100) $ tile
            where tile = if t == 1
                          then SimpleTile def black
                          else SimpleTile def white

gameState = GameState p (x .~ 50 $ es) 10.0 renderedTileMap def
  where p  = Tile def blue NotJumping 0.0
        es = Tile def green NotJumping 5.0

main = run "test game" (500, 500) gameState on stepGame

stepGame _ gs = return . scroll . checkHit . move . jump $ gs

scroll gs
  | gs ^. player.x > 250 && gs ^. player.moveX > 0.0 = x -~ (gs ^. gameMoveX) $ gs
  | gs ^. player.x > 250 && gs ^. player.moveX < 0.0 = x +~ (gs ^. gameMoveX) $ gs
  | otherwise = gs

moveEnemies gs = enemy.x +~ (gs ^. enemy.moveX) $ gs
checkHit gs = if (gs ^. player) `hits` (gs ^. enemy)
                then player.tileColor .~ red  $ gs
                else player.tileColor .~ blue $ gs

isJumping (Jumping _) = True
isJumping _ = False

move gs
  | gs ^. player.moveX < 0.0 && (left gs == 1) = gs
  | gs ^. player.moveX > 0.0 && (right gs == 1) = gs
  | otherwise = player.x +~ (gs ^. player.moveX) $ gs
jump gs
  | above gs == 1 && (isJumping (gs ^. player.jumping)) = player.jumping .~ NotJumping $ gs
  | otherwise = case gs ^. player.jumping of
                  NotJumping -> gs
                  Jumping 0  -> player.jumping .~ (Falling 10) $ gs
                  Jumping i  -> player.jumping .~ (Jumping (i-1)) $
                                player.y +~ 8 $ gs
                  Falling 0  -> player.jumping .~ NotJumping $ gs
                  Falling i  -> player.jumping .~ (Falling (i-1)) $
                                player.y -~ 8 $ gs

on (EventKey (SpecialKey KeyLeft) Down _ _) gs = return $ player.moveX .~ -10.0 $ gs
on (EventKey (SpecialKey KeyRight) Down _ _) gs = return $ player.moveX .~ 10.0 $ gs
on (EventKey (SpecialKey KeyLeft) Up _ _) gs = return $ if gs ^. player.moveX == -10.0
                                                          then player.moveX .~ 0.0 $ gs
                                                          else gs
on (EventKey (SpecialKey KeyRight) Up _ _) gs = return $ if gs ^. player.moveX == 10.0
                                                           then player.moveX .~ 0.0 $ gs
                                                           else gs
on (EventKey (SpecialKey KeyUp) Down _ _) gs = return $ if gs ^. player.jumping == NotJumping
                                                          then player.jumping .~ (Jumping 10) $ gs
                                                          else gs
on _ state = return state
