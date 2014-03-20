{-# LANGUAGE TemplateHaskell #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss

data Jumping = NotJumping | Jumping Int | Falling Int deriving Eq

data Tile = Tile {
              _tileColor :: Color,
              _tileAttrs :: Attributes,
              _jumping :: Jumping,
              _moveX :: Float
}

setJump val (Jumping _) = Jumping val
setJump val (Falling _) = Falling val
setJump val x = x

makeLenses ''Tile

instance MovieClip Tile where
  getAttrs = _tileAttrs
  setAttrs mc a = mc { _tileAttrs = a }
  render tile = (color (tile ^. tileColor) $ box 100 100)

data GameState = GameState {
                  _player :: Tile,
                  _gameAttrs :: Attributes
}

instance MovieClip GameState where
    getAttrs = _gameAttrs
    setAttrs mc a = mc { _gameAttrs = a }
    render gs = ActionKid.display . _player $ gs

makeLenses ''GameState

gameState = GameState (Tile blue defaultAttrs NotJumping 0.0) defaultAttrs
main = run "test game" (500, 500) gameState on stepGame

stepGame _ gs = return . move . jump $ gs

move gs = player.x +~ (gs ^. player.moveX) $ gs

jump gs = case gs ^. player.jumping of
            NotJumping -> gs
            Jumping 0  -> player.jumping .~ (Falling 10) $ gs
            Jumping i  -> player.jumping %~ (setJump (i-1)) $
                          player.y +~ 8 $ gs
            Falling 0  -> player.jumping .~ NotJumping $ gs
            Falling i  -> player.jumping %~ (setJump (i-1)) $
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
