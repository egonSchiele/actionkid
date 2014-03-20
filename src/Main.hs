{-# LANGUAGE TemplateHaskell #-}
import ActionKid
import Data.Monoid ((<>), mconcat)
import ActionKid.Utils
import Control.Lens
import Graphics.Gloss

data Jumping = NotJumping | Jumping Int | Falling Int

data Tile = Tile {
              _tileColor :: Color,
              _tileAttrs :: Attributes,
              _jumping :: Jumping
}

setJump val (Jumping _) = Jumping val
setJump val (Falling _) = Falling val
stJump val x = x

makeLenses ''Tile

instance MovieClip Tile where
  getAttrs = _tileAttrs
  setAttrs mc a = mc { _tileAttrs = a }
  render tile = (color (tile ^. tileColor) $ box 100 100)

adit   = Tile blue defaultAttrs NotJumping
calvin = Tile red defaultAttrs NotJumping

gameState = [adit, y .~ 50 $ calvin]

main = run "test game" (500, 500) gameState on stepGame

-- moveTile tile = (x +~ (fromIntegral $ tile ^. moveX)) tile
stepGame _ s@(t:ts) = case t ^. jumping of
                      NotJumping -> return s
                      Jumping 0 -> return ((jumping .~ (Falling 10) $ t):ts)
                      Jumping i -> return ((jumping %~ (setJump (i-1)) $ y +~ 8 $ t):ts)
                      Falling 0 -> return ((jumping .~ NotJumping $ t):ts)
                      Falling i -> return ((jumping %~ (setJump (i-1)) $ y -~ 8 $ t):ts)

on (EventKey (SpecialKey KeyLeft) Down _ _) (t:ts) = return ((moveX .~ -10.0 $ t):ts)
on (EventKey (SpecialKey KeyRight) Down _ _) (t:ts) = return ((moveX .~ 10.0 $ t):ts)
on (EventKey (SpecialKey KeyLeft) Up _ _) s@(t:ts) = return $ if t ^. moveX == -10.0
                                                                then ((moveX .~ 0.0 $ t):ts)
                                                                else s
on (EventKey (SpecialKey KeyRight) Up _ _) s@(t:ts) = return $ if t ^. moveX == 10.0
                                                                 then ((moveX .~ 0.0 $ t):ts)
                                                                 else s
on (EventKey (SpecialKey KeyUp) Down _ _) (t:ts) = return ((jumping .~ (Jumping 10) $ t):ts)
on _ state = return state
