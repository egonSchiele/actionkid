{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
import ActionKid
import ActionKid.Utils
import Control.Lens
import Control.Monad.State

-- This is an example of how to use ActionKid to make games with Haskell.
-- ActionKid is inspired by Actionscript, so you will see some differences.
--
-- Every video game has some objects on the screen that interact with each
-- other. For example, in a game of Mario, you will have Mario himself,
-- goombas, mushrooms, pipes etc. These objects are called "movie clip"s in
-- ActionKid terminology. Any data type that is an instance of the
-- MovieClip class can be used in your game.

-- So first, make a data type for every object you will have in your game.
-- In this demo game, we just have a player that will move around.
--
-- Every constructor must have `Attributes` as it's last field.
data Player = Player { _pa :: Attributes }

-- Ok, you have a Player type. Now before you can use it in your game,
-- make it an instance of MovieClip. You can do this automatically with
-- `deriveMC`:
deriveMC ''Player

-- Now that the player is a MovieClip, you can write code like this:
--
-- > player.x += 10
--
-- and the player will move 10 pixels to the right!
-- More on this later.

-- You also need a data type that will be the game state.
data GameState = GameState {
                    _player :: Player,
                    _ga :: Attributes
}

-- Use this convenience function to make MovieClip instances
-- for your data types automatically.
deriveMC ''GameState

-- Next, I suggest you make lenses for all of your data types.
-- If you don't know how lenses work, check out the intro README here:
-- https://github.com/ekmett/lens
--
-- and this tutorial:
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing
--
-- Lenses are great for working with nested data structures, and writing
-- functional code that looks imperative. Both are big plusses for game
-- development.
-- So this step is optional, but recommended.
makeLenses ''Player
makeLenses ''GameState

-- Finally, you need to make both Player and GameState
-- instances of Renderable. This defines how they will be shown on the
-- screen.
--
-- The `render` function returns a Gloss Picture:
--
-- http://hackage.haskell.org/package/gloss-1.8.2.2/docs/Graphics-Gloss-Data-Picture.html

instance Renderable Player where
    render p = color blue $ box 50 50

-- Here we are just rendering the player as a blue box. With ActionKid,
-- you can also use an image from your computer instead:
--
-- > render p = image "images/player.png"

-- To render the game state, we just render the player.
-- To do that, use the `display` function. `display` will render
-- the player at the right x and y coordinates.
instance Renderable GameState where
    render gs = display (_player gs)

-- If the game state has multiple items, you can render them all by
-- concatenating them:
--
-- > render gs = display (_player1 gs) <> display (_player2 gs)

-- this is the default game state. The player starts at coordinates (0,0)
-- (the bottom left of the screen).
-- NOTE: For the `Attributes` field, you can just use `def`. This will set
-- the correct default attributes for an object.
--
-- So this creates the game state with a player in it. Both have default
-- attributes.
gameState = (GameState (Player def) def)

-- All of the core game logic takes place in this monad transformer stack.
-- The State is the default game state we just made.
type GameMonad a = StateT GameState IO a

--------------------------------------------------------------------------
--------------------------------------------------------------------------
-- Ok, now we are done specifying all the data types and how they should
-- look! Now it's time to implement the core game logic. There are two
-- functions you need to define:
--
-- 1. An event handler (for key presses/mouse clicks)
-- 2. A game loop.
--
-- The event handler listens for user input, and moves the player etc.
-- The game loop is where the rest of the logic happens: firing bullets,
-- hitting an enemy, animations etc etc.
--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- This is the event handler. Since we are using lenses, this logic is
-- really easy to write.
eventHandler :: Event -> GameMonad ()
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) = player.x -= 10
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) = player.x += 10
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) = player.y += 10
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) = player.y -= 10
eventHandler _ = return ()

-- This is the main loop. It does nothing right now.
mainLoop :: Float -> GameMonad ()
mainLoop _ = return ()

-- Now lets run the game! The run function takes:
-- 1. the title for the window of the game
-- 2. the size of the window
-- 3. the initial game state
-- 4. the eventHandler function
-- 5. the main loop function
main = run "demo game" (500, 500) gameState eventHandler mainLoop
