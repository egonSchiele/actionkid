# ActionKid

A game framework for Haskell.

The goal is to have an easy-to-use game framework for Haskell. For example, here's how you get a player on the screen:

```haskell
-- create a new player data type
data Player = Player { _pa :: Attributes }
deriveMC ''Player

-- describe what it should look like:
instance Renderable Player where
    render p = image "images/player.png"
```

Here's how you move your player:

```haskell
handle (EventKey (SpecialKey KeyLeft) Down _ _)  = player.x -= 10
handle (EventKey (SpecialKey KeyRight) Down _ _) = player.x += 10
handle (EventKey (SpecialKey KeyUp) Down _ _)    = player.y += 10
handle (EventKey (SpecialKey KeyDown) Down _ _)  = player.y -= 10
```

## On OS X

If you are playing on OS X, please disable "automatic GPU switching from system preferences -> energy saver first. Without this, the game may appear frozen.

## Similar modules

All the hard lifting in this package is done by [Gloss](https://hackage.haskell.org/package/gloss). ActionKid provides a lot of convenience functions.
[gloss-game](https://github.com/mchakravarty/gloss-game) is a similar module.

Check out the included example for more details.
Read the documentation on Hackage.
