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

Check out the included example for more details.
Read the documentation on Hackage.
