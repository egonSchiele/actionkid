import ActionKid
import Data.Monoid ((<>), mconcat)

gameState = [Tile "adit" defaultAttrs]

main :: IO ()
main = play "test game" gameState on stepGame

stepGame _ state = return state
on _ state = return state
