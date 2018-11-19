import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D

import qualified Helm.Cmd        as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Mouse      as Mouse

import qualified Helm.Sub        as Sub
import           Helm.Time       (Time, second)
import qualified Helm.Time       as Time

data Colour = Red | Green

data Action = Idle | ChangePosition (V2 Double) | SwapColour | Animate Double

type Pos = V2 Double

data Model = Model { pos    :: [Pos]
                   , colour :: Colour }

initial :: (Model, Cmd SDLEngine Action)
initial = (model, Cmd.none) where
    model = Model {
        pos = [V2 0 0],
        colour = Green
    }

newColour :: Colour -> Colour
newColour Red   = Green
newColour Green = Red

limit :: Int
limit = 100

newPosList :: [Pos] -> Pos -> [Pos]
newPosList xs x = take limit $ x : xs

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle                 = (model, Cmd.none)
update model (ChangePosition newPos) = (model { pos = newPosList (pos model) newPos }, Cmd.none)
update model SwapColour =  (model {colour = newColour $ colour model}, Cmd.none)
update model _ = (model, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Sub.batch
    [ Mouse.clicks $ \_ _ -> SwapColour
    , Mouse.moves (\(V2 x y) -> ChangePosition $ V2 (fromIntegral x) (fromIntegral y))
    , Time.fps 60 Animate
    ]

view :: Model -> Graphics SDLEngine
view model = Graphics2D $ collage $ fmap (viewSquare $ colour model) (pos model)

-- viewSquare ::
viewSquare colour pos = move pos $ filled mouseColour $ square 10 where
    mouseColour = case colour of
        Red   -> rgb 1 0 0
        Green -> rgb 0 1 0

main :: IO ()
main = do
  engine <- SDL.startup

  run engine GameConfig
    { initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
}
