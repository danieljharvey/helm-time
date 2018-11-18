import           Linear.V2       (V2 (V2))

import           Helm
import           Helm.Color
import           Helm.Engine.SDL (SDLEngine)
import           Helm.Graphics2D

import qualified Helm.Cmd        as Cmd
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Mouse      as Mouse

data Colour = Red | Green

data Action = Idle | ChangePosition (V2 Double)
data Model = Model { pos    :: V2 Double
                   , colour :: Colour }

initial :: (Model, Cmd SDLEngine Action)
initial = (model, Cmd.none) where
    model = Model {
        pos = V2 0 0,
        colour = Green
    }

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model Idle                 = (model, Cmd.none)
update model (ChangePosition pos) = (model { pos = pos }, Cmd.none)

subscriptions :: Sub SDLEngine Action
subscriptions = Mouse.moves (\(V2 x y) -> ChangePosition $ V2 (fromIntegral x) (fromIntegral y))

view :: Model -> Graphics SDLEngine
view model = Graphics2D $ collage [move mousePos $ filled mouseColour $ square 10] where
    mousePos = pos model
    mouseColour = case colour model of
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
