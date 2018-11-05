module Main where

import           Data.Monoid                              ( (<>) )
import           Types
import           Graphics.Gloss
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.ViewPort

data Model = Model
  { earth :: Point
  , earthSpeed :: Vect
  , jupiter :: Point
  , jupiterSpeed :: Vect
  } deriving (Eq, Show)


initialModel = Model 
  { earth = (-200, -200)
  , earthSpeed = Vect 0 1
  , jupiter = (200, 200)
  , jupiterSpeed = Vect 0 (-1)}

main :: IO ()
main = simulate (InWindow "Newtons Planets" (800, 800) (500, 500))
                black
                10
                initialModel
                drawModel
                stepFunction

radiusScaleFactor :: Integer -> Float
radiusScaleFactor diameter = fromIntegral diameter / 1000000

earthRadius :: Float
earthRadius = radiusScaleFactor . diameter $ Earth

jupiterRadius :: Float
jupiterRadius = radiusScaleFactor . diameter $ Jupiter

drawModel :: Model -> Picture
drawModel model =
  let earth'               = color blue (circle earthRadius)
      jupiter'             = color cyan (circle jupiterRadius)
      (earthX  , earthY  ) = earth model
      (jupiterX, jupiterY) = jupiter model
      earthPos             = translate earthX earthY earth'
      jupiterPos           = translate jupiterX jupiterY jupiter'
  in  earthPos <> jupiterPos

stepFunction :: ViewPort -> Float -> Model -> Model
stepFunction vp secs model =
  let earthPoint = earth model
      jupiterPoint = jupiter model
      earthSpeed' = earthSpeed model
      jupiterSpeed' = jupiterSpeed model
      earthPulled = pull Earth earthPoint Jupiter jupiterPoint
      jupiterPulled = pull Jupiter jupiterPoint Earth earthPoint
  in  Model
        { earth   = newPoint earthPoint earthSpeed'
        , earthSpeed = alterSpeed earthSpeed' earthPulled
        , jupiter = newPoint jupiterPoint jupiterSpeed'
        , jupiterSpeed = alterSpeed jupiterSpeed' jupiterPulled
        }

