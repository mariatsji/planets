module Main where

import           Data.Monoid                              ( (<>) )
import           Types
import           Graphics.Gloss
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.ViewPort
import qualified Numeric.LinearAlgebra         as LinAlg

data Model = Model
  { earth :: Point
  , earthSpeed :: LinAlg.Vector Float
  , jupiter :: Point
  , jupiterSpeed :: LinAlg.Vector Float
  } deriving (Eq, Show)


initialModel = Model
  { earth        = (-200, -200)
  , earthSpeed   = LinAlg.fromList [-10, 10]
  , jupiter      = (0, 0)
  , jupiterSpeed = LinAlg.fromList [0, 0]
  }

main :: IO ()
main = simulate (InWindow "Newtons Planets" (1200, 800) (200, 500))
                black
                8
                initialModel
                drawModel
                stepFunction

radiusScaleFactor :: Integer -> Float
radiusScaleFactor diameter = fromIntegral diameter / distanceScale

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
  let earthPoint    = earth model
      jupiterPoint  = jupiter model
      earthSpeed'   = earthSpeed model
      jupiterSpeed' = jupiterSpeed model
      earthPulled   = pullOn Earth earthPoint Jupiter jupiterPoint
      jupiterPulled = pullOn Jupiter jupiterPoint Earth earthPoint
  in  Model
        { earth        = newPoint earthPoint earthSpeed'
        , earthSpeed   = alterSpeed earthSpeed' earthPulled
        , jupiter      = newPoint jupiterPoint jupiterSpeed'
        , jupiterSpeed = alterSpeed jupiterSpeed' jupiterPulled
        }

