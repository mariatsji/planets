module Main where

import           Control.Monad.Par
import           Data.Monoid                              ( (<>) )
import           Types
import qualified Text.Printf                   as Printf
import           Graphics.Gloss
import           Graphics.Gloss.Data.Color
import           Graphics.Gloss.Data.ViewPort
import qualified Numeric.LinearAlgebra         as LinAlg


windowSize :: (Int, Int)
windowSize = (1200, 800)

fps :: Int
fps = 10

data Model = Model
  { earth :: Point
  , earthSpeed :: LinAlg.Vector Float
  , jupiter :: Point
  , jupiterSpeed :: LinAlg.Vector Float
  } deriving (Show, Eq)

earthPosString :: Model -> String
earthPosString m =
  Printf.printf "earth (%.1f, %.1f)" (fst . earth $ m) (snd . earth $ m)

jupiterPosString :: Model -> String
jupiterPosString m =
  Printf.printf "jupiter (%.1f, %.1f)" (fst . jupiter $ m) (snd . jupiter $ m)

initialModel = Model
  { earth        = (-200, 0)
  , earthSpeed   = LinAlg.fromList [0, 20]
  , jupiter      = (0, 0)
  , jupiterSpeed = LinAlg.fromList [0, 0]
  }

main :: IO ()
main = simulate (InWindow "Newtons Planets" windowSize (200, 500))
                black
                fps
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
  in  earthPos <> jupiterPos <> modelInfo (earthPosString model) (-580) (-350) <>
        modelInfo (jupiterPosString model) (-580) (-300)

modelInfo :: String -> Integer -> Integer -> Picture
modelInfo text a b =
  let text'  = Text text
      scaled = scale 0.1 0.1 text'
      moved  = translate (fromIntegral a) (fromIntegral b) scaled
  in  color red moved

stepFunction :: ViewPort -> Float -> Model -> Model
stepFunction vp secs model =
  let earthPoint    = earth model
      jupiterPoint  = jupiter model
      earthSpeed'   = earthSpeed model
      jupiterSpeed' = jupiterSpeed model
      (earthPulled, jupiterPulled)  = runPar $ do
        p1 <- spawnP $ pullOn Earth earthPoint Jupiter jupiterPoint
        p2 <- spawnP $ pullOn Jupiter jupiterPoint Earth earthPoint
        p1' <- get p1
        p2' <- get p2
        return (p1', p2')

  in  Model
        { earth        = newPoint earthPoint earthSpeed'
        , earthSpeed   = alterSpeed earthSpeed' earthPulled
        , jupiter      = newPoint jupiterPoint jupiterSpeed'
        , jupiterSpeed = alterSpeed jupiterSpeed' jupiterPulled
        }

