{-# LANGUAGE ScopedTypeVariables #-}

module Types
    ( Planet(..)
    , mass
    , diameter
    , pullOn
    , distance
    , distanceScale
    , alterSpeed
    , newPoint
    )
where

import           Numeric.LinearAlgebra

data Planet = Earth  | Jupiter deriving (Eq, Show)

type Point = (Float, Float)

mass :: Planet -> Int
mass Earth   = 5972
mass Jupiter = 1898000

distanceScale :: Float
distanceScale = 1000000

diameter :: Planet -> Integer
diameter Earth   = 12756000 -- m
diameter Jupiter = 142984000

distance :: Point -> Point -> Float
distance (aX, aY) (bX, bY) =
    let height  = abs (bY - aY)
        width   = abs (bX - aX)
        height2 = height ^ 2
        width2  = width ^ 2
    in  sqrt (height2 + width2) * distanceScale * 100 -- planets actual size so brag about distance

pullOn :: Planet -> Point -> Planet -> Point -> Vector Float -- a accelaration pointing out from a
pullOn on a from b =
    let
        gConstant = 15 * 10 ^ 12
        massA     = mass on
        massB     = mass from
        dist      = distance b a ^ 2
        forceScalar =
            (-gConstant * fromIntegral massA * fromIntegral massB) / dist
        accScalar = forceScalar / fromIntegral massA
    in
        scale accScalar (vectorize b a)

vectorize :: Point -> Point -> Vector Float
vectorize (ax, ay) (bx, by) = fromList [bx - ax, by - ay]

alterSpeed :: Vector Float -> Vector Float -> Vector Float
alterSpeed current pullForce = add current pullForce -- todo

newPoint :: Point -> Vector Float -> Point
newPoint (ax, ay) v = case toList v of
    [bx, by] -> (ax + bx, ay + by)
    x ->
        error
            $  "cannot create new point from speed vector with /= 2 elements "
            ++ show x
