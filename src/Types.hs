{-# LANGUAGE ScopedTypeVariables #-}

module Types
    ( Planet(..)
    , Vect(..)
    , mass
    , diameter
    , pull
    , distance
    , createVector
    , alterSpeed
    , newPoint
    )
where

import Data.Scientific

data Planet = Earth  | Jupiter deriving (Eq, Show)

data Vect = Vect Float Float deriving (Eq, Show)

type Point = (Float, Float)

mass :: Planet -> Scientific
mass Earth   = fromFloatDigits 5972e24 --kg
mass Jupiter = fromFloatDigits 1898e27

diameter :: Planet -> Integer
diameter Earth   = 12756000 -- m
diameter Jupiter = 142984000

distance :: Point -> Point -> Float
distance (aX, aY) (bX, bY) =
    let height  = abs (bY - aY)
        width   = abs (bX - aX)
        height2 = height ^ 2
        width2  = width ^ 2
    in  sqrt (height2 + width2)

pull :: Planet -> Point -> Planet -> Point -> Vect -- a accelaration pointing out from a
pull on a from b =
    let gConstant   = fromFloatDigits 6674e-11
        massA       = mass on
        massB       = mass from
        dist        = distance a b
        sciDist     = fromFloatDigits $ dist ^ 2
        rVect       = createVector a b
        forceScalar :: Scientific
        forceScalar = (gConstant * massA * massB) / sciDist
        forceForA   = forceScalar / massA
    in  scaleVect rVect forceForA

scaleVect :: Vect -> Scientific -> Vect
scaleVect (Vect ax bx) skalar =
    Vect (ax * (toRealFloat skalar)) (bx * (toRealFloat skalar))

createVector :: Point -> Point -> Vect
createVector (ax, ay) (bx, by) = Vect (abs $ bx - ax) (abs $ by - ay)

alterSpeed :: Vect -> Vect -> Vect
alterSpeed (Vect ax ay) (Vect bx by) = Vect (ax + bx) (ay + by)

newPoint :: Point -> Vect -> Point
newPoint (x,y) (Vect a b) = (x + a, y + b)