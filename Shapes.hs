module Shapes
( Point(..)
, Shape(Rectangle, Circle)
, surface
, nudge
, baseCircle
, baseRect
) where

import Data.Function

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

surface :: Shape -> Float
surface (Circle _ r)          = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = ((*) `on` abs) (x2 - x1) (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

data Person = Person
    {
          firstName :: String
        , lastName :: String
        , age :: Int
        , height :: Float
        , phoneNumber :: String
        , flavor :: String
    } deriving (Show, Eq)

guy :: Person
guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"

data Car a b c = Car {company :: a, model :: b, year :: c} deriving Show

baseCar :: Num a => Car String String a
baseCar = Car {company="Ford", model="Mustang", year=1967}

tellCar :: Show a => Car String String a -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

data Vector a = Vector a a a deriving Show

vplus :: Num t => Vector t -> Vector t -> Vector t
(Vector x1 y1 z1) `vplus` (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)

vectMult :: Num t => Vector t -> t -> Vector t
(Vector x1 y1 z1) `vectMult` m = Vector (x1 * m) (y1 * m) (z1 * m)

scalarMult :: Num t => Vector t -> Vector t -> t
(Vector x1 y1 z1) `scalarMult` (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2


