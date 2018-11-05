module Geometry(
	area,
	perimeter,
	surfaceArea,
	volume
)
where

data Shape = Circle Double | Square Double | Rectangle Double Double | Cube Double | Sphere Double deriving Show

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Square a) = a^2
area (Rectangle a b) = a * b

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square a) = a * 4
perimeter (Rectangle a b) = 2 * (a + b)

surfaceArea::Shape -> Double
surfaceArea (Cube a) = 6 * a^2
surfaceArea (Sphere r) = 4 * pi * r^2

volume::Shape -> Double
volume (Cube a) = a^3
volume (Sphere r) = 4/3 * pi * r^3



