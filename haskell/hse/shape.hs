module Shape where

import List (union, (\\), unionBy)
import Ix (range)
import Numeric (floatToDigits)

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
             deriving Show

type Radius = Float
type Side = Float
type Vertex = (Float, Float)
type Points = [Vertex]

square :: Float -> Shape
square s = Rectangle s s
circle :: Float -> Shape
circle r = Ellipse r r

rectangle :: Float -> Float -> Shape
rectangle s1 s2 = Polygon [(0.0,0.0),(s1,0.0),(s1,s2),(0.0,s2)]
rtTriangle :: Float -> Float -> Shape
rtTriangle s1 s2 = Polygon [(0.0,0.0),(s1,0.0),(0.0,s2)]

mirrorYAxis :: Vertex -> Vertex
mirrorYAxis (x, y) = (0 + (-x), y)

mirrorXAxis :: Vertex -> Vertex
mirrorXAxis (x, y) = (x, 0 + (-y))

mirrorBoth :: Vertex -> Vertex
mirrorBoth (x, y) = (0 + (-x), 0 + (-y))

mirrorYAxisPoints :: [Vertex] -> [Vertex]
mirrorYAxisPoints points = map mirrorYAxis points \\ points

mirrorXAxisPoints :: [Vertex] -> [Vertex]
mirrorXAxisPoints points = map mirrorXAxis points \\ points

mirrorBothPoints :: [Vertex] -> [Vertex]
mirrorBothPoints points = map mirrorBoth points \\ points

compareVertex :: Vertex -> Vertex -> Bool
compareVertex (x1, x2) (y1, y2) = (twoDecimalPlaces x1) == (twoDecimalPlaces y1) && (twoDecimalPlaces x2) == (twoDecimalPlaces y2)

completeShape :: [Vertex] -> [Vertex]
completeShape q1 = unionBy compareVertex q1 (mirrorYAxisPoints q1)

radsFromDegs :: Float -> Float
radsFromDegs d = (d/180)*pi

degsFromRads :: Float -> Float
degsFromRads r = r*pi/180

-- Nasty fake precision-losing function to return 2decimal places
twoDecimalPlaces :: Float -> Float
twoDecimalPlaces x = fromInteger (round (x * 100)) / 100

regularPolygon :: Int -> Side -> Shape
regularPolygon n s =
    Polygon ([(0.0, b)] ++ (map (\p -> coords p b) (map (\j -> angle j m) (range (1, n-1)))))
    where
      i = ((fromIntegral(n)-2)*180)/fromIntegral(n) -- interior angle
      ir = radsFromDegs (i /2) -- half interior angle
      m = 360/fromIntegral(n) -- mid angle
      mr = radsFromDegs m
      b = (s*(sin ir))/(sin mr) -- brace length (inner side)
      angle j m = radsFromDegs (90 - (fromIntegral j) * m)
      coords p b = (b*(cos p), b*(sin p))

area :: Shape -> Float
area (Rectangle s1 s2) = s1 * s2
area (RtTriangle s1 s2) = s1 * s2/2
area (Ellipse r1 r2) = pi * r1 * r2
area (Polygon (v1:vs)) = polyArea vs
    where polyArea :: [Vertex] -> Float
          polyArea (v2:v3:vs') = triArea v1 v2 v3
                                 + polyArea (v3:vs')
          polyArea _ = 0

triArea :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = let a = sideLength v1 v2
                       b = sideLength v2 v3
                       c = sideLength v1 v3
                       s = 0.5 * (a + b + c)
                   in sqrt (s*(s-a)*(s-b)*(s-c))

sideLength :: Vertex -> Vertex -> Float
sideLength (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

convex :: Shape -> Bool
convex (Rectangle _ _) = True
convex (RtTriangle _ _) = True
convex (Ellipse _ _) = True
convex (Polygon vs) = allConvex (polyAngles vs)

allConvex :: [[Float]] -> Bool
allConvex ((angle:zs):vs) = (angle < 180) && allConvex vs
allConvex [[]] = True

polyAngles :: Polygon -> [[Float]]
polyAngles (Polygon ((x1,y1):(x2,y2):(x3,y3):vs')) =
    [angleA, a, b, c, cosA] : polyConvex ((x2,y2):(x3,y3):vs')
    where b = sideLength (x1,y1) (x2,y2)
          c = sideLength (x2,y2) (x3,y3)
          a = sideLength (x1,y1) (x3,y3)
          cosA = (b^2 + c^2 - a^2)/(2*b*c)
          angleA = degsFromRads(acos cosA)
polyAngles _ = [[]]

polyOne :: Shape
polyOne = Polygon([(1,7),(6,6),(7,2)])

polyTwo :: Shape
polyTwo = Polygon([(10,4),(11,8),(15,8),(15,4),(13,6),(12,1)])