module Interpolation exposing (..)

type alias Point =
    { x : Float
    , y : Float
    }

linearInterpolation : Point -> Point -> Float -> Float
linearInterpolation p1 p2 x =
    p1.y + (p2.y - p1.y) * (x - p1.x) / (p2.x - p1.x)

lagrangeInterpolation : List Point -> Float -> Float
lagrangeInterpolation points x =
    List.foldl
        (\p acc -> acc + p.y * (lagrangeBasis points x p.x))
        0
        points

lagrangeBasis : List Point -> Float -> Float -> Float
lagrangeBasis points x xi =
    List.foldl
        (\p acc -> acc * (x - p.x) / (xi - p.x))
        1
        (List.filter (\p -> p.x /= xi) points)
