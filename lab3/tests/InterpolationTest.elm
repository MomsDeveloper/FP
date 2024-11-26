module InterpolationTest exposing (..)

import Interpolation exposing (linearInterpolate, lagrangeInterpolate)

import Expect exposing (..)
import Test exposing (Test, test, describe)


tests : Test
tests =
    describe "Interpolation tests"
        [ test "Linear interpolation between two points" <|
            \_ ->
                let
                    p1 = { x = 0, y = 0 }
                    p2 = { x = 10, y = 10 }
                    x = 5
                    result = linearInterpolate p1 p2 x
                in
                Expect.equal result 5

        , test "Linear interpolation with a negative x" <|
            \_ ->
                let
                    p1 = { x = -5, y = -5 }
                    p2 = { x = 5, y = 5 }
                    x = 0
                    result = linearInterpolate p1 p2 x
                in
                Expect.equal result 0

        , test "Lagrange interpolation with three points" <|
            \_ ->
                let
                    points =
                        [ { x = 1, y = 1 }
                        , { x = 2, y = 4 }
                        , { x = 3, y = 9 }
                        ]
                    x = 2
                    result = lagrangeInterpolate points x
                in
                Expect.equal result 4

        , test "Lagrange interpolation with a single point" <|
            \_ ->
                let
                    points =
                        [ { x = 0, y = 5 } ]
                    x = 0
                    result = lagrangeInterpolate points x
                in
                Expect.equal result 5

        , test "Lagrange interpolation with two points" <|
            \_ ->
                let
                    points =
                        [ { x = 1, y = 2 }
                        , { x = 3, y = 4 }
                        ]
                    x = 2
                    result = lagrangeInterpolate points x
                in
                Expect.equal result 3
        , test "Lagrange interpolation with sin" <|
            \_ ->
                let
                    points =
                        [ { x = 0, y = 0 }
                        , { x = 1, y = 1 }
                        , { x = 2, y = 0 }
                        , { x = 3, y = -1 }
                        , { x = 4, y = 0 }
                        ]
                    x = 2
                    result = lagrangeInterpolate points x
                in
                Expect.equal result 0
        , test "Lagrange interpolation with zero points" <|
            \_ ->
                let
                    points = []
                    x = 2
                    result = lagrangeInterpolate points x
                in
                Expect.equal result 0
        ]
