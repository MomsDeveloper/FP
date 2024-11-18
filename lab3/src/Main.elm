module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Interpolation exposing (lagrangeInterpolate, linearInterpolate)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { points : List Point
    , linearInterpolatedPoints : Result String (List Point)
    , lagrangeInterpolatedPoints : Result String (List Point)
    , inputPoint : Point
    , interpolationType : InterpolationType
    }


type Msg
    = InputX String
    | InputY String
    | AddPoint
    | ChangeInterpolationType InterpolationType


type alias Point =
    { x : Float
    , y : Float
    }


type InterpolationType
    = Linear
    | Lagrange
    | LagrangeAndLinear


stringToConfiguration : String -> InterpolationType
stringToConfiguration str =
    case str of
        "Linear" ->
            Linear

        "Lagrange" ->
            Lagrange

        "LagrangeAndLinear" ->
            LagrangeAndLinear

        _ ->
            Linear


init : Model
init =
    { points = []
    , linearInterpolatedPoints = Ok []
    , lagrangeInterpolatedPoints = Ok []
    , inputPoint = { x = 0, y = 0 }
    , interpolationType = Linear
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        inputPoint =
            model.inputPoint
    in
    case msg of
        InputX x ->
            case String.toFloat x of
                Just num ->
                    { model | inputPoint = { inputPoint | x = num } }

                Nothing ->
                    model

        InputY y ->
            case String.toFloat y of
                Just num ->
                    { model | inputPoint = { inputPoint | y = num } }

                Nothing ->
                    model

        AddPoint ->
            let
                newPoints =
                    inputPoint :: model.points
            in
            case model.interpolationType of
                Linear ->
                    { model
                        | points = newPoints
                        , linearInterpolatedPoints = tryInterpolateLinearBatch newPoints
                    }

                Lagrange ->
                    { model
                        | points = newPoints
                        , lagrangeInterpolatedPoints = tryInterpolateLagrangeBatch newPoints
                    }

                LagrangeAndLinear ->
                    { model
                        | points = newPoints
                        , linearInterpolatedPoints = tryInterpolateLinearBatch newPoints
                        , lagrangeInterpolatedPoints = tryInterpolateLagrangeBatch newPoints
                    }

        ChangeInterpolationType interpolationType ->
            case interpolationType of
                Linear ->
                    { model
                        | interpolationType = Linear
                        , linearInterpolatedPoints = tryInterpolateLinearAll model.points
                        , lagrangeInterpolatedPoints = Ok []
                    }

                Lagrange ->
                    { model
                        | interpolationType = Lagrange
                        , linearInterpolatedPoints = Ok []
                        , lagrangeInterpolatedPoints = tryInterpolateLagrangeAll model.points
                    }

                LagrangeAndLinear ->
                    { model
                        | interpolationType = LagrangeAndLinear
                        , linearInterpolatedPoints = tryInterpolateLinearAll model.points
                        , lagrangeInterpolatedPoints = tryInterpolateLagrangeAll model.points
                    }


tryInterpolateLinearBatch : List Point -> Result String (List Point)
tryInterpolateLinearBatch points =
    case points of
        right :: left :: _ ->
            Ok (interpolate left.x right.x (linearInterpolate left right))

        _ ->
            Err "Linear interpolation requires at least two points"


tryInterpolateLinearAll : List Point -> Result String (List Point)
tryInterpolateLinearAll points =
    let
        aux ps acc =
            case ps of
                right :: left :: [] ->
                    Ok (interpolate left.x right.x (linearInterpolate left right) ++ acc)

                right :: left :: rest ->
                    aux (left :: rest) (interpolate left.x right.x (linearInterpolate left right) ++ acc)

                _ ->
                    Err "Linear interpolation requires at least two points"
    in
    aux points []


tryInterpolateLagrangeBatch : List Point -> Result String (List Point)
tryInterpolateLagrangeBatch points =
    case points of
        p4 :: p3 :: p2 :: p1 :: p0 :: _ ->
            Ok (interpolate p3.x p4.x (lagrangeInterpolate [ p0, p1, p2, p3, p4 ]))

        _ ->
            Err "Lagrange interpolation requires at least 5 points"


tryInterpolateLagrangeAll : List Point -> Result String (List Point)
tryInterpolateLagrangeAll points =
    let
        aux ps acc =
            case ps of
                p4 :: p3 :: p2 :: p1 :: p0 :: [] ->
                    Ok (interpolate p3.x p4.x (lagrangeInterpolate [ p0, p1, p2, p3, p4 ]) ++ acc)

                p4 :: p3 :: p2 :: p1 :: p0 :: rest ->
                    aux (p3 :: p2 :: p1 :: p0 :: rest) (interpolate p3.x p4.x (lagrangeInterpolate [ p0, p1, p2, p3, p4 ]) ++ acc)

                _ ->
                    Err "Lagrange interpolation requires at least 5 points"
    in
    aux points []


interpolate : Float -> Float -> (Float -> Float) -> List Point
interpolate start end f =
    List.map (\x -> { x = x, y = f x }) (range start end 0.1)


range : Float -> Float -> Float -> List Float
range start end step =
    if start > end then
        []

    else
        start :: range (start + step) end step


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "number", placeholder "X", onInput InputX ] []
        , input [ type_ "number", placeholder "Y", onInput InputY ] []
        , button [ onClick AddPoint ] [ text "Add Point" ]
        , select [ onInput (stringToConfiguration >> ChangeInterpolationType) ]
            [ option [ value "Linear" ] [ text "Linear" ]
            , option [ value "Lagrange" ] [ text "Lagrange" ]
            , option [ value "LagrangeAndLinear" ] [ text "Lagrange and Linear" ]
            ]
        , ul [] (List.map (\point -> li [] [ text (viewFloat point.x ++ ", " ++ viewFloat point.y) ]) <| List.reverse model.points)
        , case model.linearInterpolatedPoints of
            Ok points ->
                ul [] (List.map (\point -> li [] [ text (viewFloat point.x ++ ", " ++ viewFloat point.y) ]) points)

            Err error ->
                li [ style "color" "red" ] [ text error ]
        , case model.lagrangeInterpolatedPoints of
            Ok points ->
                ul [] (List.map (\point -> li [] [ text (viewFloat point.x ++ ", " ++ viewFloat point.y) ]) points)

            Err error ->
                li [ style "color" "red" ] [ text error ]
        ]


-- with 2 decimal places
viewFloat : Float -> String
viewFloat x =
    String.fromFloat ( (toFloat <| round (x * 100)) / 100 )
