port module Main exposing (..)

import Browser
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Interpolation exposing (lagrangeInterpolate, linearInterpolate)


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { points : List Point
    , linearInterpolatedPoints : Result String (List Point)
    , lagrangeInterpolatedPoints : Result String (List Point)
    , inputPoint : Point
    , interpolationType : InterpolationType
    , previousX : Float
    , previousLinearPoint : Point
    , previousLagrangePoint : Point
    , debug : String
    }


type Msg
    = InputX String
    | InputY String
    | AddPoint
    | ChangeInterpolationType InterpolationType
    | Draw Float


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


init : flags -> ( Model, Cmd Msg )
init _ =
    Tuple.pair
        { points =
            [ { x = 12.7, y = 12.86 }
            , { x = 11.54, y = 14.74 }
            , { x = 10.58, y = 12.63 }
            , { x = 9.63, y = 11.15 }
            , { x = 8.44, y = 11.46 }
            , { x = 7.52, y = 9.75 }
            , { x = 5.7, y = 8.02 }
            , { x = 4.15, y = 7.86 }
            , { x = 2.6, y = 5.66 }
            , { x = 1.5, y = 3.47 }
            , { x = -0.88, y = 3.77 }
            , { x = -3.24, y = 0.8 }
            ]
        , linearInterpolatedPoints = Ok []
        , lagrangeInterpolatedPoints = Ok []
        , inputPoint = { x = 0, y = 0 }
        , interpolationType = Linear
        , previousX = 0
        , previousLinearPoint = { x = 0, y = 0 }
        , previousLagrangePoint = { x = 0, y = 0 }
        , debug = ""
        }
        Cmd.none


speed : Float
speed =
    0.005


scale : Float
scale =
    10


canvasSize : Int
canvasSize =
    500



-- SUBSCRIPTIONS


type alias CanvasId =
    String


type alias CanvasAction =
    { action : String
    , args : List String
    }


port draw : (Float -> msg) -> Sub msg


port canvas : ( CanvasId, List CanvasAction ) -> Cmd msg


port clearCanvas : CanvasId -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    draw Draw



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        inputPoint =
            model.inputPoint
    in
    case msg of
        InputX x ->
            case String.toFloat x of
                Just num ->
                    Tuple.pair { model | inputPoint = { inputPoint | x = num } } Cmd.none

                Nothing ->
                    Tuple.pair model Cmd.none

        InputY y ->
            case String.toFloat y of
                Just num ->
                    Tuple.pair { model | inputPoint = { inputPoint | y = num } } Cmd.none

                Nothing ->
                    Tuple.pair model Cmd.none

        AddPoint ->
            let
                newPoints =
                    inputPoint :: model.points
            in
            case model.interpolationType of
                Linear ->
                    Tuple.pair
                        { model
                            | points = newPoints
                            , linearInterpolatedPoints = tryInterpolateLinearBatch newPoints
                            , previousLinearPoint = setPreviousPoint model.previousLinearPoint inputPoint newPoints
                            , previousLagrangePoint = setPreviousPoint model.previousLagrangePoint inputPoint newPoints
                        }
                        Cmd.none

                Lagrange ->
                    Tuple.pair
                        { model
                            | points = newPoints
                            , lagrangeInterpolatedPoints = tryInterpolateLagrangeBatch newPoints
                            , previousLinearPoint = setPreviousPoint model.previousLinearPoint inputPoint newPoints
                            , previousLagrangePoint = setPreviousPoint model.previousLagrangePoint inputPoint newPoints
                        }
                        Cmd.none

                LagrangeAndLinear ->
                    Tuple.pair
                        { model
                            | points = newPoints
                            , linearInterpolatedPoints = tryInterpolateLinearBatch newPoints
                            , lagrangeInterpolatedPoints = tryInterpolateLagrangeBatch newPoints
                            , previousLinearPoint = setPreviousPoint model.previousLinearPoint inputPoint newPoints
                            , previousLagrangePoint = setPreviousPoint model.previousLagrangePoint inputPoint newPoints
                        }
                        Cmd.none

        ChangeInterpolationType interpolationType ->
            case interpolationType of
                Linear ->
                    Tuple.pair
                        { model
                            | interpolationType = Linear
                            , linearInterpolatedPoints = tryInterpolateLinearAll model.points
                            , lagrangeInterpolatedPoints = Ok []
                            , previousLinearPoint = resetPreviousPoint model.points
                            , previousLagrangePoint = resetPreviousPoint model.points
                        }
                        (clearCanvas "plot")

                Lagrange ->
                    Tuple.pair
                        { model
                            | interpolationType = Lagrange
                            , linearInterpolatedPoints = Ok []
                            , lagrangeInterpolatedPoints = tryInterpolateLagrangeAll model.points
                            , previousLinearPoint = resetPreviousPoint model.points
                            , previousLagrangePoint = resetPreviousPoint model.points
                        }
                        (clearCanvas "plot")

                LagrangeAndLinear ->
                    Tuple.pair
                        { model
                            | interpolationType = LagrangeAndLinear
                            , linearInterpolatedPoints = tryInterpolateLinearAll model.points
                            , lagrangeInterpolatedPoints = tryInterpolateLagrangeAll model.points
                            , previousLinearPoint = resetPreviousPoint model.points
                            , previousLagrangePoint = resetPreviousPoint model.points
                        }
                        (clearCanvas "plot")

        Draw delta ->
            let
                -- no matter the interpolation type, x is the same
                nextX =
                    model.previousLinearPoint.x + delta * speed
            in
            if shouldDraw nextX model.points then
                case model.interpolationType of
                    Linear ->
                        Maybe.map (\p -> ( p, drawInterpolatedRange "red" model.previousLinearPoint p )) (getNextLinearInterpolatedPointToDraw nextX delta model.points)
                            |> Maybe.map (\( p, cmd ) -> Tuple.pair { model | previousLinearPoint = p, previousLagrangePoint = p } cmd)
                            |> Maybe.withDefault (Tuple.pair model Cmd.none)

                    Lagrange ->
                        Maybe.map (\p -> ( p, drawInterpolatedRange "blue" model.previousLagrangePoint p )) (getNextLagrangeInterpolatedPointToDraw nextX delta model.points)
                            |> Maybe.map (\( p, cmd ) -> Tuple.pair { model | previousLinearPoint = p, previousLagrangePoint = p } cmd)
                            |> Maybe.withDefault (Tuple.pair model Cmd.none)

                    LagrangeAndLinear ->
                        Maybe.map (\p -> ( p, drawInterpolatedRange "red" model.previousLinearPoint p )) (getNextLinearInterpolatedPointToDraw nextX delta model.points)
                            |> (Maybe.map (\p -> ( p, drawInterpolatedRange "blue" model.previousLagrangePoint p )) (getNextLagrangeInterpolatedPointToDraw nextX delta model.points)
                                    |> Maybe.map2 (\( p1, cmd1 ) ( p2, cmd2 ) -> Tuple.pair { model | previousLinearPoint = p1, previousLagrangePoint = p2 } (Cmd.batch [ cmd1, cmd2 ]))
                               )
                            |> Maybe.withDefault (Tuple.pair model Cmd.none)

            else
                Tuple.pair model Cmd.none


getNextLinearInterpolatedPointToDraw : Float -> Float -> List Point -> Maybe Point
getNextLinearInterpolatedPointToDraw x delta points =
    case points of
        right :: left :: rest ->
            if left.x <= x && x <= right.x then
                Just { x = x, y = linearInterpolate left right x }

            else
                getNextLinearInterpolatedPointToDraw x delta (left :: rest)

        _ ->
            Nothing


getNextLagrangeInterpolatedPointToDraw : Float -> Float -> List Point -> Maybe Point
getNextLagrangeInterpolatedPointToDraw x delta points =
    case points of
        p4 :: p3 :: p2 :: p1 :: p0 :: rest ->
            if p0.x <= x && x <= p4.x then
                Just { x = x, y = lagrangeInterpolate [ p0, p1, p2, p3, p4 ] x }

            else
                getNextLagrangeInterpolatedPointToDraw x delta (p3 :: p2 :: p1 :: p0 :: rest)

        _ ->
            Nothing


drawInterpolatedRange : String -> Point -> Point -> Cmd Msg
drawInterpolatedRange style previous current =
    canvas
        ( "plot"
        , [ { action = "setStrokeStyle", args = [ style ] }
          , { action = "beginPath", args = [] }
          , { action = "moveTo", args = [ String.fromFloat (previous.x * scale), String.fromFloat (toFloat canvasSize - previous.y * scale) ] }
          , { action = "lineTo", args = [ String.fromFloat (current.x * scale), String.fromFloat (toFloat canvasSize - current.y * scale) ] }
          , { action = "stroke", args = [] }
          , { action = "closePath", args = [] }
          ]
        )


setPreviousPoint : Point -> Point -> List Point -> Point
setPreviousPoint previous current points =
    if List.length points == 0 then
        current

    else
        previous


resetPreviousPoint : List Point -> Point
resetPreviousPoint points =
    if List.length points == 0 then
        { x = 0, y = 0 }

    else
        List.head (List.reverse points)
            |> Maybe.withDefault { x = 0, y = 0 }


shouldDraw : Float -> List Point -> Bool
shouldDraw x points =
    case points of
        last :: _ ->
            x <= last.x

        _ ->
            False


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
    let
        aux start_ end_ step_ acc =
            if start_ > end_ then
                List.reverse acc

            else
                aux (start_ + step_) end_ step_ (start_ :: acc)
    in
    aux start end step []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ Html.text "Ooga Debooga   " ]
        , input [ type_ "number", placeholder "X", onInput InputX ] []
        , input [ type_ "number", placeholder "Y", onInput InputY ] []
        , button [ onClick AddPoint ] [ Html.text "Add Point" ]
        , select [ onInput (stringToConfiguration >> ChangeInterpolationType) ]
            [ option [ value "Linear" ] [ Html.text "Linear" ]
            , option [ value "Lagrange" ] [ Html.text "Lagrange" ]
            , option [ value "LagrangeAndLinear" ] [ Html.text "Lagrange and Linear" ]
            ]
        , Html.canvas [ id "plot", width canvasSize, height canvasSize, style "border" "1px solid black" ] []
        , ul [] (List.map (\point -> li [] [ Html.text (viewFloat point.x ++ ", " ++ viewFloat point.y) ]) <| List.reverse model.points)
        , case model.linearInterpolatedPoints of
            Ok points ->
                ul [] (List.map (\point -> li [] [ Html.text (viewFloat point.x ++ ", " ++ viewFloat point.y) ]) points)

            Err error ->
                li [ style "color" "red" ] [ Html.text error ]
        , case model.lagrangeInterpolatedPoints of
            Ok points ->
                ul [] (List.map (\point -> li [] [ Html.text (viewFloat point.x ++ ", " ++ viewFloat point.y) ]) points)

            Err error ->
                li [ style "color" "red" ] [ Html.text error ]
        ]



-- with 2 decimal places


viewFloat : Float -> String
viewFloat x =
    String.fromFloat ((toFloat <| round (x * 100)) / 100)
