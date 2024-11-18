module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Interpolation exposing (lagrangeInterpolation, linearInterpolation)
import String exposing (fromFloat, fromInt)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { points : Array Point
    , interpolatedPoints : Array Point
    , currentX : Float
    , currentY : Float
    , currentPoint : Int
    , interpolationType : InterpolationType
    }


type Msg
    = AddX String
    | AddY String
    | AddPoint
    | RemovePoint
    | ChangeConfiguration InterpolationType


type alias Point =
    { x : Float
    , y : Float
    }


type InterpolationType
    = Linear
    | Lagrange
    | LagrangeAndLinear



-- type alias Configuration =
--     { step : Float
--     , interpolationType : InterpolationType
--     }


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
    { points = Array.empty
    , interpolatedPoints = Array.empty
    , currentX = 0
    , currentY = 0
    , currentPoint = 0
    , interpolationType = Linear
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddX x ->
            case String.toFloat x of
                Just num ->
                    { model | currentX = num }

                Nothing ->
                    model

        AddY y ->
            case String.toFloat y of
                Just num ->
                    { model | currentY = num }

                Nothing ->
                    model

        AddPoint ->
            let
                interpolatedModel =
                    interpolate model
            in
            { interpolatedModel | points = Array.push { x = model.currentX, y = model.currentY } interpolatedModel.points }

        RemovePoint ->
            { model | points = Array.slice 0 (Array.length model.points - 1) model.points }

        ChangeConfiguration configuration ->
            { model | interpolationType = configuration }


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "input", placeholder "X", onInput AddX ] []
        , input [ type_ "input", placeholder "Y", onInput AddY ] []
        , button [ onClick AddPoint ] [ text "Add Point" ]
        , button [ onClick RemovePoint ] [ text "Remove Point" ]
        , select [ onInput (stringToConfiguration >> ChangeConfiguration) ]
            [ option [ value "Linear" ] [ text "Linear" ]
            , option [ value "Lagrange" ] [ text "Lagrange" ]
            , option [ value "LagrangeAndLinear" ] [ text "Lagrange and Linear" ]
            ]
        , ul [] (List.map (\point -> li [] [ text (fromFloat point.x ++ ", " ++ fromFloat point.y) ]) (Array.toList model.points))
        , ul [] (List.map (\point -> li [] [ text (fromFloat point.x ++ ", " ++ fromFloat point.y) ]) (Array.toList model.interpolatedPoints))
        , text (fromInt model.currentPoint)
        ]


interpolate : Model -> Model
interpolate model =
    let
        newPoints =
            getNewPoints model.currentPoint model.points model.interpolationType
    in
    if Array.length newPoints > 0 then
        { model
            | interpolatedPoints = Array.append model.interpolatedPoints newPoints
            , currentPoint = model.currentPoint + 1
        }

    else
        model


getNewPoints : Int -> Array Point -> InterpolationType -> Array Point
getNewPoints currentPoint points interpolationType =
    case interpolationType of
        Linear ->
            if Array.length points < 2 then
                Array.empty

            else
                let
                    ( left, right ) =
                        getBorders currentPoint points

                    listOfX =
                        getListWithStep left.x right.x 0.1

                    getNewY x =
                        roundToTwoDecimals (linearInterpolation left right x)
                in
                Array.fromList (List.map (\x -> { x = x, y = getNewY x }) listOfX)

        _ ->
            Array.empty


getBorders : Int -> Array Point -> ( Point, Point )
getBorders index points =
    let
        left =
            Array.get index points

        right =
            Array.get (index + 1) points
    in
    case ( left, right ) of
        ( Just l, Just r ) ->
            ( l, r )

        _ ->
            ( { x = 0, y = 0 }, { x = 0, y = 0 } )


getListWithStep : Float -> Float -> Float -> List Float
getListWithStep start end step =
    if start > end then
        []

    else
        start :: getListWithStep (roundToTwoDecimals (start + step)) end step


roundToTwoDecimals : Float -> Float
roundToTwoDecimals number =
    toFloat (round (number * 100)) / 100
