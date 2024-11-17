module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String exposing (fromFloat)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { points : List Point
    , currentX : Float
    , currentY : Float
    }


type Msg
    = AddX String
    | AddY String
    | AddPoint
    | RemovePoint


type alias Point =
    { x : Float
    , y : Float
    }


init : Model
init =
    { points = []
    , currentX = 0
    , currentY = 0
    }



update : Msg -> Model -> Model
update msg model =
    case msg of
        AddX x ->
            case String.toFloat x of
                Just num -> { model | currentX = num }
                Nothing -> model
        
        AddY y ->
            case String.toFloat y of
                Just num -> { model | currentY = num }
                Nothing -> model
        
        AddPoint ->
            { model | points = { x = model.currentX, y = model.currentY } :: model.points }
        
        RemovePoint ->
            { model | points = List.drop 1 model.points }


view: Model -> Html Msg
view model =
    div []
        [ input [ type_ "input", placeholder "X", onInput AddX ] []
        , input [ type_ "input", placeholder "Y", onInput AddY ] []
        , button [ onClick AddPoint ] [ text "Add Point" ]
        , button [ onClick RemovePoint ] [ text "Remove Point" ]
        , ul [] (List.map (\point -> li [] [ text (fromFloat point.x ++ ", " ++ fromFloat point.y) ]) model.points)
        ]
