module Main exposing (..)

import Html exposing (Html, div, text, program)
import Html.Attributes exposing (attribute, style, classList)
import Html.Events exposing (on, onClick)
import Html.Keyed exposing (node)
import Json.Decode
import Time exposing (Time)
import Window exposing (Size)
import AnimationFrame
import Task
import Dict exposing (Dict)
import Models exposing (..)
import Boxes exposing (updateBoxes)
import ScoreLabels exposing (updateScoreLabels, addScoreLabel)


type Msg
    = Tick Time
    | Resize Size
    | BoxOnClick Int


init : ( Model, Cmd Msg )
init =
    ( initial, Task.perform Resize Window.size )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            ( { model
                | size = size
                , positionGenerator = createPositionGenerator size
                , boxSize = boxSize size
              }
            , Cmd.none
            )

        Tick time ->
            ( timeTick model time, Cmd.none )

        BoxOnClick id ->
            ( boxOnClick model id
            , Cmd.none
            )


boxOnClick : Model -> Int -> Model
boxOnClick model id =
    let
        maybeBox =
            Dict.get id model.boxes
    in
        case maybeBox of
            Just box ->
                model
                    |> addScore box
                    |> addScoreLabel box
                    |> removeBox id

            Nothing ->
                model


removeBox : Int -> Model -> Model
removeBox id model =
    { model | boxes = Dict.remove id model.boxes }


addScore : Box -> Model -> Model
addScore box model =
    { model | score = model.score + box.score }


timeTick : Model -> Float -> Model
timeTick model delta =
    let
        now =
            model.time + delta

        currentModel =
            { model | time = now }
    in
        currentModel
            |> updateBoxes
            |> updateScoreLabels


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick


view : Model -> Html Msg
view model =
    div []
        [ div [ attribute "class" "info" ]
            [ div [ attribute "class" "time" ] [ text ("Time: " ++ toString (round (model.time / 1000))) ]
            , div [ attribute "class" "score" ] [ text ("Score: " ++ toString model.score) ]
            ]
        , node "div"
            [ attribute "class" "gamefield" ]
            (List.map viewScoreLabel model.scoreLabels
                ++ List.map viewBox (Dict.values model.boxes)
            )
        ]


viewBox : Box -> ( String, Html Msg )
viewBox box =
    ( toString box.id
    , div
        [ classList
            [ ( "box", True )
            , ( "box-visible", box.status == BoxIsVisible )
            , ( "box-score-" ++ (toString box.score), True )
            ]
        , style
            [ ( "left", (toString box.x) ++ "px" )
            , ( "top", (toString box.y) ++ "px" )
            ]
        , onClick (BoxOnClick box.id)
        , onTouch (BoxOnClick box.id)
        ]
        [ text (toString box.score) ]
    )


viewScoreLabel : ScoreLabel -> ( String, Html Msg )
viewScoreLabel scoreLabel =
    ( toString scoreLabel.id
    , div
        [ classList
            [ ( "scoreLabel", True )
            , ( "scoreLabel-score-" ++ (toString scoreLabel.score), True )
            ]
        , style
            [ ( "left", (toString scoreLabel.x) ++ "px" )
            , ( "top", (toString scoreLabel.y) ++ "px" )
            ]
        ]
        [ text ("+" ++ toString scoreLabel.score) ]
    )


onTouch : Msg -> Html.Attribute Msg
onTouch msg =
    on "touchstart" (Json.Decode.succeed msg)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
