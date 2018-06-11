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
import Random
import Dict exposing (Dict)
import Models exposing (..)
import Boxes exposing (updateBoxes)
import ScoreLabels exposing (updateScoreLabels, addScoreLabel)


type Msg
    = Tick Time
    | Resize Size
    | BoxOnClick Int
    | ResetGame
    | RandomSeed Int


init : ( Model, Cmd Msg )
init =
    ( initial
    , Cmd.batch
        [ Task.perform Resize Window.size
        , Random.generate RandomSeed (Random.int 0 100)
        ]
    )


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

        RandomSeed seed ->
            ( { model
                | seed = Random.initialSeed seed
              }
            , Cmd.none
            )

        Tick time ->
            ( timeTick model time, Cmd.none )

        BoxOnClick id ->
            ( boxOnClick model id
            , Cmd.none
            )

        ResetGame ->
            init


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
    if model.boxesLeft /= 0 then
        updateGame model delta
    else
        model


updateGame : Model -> Float -> Model
updateGame model delta =
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
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        ]


view : Model -> Html Msg
view model =
    div [ attribute "class" "elmmain" ]
        [ div [ attribute "class" "lefttopinfo" ]
            [ div [ attribute "class" "score" ] [ text ("Score: " ++ toString model.score) ] ]
        , div [ attribute "class" "righttopinfo" ]
            [ div [ attribute "class" "boxesLeft" ] [ text ("Boxes left: " ++ toString model.boxesLeft) ] ]
        , node "div"
            [ classList
                [ ( "gamefield", True )
                , ( "gamefield-gameover", model.boxesLeft == 0 )
                ]
            ]
            (List.map viewScoreLabel model.scoreLabels
                ++ List.map viewBox (Dict.values model.boxes)
            )
        , viewGameOver model
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
        []
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


viewGameOver : Model -> Html Msg
viewGameOver model =
    if model.boxesLeft == 0 then
        div
            [ attribute "class" "gameover" ]
            [ div
                [ attribute "class" "gameover-header" ]
                [ text "Game Over" ]
            , div
                [ attribute "class" "gameover-text" ]
                [ div [ attribute "class" "gameover-text-el" ]
                    [ text ("Score: " ++ toString model.score) ]
                , div [ attribute "class" "gameover-text-el" ]
                    [ text ("Time: " ++ toString (round (model.time / 1000)) ++ "s") ]
                ]
            , div
                [ attribute "class" "gameover-again"
                , onClick ResetGame
                ]
                [ text "Restart" ]
            ]
    else
        Html.text ""


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
