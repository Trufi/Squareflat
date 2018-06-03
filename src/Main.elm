module Main exposing (..)

import Html exposing (Html, div, text, program)
import Html.Attributes exposing (attribute, style)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Window exposing (Size)
import AnimationFrame
import Task
import Random
import Dict exposing (Dict)


type Msg
    = Tick Time
    | Resize Size
    | BoxOnClick Int


type alias Box =
    { id : Int
    , x : Int
    , y : Int
    , score : Int
    , createTime : Float
    , lifeTime: Float
    }


type alias Model =
    { time : Float
    , lastBoxSpawnTime : Float
    , seed : Random.Seed
    , positionGenerator : Random.Generator ( Int, Int )
    , size : Size
    , boxes : Dict Int Box
    }


positionGenerator : Size -> Random.Generator ( Int, Int )
positionGenerator size =
    Random.pair (Random.int 0 size.width) (Random.int 0 size.height)


init : ( Model, Cmd Msg )
init =
    let
        seed =
            Random.initialSeed 0

        size =
            Size 100 100
    in
        ( { time = 0
          , lastBoxSpawnTime = 0
          , seed = seed
          , positionGenerator = positionGenerator size
          , size = size
          , boxes = Dict.empty
          }
        , Task.perform Resize Window.size
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize size ->
            ( { model
                | size = size
                , positionGenerator = positionGenerator size
              }
            , Cmd.none
            )

        Tick time ->
            ( timeTick model time, Cmd.none )

        BoxOnClick id ->
            ( { model
                | boxes = Dict.remove id model.boxes
              }
            , Cmd.none
            )


timeTick : Model -> Float -> Model
timeTick model delta =
    let
        now =
            model.time + delta

        currentModel =
            { model | time = now }
    in
        currentModel
            |> maybeSpawn


maybeSpawn : Model -> Model
maybeSpawn model =
    if model.time - model.lastBoxSpawnTime > 1000 then
        let
            ( box, seed ) =
                createBox model.time model.positionGenerator model.seed
        in
            { model
                | seed = seed
                , boxes = Dict.insert box.id box model.boxes
                , lastBoxSpawnTime = model.time
            }
    else
        model


createBox : Float -> Random.Generator ( Int, Int ) -> Random.Seed -> ( Box, Random.Seed )
createBox time generator seed =
    let
        ( ( x, y ), newSeed ) =
            Random.step generator seed
    in
        ( Box (floor time) x y 1 time 0
        , newSeed
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick


view : Model -> Html Msg
view model =
    div []
        [ div [ attribute "class" "score" ] [ text (toString model.time) ]
        , div [ attribute "class" "gamefield" ] (List.map box (Dict.values model.boxes))
        ]


box : Box -> Html Msg
box b =
    div
        [ attribute "class" "box"
        , style
            [ ( "left", (toString b.x) ++ "px" )
            , ( "top", (toString b.y) ++ "px" )
            ]
        , onClick (BoxOnClick b.id)
        ]
        [ text (toString b.score) ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
