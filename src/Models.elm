module Models exposing (..)

import Window exposing (Size)
import Dict exposing (Dict)
import Random


type BoxStatus
    = BoxIsInitializing
    | BoxIsVisible
    | BoxIsHiding


type alias Box =
    { id : Int
    , status : BoxStatus
    , x : Int
    , y : Int
    , score : Int
    , createTime : Float
    }


type alias ScoreLabel =
    { id : Int
    , createTime : Float
    , x : Int
    , y : Int
    , score : Int
    }


type alias Model =
    { time : Float
    , lastBoxSpawnTime : Float
    , seed : Random.Seed
    , size : Size
    , boxSize : Int
    , positionGenerator : Random.Generator ( Int, Int )
    , scoreGenerator : Random.Generator Int
    , boxes : Dict Int Box
    , score : Int
    , scoreLabels : List ScoreLabel
    , boxesLeft : Int
    }


initial : Model
initial =
    let
        size =
            Size 100 100
    in
        { time = 0
        , lastBoxSpawnTime = 0
        , seed = Random.initialSeed 0
        , size = size
        , boxSize = boxSize size
        , positionGenerator = createPositionGenerator size
        , scoreGenerator = Random.int 1 3
        , boxes = Dict.empty
        , score = 0
        , scoreLabels = []
        , boxesLeft = 3
        }


createPositionGenerator : Size -> Random.Generator ( Int, Int )
createPositionGenerator size =
    Random.pair (Random.int 0 size.width) (Random.int 0 size.height)


boxSize : Size -> Int
boxSize size =
    let
        scale =
            0.1
    in
        round (toFloat (max size.width size.height) * scale)
