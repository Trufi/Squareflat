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
    , status: BoxStatus
    , x : Int
    , y : Int
    , score : Int
    , createTime : Float
    }


type alias Model =
    { time : Float
    , lastBoxSpawnTime : Float
    , seed : Random.Seed
    , size : Size
    , positionGenerator : Random.Generator ( Int, Int )
    , scoreGenerator: Random.Generator Int
    , boxes : Dict Int Box
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
        , positionGenerator = createPositionGenerator size
        , scoreGenerator = Random.int 1 3
        , boxes = Dict.empty
        }


createPositionGenerator : Size -> Random.Generator ( Int, Int )
createPositionGenerator size =
    Random.pair (Random.int 0 size.width) (Random.int 0 size.height)
