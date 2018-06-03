module Boxes exposing (..)

import Models exposing (Model, Box, BoxStatus(..))
import Random
import Dict


boxLifeTime : number
boxLifeTime =
    5000


boxHiddingStartTime : number
boxHiddingStartTime =
    4000


boxInitializingTime : number
boxInitializingTime =
    1000


updateBoxes : Model -> Model
updateBoxes model =
    model
        |> maybeSpawn
        |> checkBoxesForDestroy
        |> updateBoxStatuses


createBox : Float -> Random.Generator ( Int, Int ) -> Random.Generator Int -> Random.Seed -> ( Box, Random.Seed )
createBox time positionGenerator scoreGenerator seed0 =
    let
        ( ( x, y ), seed1 ) =
            Random.step positionGenerator seed0

        ( score, newSeed ) =
            Random.step scoreGenerator seed1
    in
        ( Box (floor time) BoxIsInitializing x y score time
        , newSeed
        )


maybeSpawn : Model -> Model
maybeSpawn model =
    if model.time - model.lastBoxSpawnTime > 1000 then
        let
            ( box, seed ) =
                createBox model.time model.positionGenerator model.scoreGenerator model.seed
        in
            { model
                | seed = seed
                , boxes = Dict.insert box.id box model.boxes
                , lastBoxSpawnTime = model.time
            }
    else
        model


checkBoxesForDestroy : Model -> Model
checkBoxesForDestroy model =
    { model | boxes = Dict.filter (boxIsAlive model.time) model.boxes }


boxIsAlive : Float -> Int -> Box -> Bool
boxIsAlive time id box =
    time - box.createTime < boxLifeTime


updateBoxStatuses : Model -> Model
updateBoxStatuses model =
    { model | boxes = Dict.map (updateBoxStatus model.time) model.boxes }


updateBoxStatus : Float -> Int -> Box -> Box
updateBoxStatus time id box =
    let
        lifetime =
            time - box.createTime
    in
        if lifetime > boxHiddingStartTime then
            { box | status = BoxIsHiding }
        else if lifetime > boxInitializingTime then
            { box | status = BoxIsVisible }
        else
            { box | status = BoxIsInitializing }
