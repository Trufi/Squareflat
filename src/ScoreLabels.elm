module ScoreLabels exposing (..)

import Models exposing (Model, Box, ScoreLabel)


addScoreLabel : Box -> Model -> Model
addScoreLabel box model =
    let
        scoreLabel =
            ScoreLabel box.id model.time box.x box.y box.score
    in
        { model
            | scoreLabels = scoreLabel :: model.scoreLabels
        }


updateScoreLabels : Model -> Model
updateScoreLabels model =
    { model
        | scoreLabels = List.filter (scoreIsAlive model.time) model.scoreLabels
    }


scoreIsAlive : Float -> ScoreLabel -> Bool
scoreIsAlive time scoreLabel =
    time - scoreLabel.createTime < 500
