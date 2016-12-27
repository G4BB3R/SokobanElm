module Update exposing (..)

import Msg         exposing (Msg(..))
import Model       exposing (..)
import Types       exposing (Block(..), Pos)
import Definitions exposing (..)

isWalkable : Pos -> Level -> Bool
isWalkable pos level =
    let
        tile = level.tiles
            |> List.filter (.pos >> (==) pos)
            |> List.head
    in
        tile
        |> Maybe.map (.block >> (==) BFloor)
        |> Maybe.withDefault False

canMoveBoxes : Pos -> Pos -> List Pos -> List Tile -> Maybe (List Pos)
canMoveBoxes player_pos (dx, dy) boxes tiles =
    let
        boxes_ =
            flip List.map boxes (\(bx, by) ->
                if (bx, by) == player_pos then
                    (bx + dx, by + dy)
                else
                    (bx, by))

        zip_boxes_ =
            List.indexedMap (,) boxes_
            |> List.map (\(i, n) -> (i + 1, n))

        colided_with_box  =
            flip List.any zip_boxes_
                (\(i, bpos) -> flip List.any zip_boxes_
                    (\(i2, bpos2) -> i /= i2 && bpos == bpos2))

        colided_with_wall =
            flip List.any boxes_ (\bpos ->
                flip List.any tiles (\t -> t.pos == bpos && t.block == BWall))
    in
        if colided_with_box || colided_with_wall then
            Nothing
        else
            Just boxes_

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Nop   ->
            model ! []
        
        PlayerAnimation ->
            { model | player_animation = not model.player_animation } ! []

        Menu  ->
            { model | state = GSMenu } ! []
        
        Tick t ->
            let
                levels =
                    flip List.map model.levels (\lvl ->
                        { lvl | failStep =
                            if lvl.failStep == 0 then
                                lvl.failStep
                            else
                                lvl.failStep * (-1) + (if lvl.failStep > 0 then 1 else -1) })
            in
                { model | levels  = levels, time = t } ! []
        
        Enter ->
            if model.state /= GSMenu then
                model ! []
            else
                let
                    m_levelInfo =
                        model.levels
                        |> List.drop (model.selected - 1)
                        |> List.head
                in
                    case m_levelInfo of
                        Nothing ->
                            model ! []

                        Just levelInfo ->
                            if levelInfo.unlocked then
                                { model
                                | state     = GSPlaying
                                , timeStart = model.time
                                , current   = levelInfo.level
                                } ! []
                            else
                                { model | levels =
                                    mapWhere (\lvl -> lvl.id == model.selected)
                                             (\lvl -> { lvl | failStep = 4 })
                                             model.levels
                                } ! []

        Arrows (x, y) ->
            if model.state == GSMenu then
                { model
                | selected =
                    fixBetween
                        (model.selected + (arrowToIconId (x, y)))
                        (iconMin, List.length model.levels)
                } ! []
            else
                updateWalk (x, y) model ! []

        RestartLevel ->
            let
                current =
                    model.levels
                    |> List.filter (.id >> (==) model.selected)
                    |> List.head
                    |> Maybe.map .level
                    |> Maybe.withDefault level_error
            in
                { model
                | time      = model.time
                , timeStart = model.time
                , current   = current
                } ! []

updateWalk : (Int, Int) -> Model -> Model
updateWalk (x, y) model =
    let
        current =
            if model.current.won then
                model.current
            else
                let
                    pos     = model.current.player
                              |> Tuple.mapFirst ((+)x)
                              |> Tuple.mapSecond ((+)y)
                    boxes_  = canMoveBoxes pos (x, y)
                                model.current.boxes model.current.tiles
                    success = abs x + (abs y) == 1
                           && isWalkable pos model.current
                           && boxes_ /= Nothing
                    won     = success
                           && flip List.all
                                (Maybe.withDefault model.current.boxes boxes_)
                                (\bpos -> List.any ((==) bpos) model.current.holes)
                in
                    if not success then
                        model.current
                    else let current = model.current in
                        { current
                        | player = pos
                        , boxes  = Maybe.withDefault model.current.boxes boxes_
                        , won    = won
                        , steps  = model.current.steps + 1
                        }
    in
        if not current.won then
            { model | current = current }
        else
            { model | levels = updateLevels model, state = GSMenu }

updateLevels : Model -> List LevelInfo
updateLevels model =
    model.levels
    |> mapWhere
        (.id >> (==) model.selected)
        (\lvl ->
            { lvl
            | bestStep =
                let 
                    best_step = 
                        if lvl.bestStep == 0 then
                            999999
                        else
                            lvl.bestStep
                in
                    Basics.min best_step model.current.steps
                    
            , bestTime =
                let
                    best_time =
                        if lvl.bestTime == 0.0 then
                            999999.0
                        else
                            lvl.bestTime
                in
                    model.time - model.timeStart
                    |> flip (/) 100
                    |> round
                    |> toFloat
                    |> flip (/) 10
                    |>  Basics.min best_time
            })
    |> mapWhere
        (.id >> (==) (model.selected + 1))
        (\lvl -> { lvl | unlocked = True })

