module Update exposing (..)

import Msg         exposing (Msg(..))
import Model       exposing (..)
import Types       exposing (Block(..), Pos)
import Definitions exposing (..)

type WalkStatus
    = Won
    | Walked Pos (List Pos)
    | Fail

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
            { model
            | time = t
            , levels = -- fail animation when try to enter in an unlocked level
                mapWhere (.failStep >> (/=) 0) (\lvl ->
                    { lvl | failStep = -lvl.failStep + clamp -1 1 lvl.failStep })
                    model.levels
            } ! []
        
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

        Arrows offsetXY ->
            if model.state == GSMenu then
                { model
                | selected =
                    fixBetween
                        (model.selected + (arrowToIconId offsetXY))
                        (1, List.length model.levels)
                } ! []
            else
                case walk offsetXY model.current of
                    Won ->
                        { model | state = GSMenu, levels = updateLevels model } ! []

                    Walked pos boxes ->
                        { model
                        | current = let current = model.current in
                            { current
                            | player = pos
                            , boxes  = boxes
                            , steps  = model.current.steps + 1
                            }
                        } ! []

                    Fail ->
                        model ! []

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

moveBoxes : Pos -> Pos -> List Pos -> List Tile -> Maybe (List Pos)
moveBoxes player_pos (dx, dy) boxes tiles =
    let
        cannot_walk =
            tiles
            |> List.filter (.pos >> (==) player_pos)
            |> List.head
            |> Maybe.map (.block >> (/=) BFloor)
            |> Maybe.withDefault True

        boxes_ =
            boxes
            |> mapWhere ((==) player_pos) (\(bx, by) -> (bx + dx, by + dy))
            |> List.indexedMap (\i n -> (i + 1, n))

        collided_with_box  =
            flip List.any boxes_ (\(i, bpos) ->
                flip List.any boxes_ (\(i2, bpos2) -> i /= i2 && bpos == bpos2))

        collided_with_wall =
            flip List.any boxes_ (\(_, bpos) ->
                flip List.any tiles (\t -> t.pos == bpos && t.block == BWall))
    in
        if cannot_walk || collided_with_box || collided_with_wall then
            Nothing
        else
            Just <| List.map Tuple.second boxes_

walk : Pos -> Level -> WalkStatus
walk (x, y) level =
    let
        -- new position
        pos =
            (\(x_, y_) -> (x + x_, y + y_)) level.player

        -- `Maybe Pos` with the list of the new box positions, if not collided
        boxes_  =
            moveBoxes pos (x, y) level.boxes level.tiles

        -- Check if all the boxes are placed correctly
        won =
            boxes_
            |> Maybe.withDefault level.boxes
            |> List.all (\box -> List.any ((==) box) level.holes)
    in
        if won then
            Won
        else
            boxes_
            |> Maybe.map (Walked pos)
            |> Maybe.withDefault Fail

updateLevels : Model -> List LevelInfo
updateLevels model =
    model.levels
    |> mapWhere -- set best step and best time to the current level
        (.id >> (==) model.selected)
        (\lvl ->
            { lvl
            | beat     = True
            , bestStep = Basics.min lvl.bestStep (model.current.steps + 1)
            , bestTime =
                model.time - model.timeStart
                |> flip (/) 100 >> round >> toFloat >> flip (/) 10
                |> Basics.min lvl.bestTime })
    |> mapWhere -- unlock next level
        (.id >> (==) (model.selected + 1))
        (\lvl -> { lvl | unlocked = True })
