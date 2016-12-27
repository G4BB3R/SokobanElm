module Model exposing (..)

import Time exposing (Time)

import Levels      exposing (level_01_str, level_list_str, level_error_str)
import Types       exposing (Pos, Block(..))
import Definitions exposing (getPosByIndex)

type alias Tile =
    { pos   : Pos
    , block : Block
    }

type GameState
    = GSMenu
    | GSPlaying

type alias LevelInfo =
    { id       : Int
    , level    : Level
    , unlocked : Bool
    , bestStep : Int
    , bestTime : Time
    , failStep : Int -- interface only
    }

type alias Model =
    { levels    : List LevelInfo
    , current   : Level
    , state     : GameState
    , selected  : Int
    , time      : Time
    , timeStart : Time
    }

type alias Level =
    { tiles     : List Tile
    , holes     : List Pos
    , boxes     : List Pos
    , player    : Pos
    , won       : Bool
    , steps     : Int
    }

init : Model
init =
    Model
        levels
        level_01
        GSMenu
        1
        0.0
        0.0

level_01 : Level
level_01 =
    strToLevel level_01_str

level_error : Level
level_error =
    strToLevel level_error_str

levels : List LevelInfo
levels =
    List.indexedMap
            (\i level_str ->
                LevelInfo (i + 1) (strToLevel level_str) (i == 0) 0 0 0)
            level_list_str


strToLevel : String -> Level
strToLevel str =
    let 
        xs : List (Int, Char)
        xs =
            str
            |> String.toLower
            |> String.toList
            |> List.indexedMap (\i a -> (i + 1, a))

        tiles =
            flip List.map xs (\(i, c) ->
                    Tile
                        (getPosByIndex i)
                        (if c == 'x' then
                            BWall
                        else if c == '.' then
                            BVoid
                        else BFloor))

        holes = xs
             |> List.filter (\(i, c) -> c == 'o' || c == 'k' )
             |> List.map (Tuple.first >> getPosByIndex)             

        boxes = xs
             |> List.filter (\(i, c) -> c == 'b' || c == 'k' )
             |> List.map (Tuple.first >> getPosByIndex)

        player = xs
              |> List.filter (\(i, c) -> c == 'p' )
              |> List.map (Tuple.first >> getPosByIndex)
              |> List.head
              |> Maybe.withDefault (5, 5)
    in
        Level tiles holes boxes player False 0
