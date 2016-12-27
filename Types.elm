module Types exposing (..)

type alias Pos =
    (Int, Int)

type Block
    = BFloor
    | BWall
    | BVoid

blockToUrl : Block -> String
blockToUrl block =
    case block of
        BFloor ->
            "/img/2.gif"

        BWall ->
            "/img/1.gif"

        BVoid ->
            ""
