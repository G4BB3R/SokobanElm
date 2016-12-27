module Types exposing (..)

type alias Pos =
    (Int, Int)

type Block
    = BFloor
    | BWall
    | BVoid
