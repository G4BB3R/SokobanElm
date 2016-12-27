module Definitions exposing (..)

import Types exposing (..)

(tileW, tileH) = (32, 32)
(mapW, mapH)   = (13, 13)
(iconW, iconMin, iconMax)  = (3, 1, 12)

getScreenPos : (Int, Int) -> (Int, Int)
getScreenPos (x, y) =
    ( 24 + (x - 1) * tileW
    , 38 + (y - 1) * tileH
    )

getPosByIndex : Int -> Pos
getPosByIndex i =
    ((i - 1) % mapW + 1, (i - 1) // mapH + 1)

iconIdToPos : Int -> Pos
iconIdToPos id =
    ((id - 1) % iconW + 1, (id - 1) // iconW + 1)

iconPosToId (x, y) =
    y * iconW + y

arrowToIconId : Pos -> Int
arrowToIconId pos =
    case pos of
        (-1, 0) -> -1
        (1, 0)  -> 1
        (0, -1) -> -iconW
        (0, 1)  -> iconW
        _       -> 0

fixBetween : Int -> (Int, Int) -> Int
fixBetween x (min, max) =
    if x < min then
        max - (x - min - 1) % iconW
    else if x > max then
        min + (x - max - 1) % iconW
    else
        x

mapWhere : (a -> Bool) -> (a -> a) -> List a -> List a
mapWhere filter map xs =
   List.map (\x -> if filter x then map x else x) xs

iff : a -> a -> Bool -> a
iff ifThen ifElse active =
    if active then
        ifThen
    else
        ifElse
