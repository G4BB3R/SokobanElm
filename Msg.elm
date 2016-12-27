module Msg exposing (..)

import Model       exposing (Model)
import Types       exposing (Pos)
import Time        exposing (Time)

type Msg
    = Nop
    | Arrows Pos
    | Enter
    | Menu
    | RestartLevel
    | Tick Time
    | PlayerAnimation