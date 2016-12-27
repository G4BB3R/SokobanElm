module Subscriptions exposing (..)

import Msg      exposing (Msg(..))
import Model    exposing (Model, GameState(..))

import Keyboard exposing (..)
import Time
import Char

key_to_msg : KeyCode -> Msg
key_to_msg n =
    case n of
        32 -> -- Space bar
            Enter

        13 -> -- Enter
            Enter

        _ ->
            case Char.fromCode n of
                'M' ->
                    Menu

                'R' ->
                    RestartLevel

                'W' ->
                    Arrows (0, -1)

                'A' ->
                    Arrows (-1, 0)

                'S' ->
                    Arrows (0, 1)

                'D' ->
                    Arrows (1, 0)

                _ ->
                    Nop

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.millisecond Msg.Tick
        , Keyboard.downs key_to_msg
        ]
