module View  exposing (..)

import Time exposing (Time)

import Svg            exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events     exposing (..)

import Msg   exposing (..)
import Model exposing (..)

import Types       exposing (Block(..), Pos, blockToUrl)
import Definitions exposing (..)

render : (Int, Int) -> String -> Svg Msg
render (x_, y_) image_path =
    let
        (x__, y__) = getScreenPos (x_, y_)
    in
        if image_path == "" then
            svg [] []
        else
            image [ xlinkHref image_path, x <| toString x__, y <| toString y__
                , width "32px", height "32px" ] []



tile : Tile -> Svg Msg
tile tile =
    render tile.pos (blockToUrl tile.block)

hole : Pos -> Svg Msg
hole pos =
    render pos "/img/7.gif"

player : Bool -> Pos -> Svg Msg
player holed pos =
    render pos (if holed then "/img/6.gif" else "/img/5.gif")

box : Bool -> Pos -> Svg Msg
box correct pos =
    render pos (if correct then "/img/4.gif" else "/img/3.gif")

drawLevel : Time -> Level -> Svg Msg
drawLevel time_passed level =
    svg [ width "500", height "500", viewBox "0 0 500 500" ]
     <| [ text_ [ x "24", y "15", fill "black" ] [ text "Sokoban" ] ]
     ++ (List.map tile level.tiles)
     ++ (List.map hole level.holes)
     ++ (List.map (\bpos -> box (List.any ((==) bpos) level.holes) bpos) level.boxes)
     ++ [ player (List.any ((==) level.player) level.holes) level.player
        , text_ [ x "24", y "32", fill "black" ] [ text <| "Steps: " ++ toString level.steps ]
        , text_ [ x "104", y "32", fill "black" ]
                [ time_passed
                  |> flip (/) 10
                  |> round
                  |> toFloat
                  |> flip (/) 100
                  |> toString
                  |> (++) "Time: "
                  |> text
                ]
        ]

drawLevelIcon : Bool -> LevelInfo -> Svg Msg
drawLevelIcon selected levelInfo =
    let
        (x_, y_) = iconIdToPos levelInfo.id
        (sX, sY) = (x_ * 70, y_ * 70)
        color    = if levelInfo.unlocked then "yellow" else "gray"
    in
        svg []
            [ rect  [ x <| toString <| sX + levelInfo.failStep, y <| toString sY, width "48px"
                    , height "48px", fill color, stroke "black" ] []
            , text_ [ x <| toString <| sX + 16 + (if levelInfo.id < 10 then 4 else 0)
                    , y <| toString <| sY + 28, fill "black" ]
                    [ text <| toString <| levelInfo.id ]
            , text_ [ x <| toString <| sX - 4, y <| toString <| sY + 64
                    , fill "black", fontSize "12"
                    ,   if levelInfo.unlocked && levelInfo.bestStep > 0 then
                            visibility "show"
                        else
                            visibility "hidden"
                    ]
                    [ text <| "S " ++ (toString <| levelInfo.bestStep) ]
            , text_ [ x <| toString <| sX + 24, y <| toString <| sY + 64
                    , fill "black", fontSize "12"
                    ,   if levelInfo.unlocked && levelInfo.bestTime > 0 then
                            visibility "show"
                        else
                            visibility "hidden"
                    ]
                    [ text <| "T " ++ (toString <| levelInfo.bestTime)]
            , rect  [ x <| toString <| sX - 4, y <| toString <| sY - 4, width "56px"
                    , height "56px", stroke "blue", fill "none"
                    , visibility <| if selected then "show" else "hidden" ] []    
            ]

drawMenu : Model -> Svg Msg
drawMenu model =
    svg [ width "500", height "500" ] <|
        [ text_ [ x "50", y "50", fill "black" ] [ text "Choose a level" ] ]
     ++ (List.map (\level -> drawLevelIcon (model.selected == level.id) level) model.levels)

view : Model -> Svg Msg
view model =
    svg
        [ width "500", height "500", fill "blue" ]
        [ text_ [ x "24", y "15", fill "black" ]
                [ text "Sokoban - WASD to walk, R to restart and M to go back to menu" ]
        ,   if model.state == GSMenu then
                drawMenu model
            else
                drawLevel (model.time - model.timeStart) model.current
        ]
