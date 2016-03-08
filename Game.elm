module Game where

import Definitions  exposing (..)
import Level        exposing (strToLevel)

import Svg            exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events     exposing (..)

import Time exposing (Time)

type Action     = ActNop | ActArrows Pos | ActEnter Time | ActMenu | ActRestartLevel Time | ActTick Time
type GameState  = GSMenu | GSPlaying

type alias Model =
    { levels   : List LevelInfo
    , state    : GameState
    , current  : Level.Model
    , selected : Int
    }

type alias LevelInfo =
    { id       : Int
    , level    : Level.Model
    , unlocked : Bool
    , bestStep : Int
    , bestTime : Time
    , failStep : Int -- interface only
    }

initialModel =
    Model [ LevelInfo  1 level_01 True  0 0 0
          , LevelInfo  2 level_02 False 0 0 0
          , LevelInfo  3 level_03 False 0 0 0
          , LevelInfo  4 level_04 False 0 0 0
          , LevelInfo  5 level_05 False 0 0 0
          , LevelInfo  6 level_06 False 0 0 0
          , LevelInfo  7 level_07 False 0 0 0
          , LevelInfo  8 level_08 False 0 0 0
          , LevelInfo  9 level_09 False 0 0 0
          , LevelInfo 10 level_10 False 0 0 0
          , LevelInfo 11 level_11 False 0 0 0
          , LevelInfo 12 level_12 False 0 0 0
          , LevelInfo 13 level_13 False 0 0 0
          ]
          GSMenu level_01 1

drawLevelIcon : Bool -> LevelInfo -> Svg
drawLevelIcon selected levelInfo =
    let
        (x', y')   = iconIdToPos levelInfo.id
        (sX, sY) = (x' * 70, y' * 70)
        color    = if levelInfo.unlocked then "yellow" else "gray"
    in
        svg [] <|
            [ rect  [ x <| toString <| sX + levelInfo.failStep, y <| toString sY, width "48px", height "48px", fill color, stroke "black" ] []
            , text' [ x <| toString <| sX + 16 + (if levelInfo.id < 10 then 4 else 0), y <| toString <| sY + 28, fill "black" ]
                    [ text <| toString <| levelInfo.id ]
            , text' [ x <| toString <| sX - 4, y <| toString <| sY + 64, fill "black", fontSize "12"
                    , visibility <| if levelInfo.unlocked && levelInfo.bestStep > 0 then "show" else "hidden" ]
                    [ text <| "S " ++ (toString <| levelInfo.bestStep) ]
            , text' [ x <| toString <| sX + 24, y <| toString <| sY + 64, fill "black", fontSize "12"
                    , visibility <| if levelInfo.unlocked && levelInfo.bestTime > 0 then "show" else "hidden" ]
                    [ text <| "T " ++ (toString <| levelInfo.bestTime)]
            , rect  [ x <| toString <| sX - 4, y <| toString <| sY - 4, width "56px", height "56px", stroke "blue", fill "none"
                    , visibility <| if selected then "show" else "hidden" ] []    
            ]

drawMenu : Signal.Address Action -> Model -> Svg
drawMenu address model =
    svg [ width "500", height "500" ] <|
        [ text' [ x "50", y "50", fill "black" ] [ text "Choose a level" ] ]
     ++ (List.map (\level -> drawLevelIcon (model.selected == level.id) level) model.levels)

drawModel : Signal.Address Action -> Model -> Svg
drawModel address model =
    svg [ width "500", height "500", fill "blue" ]
     <| [ text' [ x "24", y "15", fill "black" ] [ text <| "Sokoban - WASD to walk, R to restart and M to go back to menu" ] ]
     ++ [ if model.state == GSMenu
          then drawMenu address model
          else Level.drawModel model.current ]

canMoveBoxes : Pos -> Pos -> List Pos -> List Tile -> Maybe (List Pos)
canMoveBoxes player_pos (dx, dy) boxes tiles =
    let
        boxes' = List.map (\(bx, by) ->
                     if (bx, by) == player_pos
                     then (bx + dx, by + dy)
                     else (bx, by))  boxes
        zip_boxes' = List.map2 (,) [1..List.length boxes'] boxes'
        colided_with_box  =
            flip List.any zip_boxes'
                (\(i, bpos) -> flip List.any zip_boxes'
                    (\(i2, bpos2) -> i /= i2 && bpos == bpos2))
        colided_with_wall =
            flip List.any boxes' (\bpos ->
                flip List.any tiles (\t -> t.pos == bpos && t.block == BWall))
    in
        if colided_with_box || colided_with_wall
        then Nothing
        else Just boxes'

update : Action -> Model -> Model
update action model =
    case action of
        ActNop   ->
            model
            
        ActMenu  ->
            { model | state = GSMenu }
        
        ActTick t ->
            let
                levels =
                    List.map (\lvl -> { lvl | failStep = if   lvl.failStep == 0
                                                         then lvl.failStep
                                                         else lvl.failStep * (-1) + (if lvl.failStep > 0 then 1 else -1) }) model.levels
            in
                { model | levels  = levels
                        , current = Level.update (Level.ActTick t) (model.current) }
        
        ActEnter t ->
            if model.state /= GSMenu then model
            else
            let
                m_levelInfo = model.levels
                           |> List.drop (model.selected - 1)
                           |> List.head
            in
                case m_levelInfo of
                    Nothing -> model
                    Just levelInfo ->
                        if levelInfo.unlocked
                        then { model | state   = GSPlaying
                                     , current = let c = levelInfo.level in { c | timeStart = t } }
                        else { model | levels  = mapWhere (\lvl -> lvl.id == model.selected)
                                                          (\lvl -> { lvl | failStep = 4 } )
                                                          model.levels }

        ActArrows pos ->
            if model.state == GSMenu then
                { model | selected = fixBetween (model.selected + (arrowToIconId pos)) (iconMin, List.length model.levels) }
            else
                let
                    current = Level.update (Level.ActWalk pos) model.current
                in
                    if not current.won
                    then { model | current = current }
                    else
                        let
                            levels = model.levels
                                  |> mapWhere (.id >> (==) model.selected)
                                              (\lvl -> { lvl | bestStep =
                                                                   Basics.min (if lvl.bestStep == 0 then 999999 else lvl.bestStep) current.steps
                                                             , bestTime =
                                                                   Basics.min (if lvl.bestTime == 0.0 then 999999.0 else lvl.bestTime)
                                                                              (current.timestamp - current.timeStart |> flip (/) 100 |> round |> toFloat |> flip (/) 10) })
                                  |> mapWhere (.id >> (==) (model.selected + 1))
                                              (\lvl -> { lvl | unlocked = True })
                        in
                            { model | levels = levels, state = GSMenu }
        
        ActRestartLevel t -> 
            let
                current = model.levels
                       |> List.filter (.id >> (==) model.selected)
                       |> List.head
                       |> Maybe.map .level
                       |> Maybe.withDefault level_error
            in
                { model | current = { current | timestamp = t, timeStart = t } }
        
level_error : Level.Model
level_error =
    strToLevel <| "...XXXXXXXXXX"
               ++ "...X   p    X"
               ++ "...XXXXXXXXXX"

level_01 : Level.Model      
level_01 =
    strToLevel <| "..XXX........"
               ++ "..XoX........"
               ++ "..XbXXXX....."
               ++ "XXX pboX....."
               ++ "Xob  XXX....."
               ++ "XXXXbX......."
               ++ "...XoX......."
               ++ "...XXX......."

level_02 : Level.Model      
level_02 =
    strToLevel <| "XXXXX........"
               ++ "Xp  X........"
               ++ "X bbX.XXX...."
               ++ "X b X.XoX...."
               ++ "XXX XXXoX...."
               ++ ".XX    oX...."
               ++ ".X   X  X...."
               ++ ".X   XXXX...."
               ++ ".XXXXX......."
               
level_03 : Level.Model      
level_03 =
    strToLevel <| ".XXXXXXX....."
               ++ ".X  p  XXX..."
               ++ "XXbXXX   X..."
               ++ "X   b  b X..."
               ++ "X ooX b XX..."
               ++ "XXooX   X...."
               ++ ".XXXXXXXX...."

level_04 : Level.Model      
level_04 =
    strToLevel <| ".XXXX........"
               ++ "XX  X........"
               ++ "Xpb X........"
               ++ "XXb XX......."
               ++ "XX b X......."
               ++ "Xob  X......."
               ++ "XooKoX......."
               ++ "XXXXXX......."

level_05 : Level.Model      
level_05 =
    strToLevel <| ".XXXXX......."
               ++ ".Xp XXX......"
               ++ ".X b  X......"
               ++ "XXX X XX....."
               ++ "XoX X  X....."
               ++ "Xob  X X....."
               ++ "Xo   b X....."
               ++ "XXXXXXXX....."
      
level_06 = strToLevel <| "...XXXXXXX..."
                      ++ "XXXX     X..."
                      ++ "X   oXXX X..."
                      ++ "X X X    XX.."
                      ++ "X X B BXo X.."
                      ++ "X X  K  X X.."
                      ++ "X oXB B X X.."
                      ++ "XX    X X XXX"
                      ++ ".X XXXo    PX"
                      ++ ".X     XX   X"
                      ++ ".XXXXXXXXXXXX"

level_07 = strToLevel <| "...XXXXXXX..."
                      ++ "..XX  X PX..."
                      ++ "..X   X  X..."
                      ++ "..XB B B X..."
                      ++ "..X BXX  X..."
                      ++ "XXX B X XX..."
                      ++ "XOOOOO  X...."
                      ++ "XXXXXXXXX...."

level_08 = strToLevel <| "...XXXXXX...."
                      ++ ".XXX    X...."
                      ++ "XXO BXX XX..."
                      ++ "XOOB B  PX..."
                      ++ "XOO B B XX..."
                      ++ "XXXXXX  X...."
                      ++ ".....XXXX...."
                      
level_09 = strToLevel <| ".XXXXXXXXX..."
                      ++ ".X  XX   X..."
                      ++ ".X   B   X..."
                      ++ ".XB XXX BX..."
                      ++ ".X XOOOX X..."
                      ++ "XX XOOOX Xx.."
                      ++ "X B  B  B X.."
                      ++ "X     X P X.."
                      ++ "XXXXXXXXXXX.."

level_10 = strToLevel <| "..XXXXXX....."
                      ++ "..X    X....."
                      ++ "XXXBBB X....."
                      ++ "XP BOO X....."
                      ++ "X BOOOXX....."
                      ++ "XXXX  X......"
                      ++ "...XXXX......"

level_11 = strToLevel <| ".XXXX..XXXXX."
                      ++ "XX  X..X   X."
                      ++ "X B XXXXB  X."
                      ++ "X  BOOOO B X."
                      ++ "XX    X P XX."
                      ++ ".XXXXXXXXXX.."

level_12 = strToLevel <| "..XXXXX......"
                      ++ "XXX  PX......"
                      ++ "X  Bo XX....."
                      ++ "X  oBo X....."
                      ++ "XXX KB X....."
                      ++ "..X   XX....."
                      ++ "..XXXXX......"

level_13 = strToLevel <| "..XXXX......."
                      ++ "..XooX......."
                      ++ ".XX oXX......"
                      ++ ".X  boX......"
                      ++ "XX b  XX....."
                      ++ "X  Xbb X....."
                      ++ "X  p   X....."
                      ++ "XXXXXXXX....."
