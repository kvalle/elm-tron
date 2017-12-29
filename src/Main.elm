module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Collage exposing (defaultLine)
import Element
import Color
import Time exposing (Time)
import Keyboard
import List.Nonempty exposing (Nonempty, (:::))


type alias Model =
    { red : Player
    , blue : Player
    , state : GameState
    }


type alias Player =
    { path : Path
    , direction : Direction
    }


type Color
    = Red
    | Blue


type GameState
    = NotStarted
    | Running
    | GameOver


width : number
width =
    50


height : number
height =
    40


type alias Path =
    Nonempty Pos


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Pos =
    ( Float, Float )


type Msg
    = NoOp
    | Move
    | Go
    | ChangeDirection Color Direction
    | NewGame


init : ( Model, Cmd Msg )
init =
    ( { red =
            { path = List.Nonempty.fromElement ( -3, 0 )
            , direction = Up
            }
      , blue =
            { path = List.Nonempty.fromElement ( 3, 0 )
            , direction = Up
            }
      , state = NotStarted
      }
    , Cmd.none
    )


setPlayer : Color -> Player -> Model -> Model
setPlayer color player model =
    case color of
        Red ->
            { model | red = player }

        Blue ->
            { model | blue = player }


getPlayer : Color -> Model -> Player
getPlayer color =
    case color of
        Red ->
            .red

        Blue ->
            .blue


move : Player -> Player
move player =
    let
        ( x, y ) =
            List.Nonempty.head player.path
    in
        { player
            | path =
                case player.direction of
                    Up ->
                        ( x, y + 1 ) ::: player.path

                    Down ->
                        ( x, y - 1 ) ::: player.path

                    Left ->
                        ( x - 1, y ) ::: player.path

                    Right ->
                        ( x + 1, y ) ::: player.path
        }


isOutsideBoard : Pos -> Bool
isOutsideBoard (( x, y ) as pos) =
    (abs x > (width / 2)) || (abs y > (height / 2))


checkPosition : Model -> Model
checkPosition model =
    let
        redHead =
            List.Nonempty.head <| model.red.path

        redTail =
            List.Nonempty.tail <| model.red.path

        blueHead =
            List.Nonempty.head <| model.blue.path

        blueTail =
            List.Nonempty.tail <| model.blue.path
    in
        { model
            | state =
                if isOutsideBoard redHead then
                    GameOver
                else if isOutsideBoard blueHead then
                    GameOver
                else if List.member redHead (redTail ++ blueTail) then
                    GameOver
                else if List.member blueHead (redTail ++ blueTail) then
                    GameOver
                else
                    Running
        }


setDirection : Direction -> Player -> Player
setDirection direction player =
    let
        impossibleChange from to =
            List.member ( from, to )
                [ ( Up, Down ), ( Down, Up ), ( Left, Right ), ( Right, Left ) ]
    in
        if impossibleChange direction player.direction then
            player
        else
            { player | direction = direction }


setState : GameState -> Model -> Model
setState gameState model =
    { model | state = gameState }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Move ->
            ( case model.state of
                Running ->
                    model
                        |> setPlayer Red (move model.red)
                        |> setPlayer Blue (move model.blue)
                        |> checkPosition

                _ ->
                    model
            , Cmd.none
            )

        ChangeDirection color direction ->
            ( model |> setPlayer color (setDirection direction <| getPlayer color model)
            , Cmd.none
            )

        Go ->
            ( model |> setState Running
            , Cmd.none
            )

        NewGame ->
            init


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running ->
            Sub.batch
                [ Time.every (Time.millisecond * 100) (always Move)
                , Keyboard.downs gameControlKeys
                ]

        NotStarted ->
            Keyboard.downs gameControlKeys

        _ ->
            Sub.none


gameControlKeys : Keyboard.KeyCode -> Msg
gameControlKeys keyCode =
    case keyCode of
        32 ->
            -- Space
            Go

        37 ->
            -- Arrow left
            ChangeDirection Blue Left

        38 ->
            -- Arrow up
            ChangeDirection Blue Up

        39 ->
            -- Arrow right
            ChangeDirection Blue Right

        40 ->
            -- Arrow down
            ChangeDirection Blue Down

        65 ->
            -- A key
            ChangeDirection Red Left

        87 ->
            -- W key
            ChangeDirection Red Up

        68 ->
            -- D key
            ChangeDirection Red Right

        83 ->
            -- S key
            ChangeDirection Red Down

        _ ->
            NoOp


view : Model -> Html Msg
view model =
    let
        pixelSize =
            10
    in
        div [ class "game" ]
            [ div [ class "board" ]
                [ let
                    path player color =
                        if model.state == NotStarted then
                            Collage.square pixelSize
                                |> Collage.filled color
                                |> Collage.move
                                    (List.Nonempty.head player.path
                                        |> Tuple.mapFirst ((*) pixelSize)
                                        |> Tuple.mapSecond ((*) pixelSize)
                                    )
                        else
                            player.path
                                |> List.Nonempty.toList
                                |> List.map (Tuple.mapFirst <| (*) pixelSize)
                                |> List.map (Tuple.mapSecond <| (*) pixelSize)
                                |> Collage.path
                                |> Collage.traced
                                    { defaultLine
                                        | width = pixelSize
                                        , cap = Collage.Padded
                                        , color = color
                                    }

                    players =
                        [ path model.red Color.red
                        , path model.blue Color.blue
                        ]
                  in
                    players
                        |> Collage.collage (width * pixelSize + pixelSize // 2) (height * pixelSize + pixelSize // 2)
                        |> Element.toHtml
                ]
            , div [ class "info" ]
                [ div [ class "title" ] [ text "TRON" ]
                , div [ class "new-game" ]
                    [ button
                        [ onClick NewGame
                        , disabled <| model.state /= GameOver
                        ]
                        [ text "new game" ]
                    ]
                , div [ class "status" ]
                    [ text <|
                        case model.state of
                            NotStarted ->
                                "press SPACE to begin"

                            Running ->
                                "control with ASDF and arrow keys"

                            GameOver ->
                                "game over"
                    ]
                ]
            ]
