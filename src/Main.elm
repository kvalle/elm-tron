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
    { player1 : Player
    , player2 : Player
    , state : GameState
    }


type alias Player =
    { path : Path
    , direction : Direction
    }


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
    = Tick Time.Time
    | KeyPress Keyboard.KeyCode
    | NewGame


init : ( Model, Cmd Msg )
init =
    ( { player1 =
            { path = List.Nonempty.fromElement ( -3, 0 )
            , direction = Up
            }
      , player2 =
            { path = List.Nonempty.fromElement ( 3, 0 )
            , direction = Up
            }
      , state = NotStarted
      }
    , Cmd.none
    )


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
        player1Head =
            List.Nonempty.head model.player1.path

        player1Tail =
            List.Nonempty.tail model.player1.path

        player2Head =
            List.Nonempty.head model.player2.path

        player2Tail =
            List.Nonempty.tail model.player2.path
    in
        { model
            | state =
                if isOutsideBoard player1Head then
                    GameOver
                else if isOutsideBoard player2Head then
                    GameOver
                else if List.member player1Head (player1Tail ++ player2Tail) then
                    GameOver
                else if List.member player2Head (player1Tail ++ player2Tail) then
                    GameOver
                else
                    Running
        }


changeDirectionPlayer1 : Keyboard.KeyCode -> Player -> Player
changeDirectionPlayer1 keyCode player =
    { player
        | direction =
            case keyCode of
                37 ->
                    -- Arrow left
                    Left

                38 ->
                    -- Arrow up
                    Up

                39 ->
                    -- Arrow right
                    Right

                40 ->
                    -- Arrow down
                    Down

                _ ->
                    player.direction
    }


changeDirectionPlayer2 : Keyboard.KeyCode -> Player -> Player
changeDirectionPlayer2 keyCode player =
    { player
        | direction =
            case keyCode of
                65 ->
                    -- A key
                    Left

                87 ->
                    -- W key
                    Up

                68 ->
                    -- D key
                    Right

                83 ->
                    -- S key
                    Down

                _ ->
                    player.direction
    }


setState : GameState -> Model -> Model
setState gameState model =
    { model | state = gameState }


setPlayer1 : Player -> Model -> Model
setPlayer1 player model =
    { model | player1 = player }


setPlayer2 : Player -> Model -> Model
setPlayer2 player model =
    { model | player2 = player }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( case model.state of
                Running ->
                    model
                        |> setPlayer1 (move model.player1)
                        |> setPlayer2 (move model.player2)
                        |> checkPosition

                _ ->
                    model
            , Cmd.none
            )

        KeyPress keyCode ->
            ( model
                |> setState Running
                |> setPlayer1 (changeDirectionPlayer1 keyCode model.player1)
                |> setPlayer2 (changeDirectionPlayer2 keyCode model.player2)
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
                [ Time.every (Time.millisecond * 100) Tick
                , Keyboard.downs KeyPress
                ]

        NotStarted ->
            Keyboard.downs KeyPress

        _ ->
            Sub.none


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
                        [ path model.player1 Color.red
                        , path model.player2 Color.blue
                        ]
                  in
                    players
                        |> Collage.collage (width * pixelSize + pixelSize // 2) (height * pixelSize + pixelSize // 2)
                        |> Element.toHtml
                ]
            , div [ class "info" ]
                [ div [ class "title" ] [ text "TRON" ]
                , div [ class "status" ]
                    [ text <|
                        case model.state of
                            NotStarted ->
                                "press SPACE to begin"

                            Running ->
                                ""

                            GameOver ->
                                "game over"
                    ]
                , if model.state == GameOver then
                    button [ class "new-game", onClick NewGame ] [ text "new game" ]
                  else
                    text ""
                ]
            ]
